# ==============================================================================
# Pipeline: Multi-Collector Data Cleaning, Spatial Merge, and Lab Integration
# ==============================================================================

library(sf)
library(dplyr)
library(purrr)
library(readr)
library(stringr)

# --- 1. Directory Setup ---
raw_dir  <- "data/raw_field_data"
data_dir <- "data"

if (!dir.exists(raw_dir)) dir.create(raw_dir, recursive = TRUE)

# --- 2. Load Incoming GPKG Files from Workers ---
incoming_files <- list.files(raw_dir, pattern = "\\.gpkg$", full.names = TRUE)

if (length(incoming_files) == 0) {
  stop("❌ No .gpkg files found in 'data/raw_field_data/'. Download worker files first.")
}

message("📦 Found ", length(incoming_files), " field file(s). Merging...")

field_points <- incoming_files %>%
  map_dfr(function(file_path) {
    
    # Get layer names inside the GeoPackage
    layers <- st_layers(file_path)
    
    # Find spatial point layers (skips QField system tables)
    geom_layers <- layers$name[layers$geomtype != "NA" & !is.na(layers$geomtype)]
    
    if (length(geom_layers) == 0) {
      warning("⚠️ File ", basename(file_path), " has no spatial layers. Skipping.")
      return(NULL)
    }
    
    # Read the spatial layer
    layer <- st_read(file_path, layer = geom_layers[1], quiet = TRUE)
    
    # -------------------------------------------------------------
    # Case-Insensitive Field Mapping
    # -------------------------------------------------------------
    # Convert all column names to lowercase to avoid case-sensitivity bugs
    names(layer) <- tolower(names(layer))
    
    # Map Sample_ID from 'title', 'name', or 'sample_id'
    if ("title" %in% names(layer)) {
      layer <- layer %>% mutate(Sample_ID = title)
    } else if ("name" %in% names(layer)) {
      layer <- layer %>% mutate(Sample_ID = name)
    } else if ("sample_id" %in% names(layer)) {
      layer <- layer %>% mutate(Sample_ID = sample_id)
    } else {
      warning("⚠️ File ", basename(file_path), " lacks 'title', 'name', or 'sample_id' fields. Skipping.")
      return(NULL)
    }
    
    # Map Barangay from 'note', 'description', or 'barangay'
    if ("note" %in% names(layer)) {
      layer <- layer %>% mutate(Barangay = note)
    } else if ("description" %in% names(layer)) {
      layer <- layer %>% mutate(Barangay = description)
    } else if ("barangay" %in% names(layer)) {
      layer <- layer %>% mutate(Barangay = barangay)
    } else {
      layer <- layer %>% mutate(Barangay = NA_character_)
    }
    
    # Select standardized columns (retains spatial geometry)
    layer <- layer %>% select(Sample_ID, Barangay)
    
    return(layer)
  })

# Verify points were loaded
if (is.null(field_points) || nrow(field_points) == 0) {
  stop("❌ No valid spatial points were loaded. Please check your GPKG field layer names.")
}

# Transform coordinates to WGS 84
field_points <- st_transform(field_points, crs = 4326)

# --- 3. Clean Text & Build Location Strings ---
field_points <- field_points %>%
  filter(!is.na(Sample_ID) & Sample_ID != "") %>%
  mutate(
    # Clean Sample ID
    Sample_ID = toupper(trimws(as.character(Sample_ID))),
    
    # Clean Barangay Name (Auto-Capitalize proper words)
    Barangay = str_to_title(trimws(as.character(Barangay))),
    Barangay = ifelse(is.na(Barangay) | Barangay == "", "Unspecified Barangay", Barangay),
    
    # Readable display string for map popups
    Full_Location = paste0("Brgy. ", Barangay)
  )

# --- 4. Check for Duplicate Sample IDs across collectors ---
duplicates <- field_points %>%
  st_drop_geometry() %>%
  filter(duplicated(Sample_ID)) %>%
  pull(Sample_ID)

if (length(duplicates) > 0) {
  warning("⚠️ DUPLICATE FIELD SAMPLE IDs DETECTED: ", paste(unique(duplicates), collapse = ", "))
} else {
  message("✅ All field Sample IDs across collectors are unique!")
}

# --- 5. Merge with Lab Results CSV ---
lab_file <- file.path(data_dir, "lab_results.csv")

if (file.exists(lab_file)) {
  message("🧪 Processing lab results from ", lab_file, "...")
  
  lab_data <- read_csv(lab_file, show_col_types = FALSE)
  
  if ("Sample_ID" %in% names(lab_data)) {
    lab_data$Sample_ID <- toupper(trimws(as.character(lab_data$Sample_ID)))
    
    # Audit unmatched IDs
    unmatched_lab <- setdiff(lab_data$Sample_ID, field_points$Sample_ID)
    if (length(unmatched_lab) > 0) {
      warning("⚠️ Lab Sample IDs without GPS coordinates: ", paste(unmatched_lab, collapse = ", "))
    }
    
    # Left Join lab values to spatial points
    master_data <- field_points %>%
      left_join(lab_data, by = "Sample_ID")
    
  } else {
    warning("⚠️ 'lab_results.csv' missing 'Sample_ID' column. Joining skipped.")
    master_data <- field_points
  }
} else {
  message("ℹ️ No 'lab_results.csv' found. Proceeding with spatial points only.")
  master_data <- field_points
}

# --- 6. Export Master GeoPackage for Quarto Site ---
output_path <- file.path(data_dir, "Soil_Master_Data.gpkg")
st_write(master_data, output_path, delete_dsn = TRUE, quiet = TRUE)

message("🎉 SUCCESS: Master dataset saved to ", output_path)
message("📊 Total valid locations logged: ", nrow(master_data))