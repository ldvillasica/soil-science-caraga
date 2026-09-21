# ==============================================================================
# Pipeline: Data Cleaning, Spatial Merge, and Lab Data Integration
# ==============================================================================

library(sf)
library(dplyr)
library(purrr)
library(readr)

# --- 1. Set Up Directory Paths ---
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
    layer <- st_read(file_path, quiet = TRUE)
    
    # Identify which field holds the Sample ID string
    if ("Note" %in% names(layer)) {
      layer <- layer %>% mutate(Sample_ID = Note)
    } else if (!"Sample_ID" %in% names(layer)) {
      warning("⚠️ File ", basename(file_path), " lacks 'Note' or 'Sample_ID' fields. Skipping.")
      return(NULL)
    }
    
    # Select ONLY Sample_ID; sf automatically keeps the spatial geometry intact!
    layer <- layer %>% select(Sample_ID)
    
    return(layer)
  }) %>%
  filter(!is.na(Sample_ID) & Sample_ID != "") %>%
  st_transform(crs = 4326) # Standard WGS 84 coordinate system

# Clean strings (Convert to Uppercase & Trim extra white spaces)
field_points$Sample_ID <- toupper(trimws(field_points$Sample_ID))

# Check for Duplicate GPS Sample IDs
duplicates <- field_points %>%
  st_drop_geometry() %>%
  filter(duplicated(Sample_ID)) %>%
  pull(Sample_ID)

if (length(duplicates) > 0) {
  warning("⚠️ DUPLICATE FIELD SAMPLE IDs DETECTED: ", paste(unique(duplicates), collapse = ", "))
} else {
  message("✅ All field Sample IDs are unique!")
}

# --- 3. Merge with Lab Results CSV ---
lab_file <- file.path(data_dir, "lab_results.csv")

if (file.exists(lab_file)) {
  message("🧪 Processing lab results from ", lab_file, "...")
  
  lab_data <- read_csv(lab_file, show_col_types = FALSE)
  
  if ("Sample_ID" %in% names(lab_data)) {
    lab_data$Sample_ID <- toupper(trimws(lab_data$Sample_ID))
    
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

# --- 4. Export Master GeoPackage for Quarto Site ---
output_path <- file.path(data_dir, "Soil_Master_Data.gpkg")
st_write(master_data, output_path, delete_dsn = TRUE, quiet = TRUE)

message("🎉 SUCCESS: Master dataset saved to ", output_path)
message("📊 Total valid locations logged: ", nrow(master_data))