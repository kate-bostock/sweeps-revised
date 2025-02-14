#!/bin/bash

# Define the source and destination directories
SOURCE_DIR="simulations"
DEST_DIR="extracted_files2"

# Create the destination directory if it doesn't exist
mkdir -p "$DEST_DIR"

# Loop through each folder in the simulations directory
for folder in "$SOURCE_DIR"/*; do
    # Get the folder name (e.g., 0000000A, 00000001)
    folder_name=$(basename "$folder")
    
    # Create a corresponding folder in the destination directory
    mkdir -p "$DEST_DIR/$folder_name"
    
    # Copy the desired files to the destination folder
    cp "$folder/output_driver_genotype_properties.dat" "$DEST_DIR/$folder_name/"
    cp "$folder/parameters.dat" "$DEST_DIR/$folder_name/"
    cp "$folder/output.dat" "$DEST_DIR/$folder_name/"
done

echo "Extraction complete. Files are stored in the '$DEST_DIR' directory."
