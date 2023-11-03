# Creating folders
dir.create("../DATA")
dir.create("../DATA/RAW",recursive = T)

# Data
FAO_AS <- "https://fenix.fao.org/faostat/static/bulkdownload/zip_files/FAOSTAT_A-S_E.zip" 
FAO_TZ <- "https://fenix.fao.org/faostat/static/bulkdownload/zip_files/FAOSTAT_T-Z_E.zip" 

# Define the path for the downloaded ZIP file
download_path <- "../DATA/RAW/"

# Download the ZIP file from the URL to the specified path
download.file(FAO_AS, destfile = paste0(download_path,"FAOSTAT_AS.zip"), mode = "wb", method = "curl")
download.file(FAO_TZ, destfile = paste0(download_path,"FAOSTAT_TZ.zip"), mode = "wb", method = "curl")
