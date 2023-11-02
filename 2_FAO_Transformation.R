DATA_RAW <- "DATA/RAW/" 
DATA_EXTRACTED <- "DATA/EXTRACTED/"

file <- paste0(DATA_RAW,list.files(path = DATA_RAW, pattern = "zip"))
dir.create(DATA_EXTRACTED,recursive = T)

# Extract the contents of the zipfile to a temporary directory
tempdir <- tempdir()
nav <- lapply(file, unzip, exdir = tempdir)

# just selecting agricultural data
unziplist <- file.path((tempdir),dir(tempdir)[grepl("Production_Crops",((dir(tempdir))))])

# Unzip the file to the specified directory
lapply(unziplist,unzip, exdir = DATA_EXTRACTED)

# deleting directories ----------------------------------------------------
unlink("DATA/RAW/", recursive = TRUE)