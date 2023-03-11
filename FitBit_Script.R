# Importing libraries and data sets ---------------------------------------

# Load libraries
library(ggplot2)
library(readr)
library(stringr)

# Import data sets
# Create a function to import all the data sets within the folder as a list
importDataList <- function(folder_path = "data") {
        
        # Get a list off all files names in the given folder
        file_names <- list.files(path = folder_path)
        
        # Create empty vectors to store data names and data lists
        data_name <- c()
        data_list <- list()
        
        # Loop through each file in the list of file names
        for(i in seq_along(file_names)) {
                
                # Find the position of the first underscore in the file 
                char_pos <- sapply(lapply(strsplit(file_names, split = ""), function(x) {which(x == "_")}), function(x) {x[1]})
                
                # Extract the data name from the file name 
                data_name[i] <- substr(file_names[i], start = 1, stop = char_pos[i] - 1)
                
                # Read in the CSV file and store it in the data list
                file_path <- paste0(folder_path, "/", file_names[i])
                data_list[[i]] <- read_csv(file_path)
        }
        
        # Rename the data sets in the list and return the list
        names(data_list) <- data_name
        return(data_list)
}
data_list <- importDataList()





