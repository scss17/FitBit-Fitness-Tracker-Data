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
bellaFit <- importDataList()

# EDA ---------------------------------------------------------------------

# Define a function that takes a list of data frames as input and returns a list of data frames
# with information about the data types of each data frame.
num_dataType <- function(dataList) {
        
        # Initialize an empty list to store the results
        typeList <- list()
        
        # Loop over the data frames in the input list
        for(i in seq_along(dataList)) {
                
                # Count the number of columns that are of each data type
                num_char <- sum(sapply(dataList[[i]], is.character))
                num_nume <- sum(sapply(dataList[[i]], is.numeric))
                num_fact <- sum(sapply(dataList[[i]], is.factor))
                num_date <- sum(sapply(dataList[[i]], lubridate::is.Date))
                
                # Store the results in a data frame and add it to the output list
                typeList[[i]] <- data.frame(character = num_char,
                                            numeric = num_nume,
                                            factor = num_fact,
                                            date = num_date)
        }
        
        # Return the list of data frames
        names(typeList) <- names(dataList)
        return(typeList)
}
bellaFit_dataType <- num_dataType(bellaFit)

# This function takes a list of data frames as an input and returns a data frame 
# with information about the data types of each data frame.
num_dataType <- function(dataList) {
        
        # Calculate the number of distinct users per table
        users_per_table <- sapply(dataList, function(x){n_distinct(x[[1]])})
        
        # Calculate the number of variables per table
        var_per_table <- sapply(dataList, ncol)
        
        # Initialize empty vectors to hold the counts of each data type
        num_char <- c() # Count of character variables
        num_nume <- c() # Count of numeric variable
        num_fact <- c() # Count of factor variables
        num_date <- c() # Count of date variables
        
        # Loop through each data frame in the input list and count the variables of each data type
        for(i in seq_along(dataList)) {
                
                num_char[i] <- sum(sapply(dataList[[i]], is.character)) # Count the number of character variables
                num_nume[i] <- sum(sapply(dataList[[i]], is.numeric)) # Count the number of numeric variables
                num_fact[i] <- sum(sapply(dataList[[i]], is.factor)) # Count the number of factor variables
                num_date[i] <- sum(sapply(dataList[[i]], lubridate::is.Date)) # Count the number of date variables
                
        }
        
        # Combine the results into a data frame
        type_df <- data.frame(users = users_per_table,
                              variables = var_per_table,
                              character = num_char,
                              numeric = num_nume,
                              factor = num_fact,
                              date = num_date)
        
        # Assign the names of the input data frames as row names in the output data frame
        row.names(type_df) <- names(dataList)
        
        # Return the output data frame
        return(type_df)
}
bellaFit_dataType <- num_dataType(bellaFit)
bellaFit_dataType






