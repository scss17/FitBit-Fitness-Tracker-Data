# Importing libraries and data sets ---------------------------------------

# Installing packages
install.packages("janitor")

# Load libraries
library(ggplot2)
library(readr)
library(stringr)
library(dplyr)
library(janitor)
library(lubridate)
library(tidyr)
library(cowplot)

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

# Define a function takes a list of data frames as an input and returns a data frame 
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
        num_logi <- c() # Count of logical variables
        
        # Loop through each data frame in the input list and count the variables of each data type
        for(i in seq_along(dataList)) {
                
                num_char[i] <- sum(sapply(dataList[[i]], is.character)) # Count the number of character variables
                num_nume[i] <- sum(sapply(dataList[[i]], is.numeric)) # Count the number of numeric variables
                num_fact[i] <- sum(sapply(dataList[[i]], is.factor)) # Count the number of factor variables
                num_date[i] <- sum(sapply(dataList[[i]], lubridate::is.Date)) # Count the number of date variables
                num_logi[i] <- sum(sapply(dataList[[i]], is.logical)) # Count the number of logical variables
        }
        
        # Combine the results into a data frame
        type_df <- data.frame(users = users_per_table,
                              variables = var_per_table,
                              character = num_char,
                              numeric = num_nume,
                              factor = num_fact,
                              date = num_date,
                              logical = num_logi)
        
        # Assign the names of the input data frames as row names in the output data frame
        row.names(type_df) <- names(dataList)
        
        # Return the output data frame
        return(type_df)
}
bellaFit_dataType <- num_dataType(bellaFit)
bellaFit_dataType

# Inspect the data sets content and structure
glimpse(bellaFit$dailyActivity)
glimpse(bellaFit$dailyCalories)
glimpse(bellaFit$dailyIntensities)
glimpse(bellaFit$dailySteps)
glimpse(bellaFit$heartrate)
glimpse(bellaFit$sleepDay)

# Clean the data ----------------------------------------------------------

# Check the first and last date in the data set
min(lubridate::as_date(bellaFit$dailyActivity$ActivityDate, format = "%m/%d/%Y")) 
max(lubridate::as_date(bellaFit$dailyActivity$ActivityDate, format = "%m/%d/%Y"))

# Change data type from text to date  
bellaFit$dailyActivity <- bellaFit$dailyActivity %>%
        mutate(ActivityDate = as_date(ActivityDate, format = "%m/%d/%Y"))

bellaFit$dailySteps <- bellaFit$dailySteps %>%
        mutate(ActivityDay = as_date(ActivityDay, format = "%m/%d/%Y"))

bellaFit$sleepDay <- bellaFit$sleepDay %>%
        mutate(SleepDay = mdy_hms(bellaFit$sleepDay$SleepDay))

# Rename columns
bellaFit$dailyActivity <- clean_names(bellaFit$dailyActivity)
bellaFit$dailySteps <- clean_names(bellaFit$dailySteps)
bellaFit$sleepDay <- clean_names(bellaFit$sleepDay)

# Rename data set names
chosen_tables <- c("dailyActivity", "dailySteps", "sleepDay")
names(bellaFit)[which(names(bellaFit) %in% chosen_tables)] <- c("daily_activity", "daily_steps", "sleep_day") 

# Remove duplicated rows
# Create a data frame with the duplicated rows which are present in the data sets
data.frame(table_name = c("dailyActivity", "dailySteps", "sleepDay"),
           duplicated_rows = c(sum(duplicated(bellaFit$dailyActivity)),
                               sum(duplicated(bellaFit$dailySteps)),
                               sum(duplicated(bellaFit$sleepDay))))

# Remote duplicated rows
bellaFit$sleepDay <- bellaFit$sleepDay %>% distinct()


# Visualizations ----------------------------------------------------------

# Sleeping habits
# Weekends vs Weekdays
sl1 <- bellaFit$sleep_day %>% 
        select(-total_sleep_records) %>%
        mutate(type_of_day = ifelse(wday(sleep_day) %in% 2:6, "Weekday", "Weekend")) %>%
        ggplot(mapping = aes(x = type_of_day, y = total_minutes_asleep, fill = type_of_day)) +
        geom_boxplot() + 
        geom_hline(yintercept = mean(bellaFit$sleep_day$total_minutes_asleep), lty = "twodash", col = "red") +
        scale_y_continuous(limits = c(0, 1000)) +
        theme(legend.position = "none") +
        labs(x = "", 
             y = "Minutes Asleep")

sl2 <- bellaFit$sleep_day %>% 
        select(-total_sleep_records) %>%
        mutate(type_of_day = ifelse(wday(sleep_day) %in% 2:6, "Weekday", "Weekend")) %>%
        ggplot(mapping = aes(x = type_of_day, y = total_time_in_bed, fill = type_of_day)) +
        geom_boxplot() +
        geom_hline(yintercept = mean(bellaFit$sleep_day$total_time_in_bed), lty = "twodash", col = "red") + 
        scale_y_continuous(limits = c(0, 1000)) +
        theme(legend.position = "none") +
        labs(x = "", 
             y = "Minutes in Bed")

plot_grid(sl1, sl2, labels = c('A', 'B'), label_size = 12)

# Along the week 
sl3 <- bellaFit$sleep_day %>% select(-total_sleep_records) %>%
        mutate(day = factor(wday(sleep_day), 
                            labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
                            ordered = TRUE)) %>%
        ggplot(mapping = aes(x = day, y = total_minutes_asleep, fill = day)) +
        geom_boxplot() + 
        scale_y_continuous(limits = c(0, 1000)) +
        theme(legend.position = "none") +
        labs(x = "Day", y = "Minutes Asleep")

sl4 <- bellaFit$sleep_day %>% select(-total_sleep_records) %>%
        mutate(day = factor(wday(sleep_day), 
                            labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
                            ordered = TRUE)) %>%
        ggplot(mapping = aes(x = day, y = total_time_in_bed, fill = day)) +
        geom_boxplot() + 
        scale_y_continuous(limits = c(0, 1000)) +
        theme(legend.position = "none") +
        labs(x = "Day", y = "Minutes in Bed")   

plot_grid(sl3, sl4, labels = c('A', 'B'), label_size = 12)



# Correlation matrix
bellaFit$daily_activity %>% 
        select(calories, total_steps, 
               total_distance, 
               sedentary_minutes, 
               lightly_active_minutes, 
               fairly_active_minutes,
               very_active_minutes) %>% 
        
        rename(cal = calories,
               ste = total_steps,
               dis = total_distance,
               sed = sedentary_minutes,
               lig = lightly_active_minutes,
               fai = fairly_active_minutes,
               ver = very_active_minutes) %>% 

cor() %>% corrplot::corrplot(type = "lower", method = "number")

# Scatter plot
bellaFit$daily_activity %>% 
        select(calories, total_distance) %>%
        ggplot(mapping = aes(x = total_distance, y = calories)) +
        geom_point(alpha = 0.5) +  geom_smooth(col = "red") +
        labs(y = "Calories", x = "Total Distance")


# Statistics --------------------------------------------------------------
# Sleep summary statistics
bellaFit$sleep_day %>% 
        select(total_minutes_asleep, total_time_in_bed) %>% 
        summary()


# Number of steps
bellaFit$daily_steps %>% 
        select(id, step_total) %>%
        group_by(id) %>%
        summarise(mean_day_steps = mean(step_total)) %>%
        mutate(category = factor(case_when(mean_day_steps < 5000 ~ "Inactive",
                                           mean_day_steps >= 5000 & mean_day_steps <= 10000 ~ "Average",
                                           mean_day_steps > 10000 ~ "Active"),
                                 levels = c("Inactive", "Average", "Active"),
                                 ordered = TRUE)) %>% 
        select(category) %>% group_by(category) %>% summarise(count_category = n()) %>%
        mutate(percent = count_category / sum(count_category))


# Daily use
bellaFit$daily_activity %>% select(id) %>%
        group_by(id) %>% summarise(days_used = n()) %>%
        mutate(category = factor(case_when(days_used < 10 ~ "Low Use",
                                           days_used >= 10 & days_used <= 20 ~ "Average Use",
                                           days_used > 20 ~ "High Use"),
                                 levels = c("Low Use", "Average Use", "High Use"),
                                 ordered = TRUE)) %>%
        select(category) %>% group_by(category) %>% summarise(count_category = n()) %>%
        mutate(percent = count_category / sum(count_category))


