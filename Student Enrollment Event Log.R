# Load necessary libraries

# bupaR: Business Process Analysis in R
library(bupaR)

# eventdataR: Event Data Handling in R
library(eventdataR)

# processmapR: Process Map Visualization in R
library(processmapR)

# edeaR: Exploratory and Descriptive Event-Based Data Analysis in R
library(edeaR)

# For data manipulation
library(dplyr)

# Load the dataset from a CSV file
dataset <- read.csv("D:\\22.2 NSBM\\2nd Year 2nd Sem\\R programming\\Student_Enrollment_Event_Log.csv")

# Display the structure of the dataset
str(dataset)

# Replace empty strings with NA (missing values)
dataset[dataset == "?"] <- NA

# Print the data frame after replacing empty strings with NA
print(dataset)

# Count the number of missing values in the data frame
sum(is.na(dataset))

# Remove rows with missing values
dataset <- na.omit(dataset)

# Convert to datetime format
dataset$Opened_At              <- as.POSIXct(dataset$Opened_At, format = "%d/%m/%Y %H:%M")
dataset$Application_Created_At <- as.POSIXct(dataset$Application_Created_At, format = "%d/%m/%Y %H:%M")
dataset$Last_Updated_At        <- as.POSIXct(dataset$Last_Updated_At, format = "%d/%m/%Y %H:%M")
dataset$Resolved_At            <- as.POSIXct(dataset$Resolved_At, format = "%d/%m/%Y %H:%M")
dataset$Closed_At              <- as.POSIXct(dataset$Closed_At, format = "%d/%m/%Y %H:%M")

# Display the current data in a tabular format 
View(dataset)

# Arrange the dataset by Student_ID,Opened_At, then group by Student_ID
# Create a new column 'activity_instance_id' by concatenating Student_ID,Enrollment_Status, and row_number
# event_log_1
dataset <- dataset %>%
  arrange(Student_ID,Opened_At) %>%
  group_by(Student_ID) %>%
  mutate(activity_instance_id = paste(Student_ID,Enrollment_Status, row_number(), sep="_")) %>%
  ungroup()

# Arrange the dataset by Student_ID,Application_Created_At, then group by Student_ID
# Create a new column 'activity_instance_id' by concatenating Student_ID,Enrollment_Status, and row_number
# event_log_2
dataset <- dataset %>%
  arrange(Student_ID,Application_Created_At) %>%
  group_by(Student_ID) %>%
  mutate(activity_instance_id = paste(Student_ID,Enrollment_Status, row_number(), sep="_")) %>%
  ungroup()

# Arrange the dataset by Student_ID,Last_Updated_At, then group by Student_ID
# Create a new column 'activity_instance_id' by concatenating Student_ID,Enrollment_Status, and row_number
# event_log_3
dataset <- dataset %>%
  arrange(Student_ID,Last_Updated_At) %>%
  group_by(Student_ID) %>%
  mutate(activity_instance_id = paste(Student_ID,Enrollment_Status, row_number(), sep="_")) %>%
  ungroup()

# Arrange the dataset by Student_ID,Resolved_At, then group by Student_ID
# Create a new column 'activity_instance_id' by concatenating Student_ID,Enrollment_Status, and row_number
# event_log_4
dataset <- dataset %>%
  arrange(Student_ID,Resolved_At) %>%
  group_by(Student_ID) %>%
  mutate(activity_instance_id = paste(Student_ID,Enrollment_Status, row_number(), sep="_")) %>%
  ungroup()

# Arrange the dataset by Student_ID,Closed_At, then group by Student_ID
# Create a new column 'activity_instance_id' by concatenating Student_ID,Enrollment_Status, and row_number
# event_log_5
dataset <- dataset %>%
  arrange(Student_ID,Closed_At) %>%
  group_by(Student_ID) %>%
  mutate(activity_instance_id = paste(Student_ID,Enrollment_Status, row_number(), sep="_")) %>%
  ungroup()

# Set 'resource_id' column to "NA"
dataset$resource_id <- "NA" 

# Create an event log object from the dataset, specifying columns for Student_ID,Enrollment_Status, Opened_At, Enrollment_Category, resource_id, and activity_instance_id
event_log_1 <- eventlog(dataset,
                        case_id = "Student_ID",
                        activity_id = "Enrollment_Status",
                        timestamp = "Opened_At",
                        lifecycle_id = "Enrollment_Category",
                        resource_id = "resource_id",
                        activity_instance_id = "activity_instance_id")

# Generate a process map from the event log 1, with performance metrics calculated using median
process_map(event_log_1, performance(median))

# Create an event log object from the dataset, specifying columns for Student_ID,Enrollment_Status, Application_Created_At, Enrollment_Category, resource_id, and activity_instance_id
event_log_2 <- eventlog(dataset,
                        case_id = "Student_ID",
                        activity_id = "Enrollment_Status",
                        timestamp = "Application_Created_At",
                        lifecycle_id = "Enrollment_Category",
                        resource_id = "resource_id",
                        activity_instance_id = "activity_instance_id")

# Generate a process map from the event log 2, with performance metrics calculated using median
process_map(event_log_2, performance(median))

# Create an event log object from the dataset, specifying columns for Student_ID,Enrollment_Status, Last_Updated_At, Enrollment_Category, resource_id, and activity_instance_id
event_log_3 <- eventlog(dataset,
                        case_id = "Student_ID",
                        activity_id = "Enrollment_Status",
                        timestamp = "Last_Updated_At",
                        lifecycle_id = "Enrollment_Category",
                        resource_id = "resource_id",
                        activity_instance_id = "activity_instance_id")

# Generate a process map from the event log 3, with performance metrics calculated using median
process_map(event_log_3, performance(median))

# Create an event log object from the dataset, specifying columns for Student_ID,Enrollment_Status, Resolved_At, Enrollment_Category, resource_id, and activity_instance_id
event_log_4 <- eventlog(dataset,
                        case_id = "Student_ID",
                        activity_id = "Enrollment_Status",
                        timestamp = "Resolved_At",
                        lifecycle_id = "Enrollment_Category",
                        resource_id = "resource_id",
                        activity_instance_id = "activity_instance_id")

# Generate a process map from the event log 4, with performance metrics calculated using median
process_map(event_log_4, performance(median))

# Create an event log object from the dataset, specifying columns for Student_ID,Enrollment_Status, Closed_At, Enrollment_Category, resource_id, and activity_instance_id
event_log_5 <- eventlog(dataset,
                        case_id = "Student_ID",
                        activity_id = "Enrollment_Status",
                        timestamp = "Closed_At",
                        lifecycle_id = "Enrollment_Category",
                        resource_id = "resource_id",
                        activity_instance_id = "activity_instance_id")

# Generate a process map from the event log 5, with performance metrics calculated using median
process_map(event_log_5, performance(median))
