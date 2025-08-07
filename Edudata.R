##Load libraries
library(dplyr)    # Data manipulation
library(stringr)  # String cleaning
library(ggplot2)  # Visualization
library(tidyr)    # Data tidying
library(readxl)

##Loading the dataset 
terridata<- read_excel("Thesis /Education dataset.xlsx")
View(terridata)                                                                                      
glimpse(terridata)

##Trying to understand the dataset 
head(terridata)
str(terridata)
unique(terridata$`Entity Code`)
unique(terridata$`Indicator`)
##Cleaning the dataset 

terridata <- terridata %>%
  mutate(
    `Numerical Data` = str_replace(`Numerical Data`, ",", "."),
    `Numerical Data` = as.numeric(`Numerical Data`),
    Year = as.numeric(Year)
  )

summary(terridata$`Numerical Data`)  # Check min/max/NA count
table(terridata$Year)                # Check year range

##Remove unnecesary columns
filtered_data <- terridata%>%
  select(
    -Mes,                   # Remove month column
    -`Qualitative Data`,     # Remove empty qualitative column
    -Source                 # Remove source column (optional)
  )

##

wide_data <- filtered_data %>%
  pivot_wider(
    # Columns that identify unique rows (will stay as-is)
    id_cols = c(`Department Code`, Department, `Entity Code`, Entity, Year),
    
    # Column whose values will become new column names
    names_from = Indicator,
    
    # Columns whose values will fill the new columns
    values_from = c(`Numerical Data`, `Unit of Measurement`),
    
    # Format new column names as "Indicator_[Numerical Data/Unit of Measurement]"
    names_glue = "{Indicator}_{.value}"
  )

wide_data <- wide_data%>%
  select(-)










column_names <- names(wide_data)
view(column_names)









