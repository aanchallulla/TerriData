##Load libraries
library(dplyr)    # Data manipulation
library(stringr)  # String cleaning
library(ggplot2)  # Visualization
library(tidyr)    # Data tidying
library(readxl)

##Loading the dataset
terridata<- read_excel("Thesis /Education dataset.xlsx")

##Exploring the dataset
View(terridata)
glimpse(terridata)
str(terridata)
unique(terridata$`Entity Code`)
length(unique(terridata$Entity))
length(unique(terridata$Department))
length(unique(terridata$Department))
length(unique(terridata$`Indicator`))
length(unique(terridata$`Numerical Data`))
is.na(terridata$`Numerical Data`)
length(is.na(terridata$`Numerical Data`))
sum(is.na(terridata$`Numerical Data`))

terridata %>%
  filter(`Indicator` == "Average score on the Saber 11 test - Critical reading"|
           `Indicator` =="Average score on Saber 11 tests - Mathematics") %>%
  arrange(`Indicator`) %>%
  View


terridata %>%
  filter((`Indicator` == "Gross coverage in transition") &
           (`Department` == "Bolívar")) %>%
  View


glimpse(terridata$`Numerical Data`)
unique(terridata$`Numerical Data`)

##Modifying the dataset
terridata <- terridata %>%
mutate(
`Numerical Data` = str_replace_all(`Numerical Data`, ",", "."),
)

terridata %>%
filter(`Numerical Data` == "0") %>%
View() ## we have a total of 7,388 obs

#checking the number of NA
terridata %>%
filter(is.na(`Numerical Data`)) %>%
View() ## we have a total of 23,673 obs

length(unique(terridata$`Numerical Data`))
sample(unique(terridata$`Numerical Data`), 50)
glimpse(terridata$`Numerical Data`)
unique(terridata$`Numerical Data`)
head(sort(unique(terridata$`Numerical Data`)), 50)
tail(sort(unique(terridata$`Numerical Data`)), 50)
head(sort(unique(terridata$`Numerical Data`)), 100)
tail(sort(unique(terridata$`Numerical Data`)), 100)
summary(as.numeric(terridata$`Numerical Data`))
terridata$`Numerical Data`[is.na(terridata$`Numerical Data`)]
terridata$`Numerical Data`[is.na(terridata$`Numerical Data`)]

##Converting chr to numeric

terridata <- terridata %>%
mutate(
`Numerical Data` = as.numeric(`Numerical Data`)
)           ##This introduces NAs by coercion and thus we now have 25,797 obs - 2124 more.

terridata <- terridata %>%
mutate(
Year = as.numeric(Year)
)

summary(as.numeric(terridata$`Numerical Data`))
glimpse(terridata)

##Removing unnecssary columns
filtered_data <- terridata%>%
select(
-Mes,                   # Remove month column
-`Qualitative Data`,     # Remove empty qualitative column
-Source,# Remove source column
-Subcategory
)
filtered_data %>% distinct() 
filtered_data[!duplicated(filtered_data),]
filtered_data[duplicated(filtered_data),] ##This tells us that we have 1655 row duplicates in the filtered data


##Do we need this code? It gives us the Indicators that are duplicated and we also don't need.
duplicates <- filtered_data |>
dplyr::summarise(n = dplyr::n(),
.by = c(`Department Code`, Department, `Entity Code`, Entity,
Dimension, Year, `Unit of Measurement`, Indicator)) |>
dplyr::filter(n > 1L)
View(duplicates)
unique(duplicates$Indicator)

##This code removes the rows with the above Indicators considering we don't need them anyway
patterns <- c(
"Net coverage in secondary education",
"Rate of grade repetition in the official sector in elementary and middle school",
"Number of children, girls and pregnant women with initial education within the framework of comprehensive early childhood care",
"Percentage of children in comprehensive early education services who receive six or more visits",
"Net coverage in basic education - Men"
)
regex_pattern <- paste(patterns, collapse = "|")
row_filtered <- filtered_data %>%
filter(!str_detect(Indicator, regex_pattern))
View(row_filtered)
unique(row_filtered$Indicator)  ##We are now left with 29 indicators

##Checking, apparently it works.
row_duplicates <- row_filtered|>
dplyr::summarise(n = dplyr::n(),
.by = c(`Department Code`, Department, `Entity Code`, Entity,
Dimension, Year, `Unit of Measurement`, Indicator)) |>
dplyr::filter(n > 1L)
View(row_duplicates)
unique(row_duplicates$Indicator)


## Removing rows with indicators we do not need, we are now left with 16 indicators

patterns1 <- c(
"Gross coverage in transition",
"Gross coverage in primary education",
"Gross coverage in secondary education" ,
"Gross coverage in middle school education",
"Gross coverage in basic education" ,
"Net coverage in transition education" ,
"Net coverage in primary education"   ,
"Net coverage in basic education" ,
"Rate of immediate transition to higher education" ,
"Percentage of preschool children who receive nutritional requirements through the PAE",
"Percentage of children whose families participate in training processes in initial education services within the framework of comprehensive care",
"Percentage of children in early childhood receiving early childhood education within the framework of care integral con esquema de vacunación completo para la edad",
"Net coverage in basic education - Women"
)
regex_pattern1 <- paste(patterns1, collapse = "|")
row_filtered1 <- row_filtered%>%
filter(!str_detect(Indicator, regex_pattern1))
View(row_filtered1)
unique(row_filtered1$Indicator) 

## Converting this to wide data format
wide_data <- row_filtered1 %>%
pivot_wider(names_from =`Indicator`, values_from = `Numerical Data`)
View(wide_data)
glimpse(wide_data)

##Exploring the wide data


unique(wide_data$`Coverage in higher education`)

sum(is.na(wide_data$`Coverage in higher education`))



## Adding the EFD dataset

efd <- read_excel("Thesis /EfD_all-that-glitters - edited.xlsx")
glimpse(efd)
length(unique(efd$muncod))

efd <- efd %>%
  mutate(
    muncod= as.character(muncod),
    muncod_old = as.character(muncod_old)
  )


## Replacing the muncode column to look like the terridata code

lookup <- unique(data.frame(
  old = efd$muncod_old,
  new = efd$muncod_new
))

# Then replace using match
efd$muncod <- lookup$new[match(efd$muncod, lookup$old)]

# If some values don't have a match and you want to keep original:
efd$muncod <- ifelse(is.na(match(efd$muncod, lookup$old)),
                    efd$muncod,
                    lookup$new[match(efd$muncod, lookup$old)])
##Dropping extra columns and making a new dataset 

efd_new <- efd %>% 
  select(
    -muncod_old,                  
    -`muncod_new`,  
  )
glimpse(efd_new)
glimpse(wide_data)

View(efd_new)
View(wide_data)

wide_data %>% 
  filter(Year== "2005" &
        `Entity Code` == "05002" ) %>% 
View()