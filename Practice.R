
##Exploring the dataset using a video on youtube

library(tidyverse)
data()
?starwars
dim(starwars)
str(starwars)
glimpse(starwars)
view(starwars)
head(starwars)
tail(starwars)

attach(starwars) ##lazy and not best practice
names(starwars)
length(starwars) 
class(hair_color)
unique(hair_color)
view(sort(table(hair_color), decreasing = TRUE))
barplot(sort(table(hair_color), decreasing = TRUE))

starwars%>%
  select(hair_color)%>%
  count(hair_color)%>%
  arrange(desc(n))%>%
  view()

##both above codes are the same, the second one is tidyverse
view(starwars[is.na(hair_color), ])

##whenever you use a square bracket and a comma within in, the word before the comma will tell R to
##select certain rows which aligns to what you say

class(height)
length(height)
summary(height)
boxplot(height)
hist(height)

###########################

##pipe operators

starwars%>%
  select(gender,height,mass,species) %>% 
  filter(species == "Human") %>% 
  na.omit() %>% 
  mutate(height = height/100) %>% 
  mutate(BMI= mass/height^2) %>% 
  group_by(gender) %>% 
  summarise(Average_BMI = mean(BMI))

##Cleaning the data with R
library(tidyverse)
view(starwars)
glimpse(starwars)
class(starwars$gender)
unique(starwars$gender)

starwars$gender <- as.factor(starwars$gender) ##convert to a factor variable
class(starwars$gender) 
levels(starwars$gender)
starwars$gender <- factor((starwars$gender),levels = c("masculine","feminine")) #changing levels in a factor

starwars %>% 
  select(name, height, ends_with("color")) %>% 
  filter(hair_color %in% c("blond", "brown") & height < 180)
  
           
#missing data
mean(starwars$height, na.rm = TRUE) ##take the average height na.rm - if its NA then remove 
starwars %>% 
  select(name,gender, hair_color, height)

starwars %>% 
  select(name,gender, hair_color, height) %>% 
  na.omit(). ##NOT RECOMMENDED - do not know what the dataset is omitting

starwars %>% 
  select(name,gender, hair_color, height) %>% 
  filter(complete.cases(.)) ## does the same thing as na.omit - keeps the datapoints that are complete. 

starwars %>% 
  select(name,gender, hair_color, height) %>% 
  filter(!complete.cases(.)) ##exact opposite of the above code - shows where NA actually is

starwars %>% 
  select(name,gender, hair_color, height) %>% 
drop_na(height)

starwars %>% 
  select(name,gender, hair_color, height) %>% 
  filter(!complete.cases(.)) %>% 
  mutate(hair_color = replace_na(hair_color, "none"))


##Duplicates
Names <- c("Peter", "John", "Andrew", "Peter")
Age <- c(22,33,44,22)
Friends <- data.frame(Names,Age)
Friends %>% distinct()

duplicated(Friends) ##creates a logical vector
Friends[!duplicated(Friends),]


#Recoding variables
starwars %>% 
  select(name, gender) %>% 
  mutate = (gender = recode(gender, "masculine" = 1, "feminine" = 2))
view(starwars)

## Manipulate your data. Data wrangling. R programmning for beginners.

library(tidyverse)
?msleep  
glimpse(msleep)  
view(msleep)  

#rename a variable 

msleep %>% 
  rename("conserv" = "conservation") %>% 
  glimpse()


#reorder the variables
msleep %>% 
  select(vore, name, everything())

#change variable type character to factor and then back 
class(msleep$vore) #vore is a character
msleep$vore <- as.factor(msleep$vore) #changes to factor 
glimpse(msleep)

#using tidyverse
msleep %>% 
  mutate(vore = as.character(vore)) %>% #changing back into character 
  glimpse()

#select variables to work with 
names(msleep)

msleep %>% 
  select(2:4,
         awake,
         starts_with("sleep"),
         contains("wt")) %>% 
  names()

#filter and arrange the data 
unique(msleep$order)
 
##subset a data, select rows that meet certain criterias

## == is usually a question and not an assignment , | is "or"
## The code below filters the dataset by rows, it tells R to filter on the basis of the column order 
## and find either Carnivora or Primates who have a sleep total of more than 8
## It furthermore selects only 3 columns for view 
## And arranges in the decending order of the sleep

msleep %>% 
  filter((order == "Carnivora" |
         order == "Primates") & 
         sleep_total > 8) %>% 
  select(name, order, sleep_total) %>% 
  arrange(-sleep_total) %>% 
view


## same thing with a different code, arranged by order and written in an easier manner

msleep %>% 
  filter(order %in% c("Carnivora", "Primates") &
        sleep_total > 8 ) %>% 
  select(name, order, sleep_total) %>% 
  arrange(order) %>% 
  view


# change observations (mutate)
msleep %>% 
  mutate(brainwt = brainwt * 1000) %>% 
  view

# change observations (mutate)
msleep %>% 
  mutate(brainwt_ingm = brainwt * 1000) %>% 
  view


# conditonal changes (if else)
# logical vectors based on a condition

## where is this variable greater than 0.01? This gives us a logical vector TRUE FALSE
## if else starts with a logical fector, if the answer is true stick LARGE if its false stick FALSE
msleep$brainwt > 0.01

size_of_brain <- msleep %>% 
  select(name, brainwt) %>% 
  drop_na(brainwt) %>% 
  mutate(brain_size = if_else(brainwt > 0.01, "large","small"))
size_of_brain

## Recoding data, rename a variable
size_of_brain %>% 
  mutate(brain_size = recode(brain_size, "large" = 1, "small" = 2))


## long to wide OR wide to long 
library(gapminder)
view(gapminder)

data <- select(gapminder, country, year, lifeExp)
view(data)

wide_data <- data %>% 
  pivot_wider(names_from = year, values_from = lifeExp)
view(wide_data)

long_data <- wide_data %>% 
  pivot_longer(2:13,
               names_to = "year",
               values_to = "lifeExp")
view(long_data)





data <- data.frame(x1 = c(1, 2, 3, 1, 1, 2),        # Create example data
                   x2 = 1:6,
                   x3 = 3:1)
data                                                # Print example data


val_repl <- c(1, 3)                                 # Specify values to be replaced
val_repl      

data_new1 <- sapply(data,                           # Replace values in all columns
                    function(x) replace(x, x %in% val_repl, 99))
data_new1  

library(tidyverse)
library(babynames)
?left_join
babynames
