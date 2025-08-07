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
  
  
  
  
  
  
  
  
  
  
  
  



















