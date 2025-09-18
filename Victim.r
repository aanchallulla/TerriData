##Load libraries
library(dplyr)    # Data manipulation
library(stringr)  # String cleaning
library(ggplot2)  # Visualization
library(tidyr)    # Data tidying
library(readxl)

##SEXUAL VIOLENCE DATSET
##Loading the dataset
victim_sv <- read_excel("Victim Dataset/Sexual Violence.xlsx")
View(victim_sv)
glimpse(victim_sv)
str(victim_sv)

##Filtering the dataset
victim_sv_new <- victim_sv%>%
  select(
    -Month,                   
    -Day,     
    -`Political Activist`,
    -Ethinicity,
    -`Quality of the Victim or Casualty`,
    -`Description of the Armed Force or Organized Armed Group to Which the Combatant Belongs`,
    -`Armed Force or Organized Armed Group to Which the Combatant Belongs` ,
    -`Current Situation of the Victim`  ,
    -Latitude,
    -Longitude,
  )

victim_sv_stud <- victim_sv_new %>% 
  filter(Occupation== "ESTUDIANTE") 


(unique(victim_sv_stud$Sex))
(unique(victim_sv_stud$Occupation))

victim_sv_stud <- victim_sv_stud %>%
  mutate(
    Sex = recode(Sex,
                 "HOMBRE" = "man",
                 "MUJER" = "woman"),
    Occupation = recode(Occupation,
                        "ESTUDIANTE" = "Student")
  )

victim_sv_stud %>%
  arrange(`Year`) %>%
  View

----------------------------------
##KIDNAPPINGS DATASET
##Loading the dataset
  
victim_kid<-read_excel("Victim Dataset/Kidnappings.xlsx")

victim_kid <- victim_kid %>%
  rename(
    Case_ID = `ID Caso`,
    Municipality_DANE_Code = `Código DANE de Municipio`,
    Municipality = Municipio,
    Department = Departamento,
    Year = Año,
    Month = Mes,
    Day = Día,
    Person_ID = `ID Persona`,
    Sex = Sexo,
    Ethnicity = Etnia,
    Occupation = Ocupación,
    Victim_Status = `Calidad de la Víctima o la Baja`,
    Vulnerable_Population_Type = `Tipo de Población Vulnerable`,
    Political_Militant = `Militante Político`,
    Armed_Group = `Fuerza o Grupo Armado Organizado al que Pertenece el Combatiente`,
    Armed_Group_Description = `Descripción Fuerza o Grupo Armado Organizado al que Pertenece el Combatiente`,
    Victim_Current_Status = `Situación Actual de la Víctima`,
    Death_in_Captivity_Circumstance = `Circunstancia Muerte en Cautiverio`,
    Release_Type = `Tipo de Liberación`,
    Days_in_Captivity = `Días de Cautiverio`,
    Times_Kidnapped = `No. de Veces secuestrado`,
    Age = Edad,
    Latitude = Latitud,
    Longitude = Longitud
  )
View(victim_kid)
glimpse(victim_kid)
colnames(victim_kid)


##Filtering the dataset

victim_kid_new <- victim_kid %>%
  select(
    -Month,                   
    -Day,     
    -Political_Militant,
    -Ethnicity,
    -Victim_Status,
    -Vulnerable_Population_Type,
    -Armed_Group,
    -Armed_Group_Description,
    -Victim_Current_Status,
    -Death_in_Captivity_Circumstance,
    -Release_Type,
    -Days_in_Captivity,
    -Times_Kidnapped,
    -Latitude,
    -Longitude,
  )

victim_kid_stud<- victim_kid_new %>% 
  filter(Occupation== "ESTUDIANTE") 


(unique(victim_kid_stud$Sex))
(unique(victim_kid_stud$Occupation))

victim_kid_stud <- victim_kid_stud %>%
  mutate(
    Sex = recode(Sex,
                 "HOMBRE" = "man",
                 "MUJER" = "woman",
                 "SIN INFORMACION" ="No information"),
    Occupation = recode(Occupation,
                        "ESTUDIANTE" = "Student")
  )

victim_kid_stud %>%
  arrange(`Year`) %>%
  View

----------------------
##WAR ACTIONS DATASET
  ##Loading the dataset

victim_war<-read_excel("Victim Dataset/War Actions.xlsx")
colnames(victim_war)
library(dplyr)

victim_war_new <- victim_war %>%
  rename(
    Case_ID = `ID Caso`,
    Municipality_DANE_Code = `Código DANE de Municipio`,
    Municipality = Municipio,
    Department = Departamento,
    Year = Año,
    Month = Mes,
    Day = Día,
    Person_ID = `ID Persona`,
    Sex = Sexo,
    Ethnicity = Etnia,
    Occupation = Ocupación,
    Victim_Status = `Calidad de la Víctima o la Baja`,
    Vulnerable_Population_Type = `Tipo de Población Vulnerable`,
    Armed_Group = `Fuerza o Grupo Armado Organizado al que Pertenece el Combatiente`,
    Armed_Group_Description = `Descripción Fuerza o Grupo Armado Organizado al que Pertenece el Combatiente`,
    Age = Edad,
    Latitude = Latitud,
    Longitude = Longitud
  )
colnames(victim_war_new)

victim_war_new <- victim_war_new %>%
  select(
    -Month,                   
    -Day,     
    -Ethnicity,
    -Victim_Status,
    -Vulnerable_Population_Type,
    -Armed_Group,
    -Armed_Group_Description,
    -Latitude,
    -Longitude,
  )


victim_war_stud<- victim_war_new %>% 
  filter(Occupation== "ESTUDIANTE") 


(unique(victim_war_stud$Sex))
(unique(victim_war_stud$Occupation))

victim_war_stud <- victim_war_stud %>%
  mutate(
    Sex = recode(Sex,
                 "HOMBRE" = "man",
                 "MUJER" = "woman",
                 "SIN INFORMACION" ="No information"),
    Occupation = recode(Occupation,
                        "ESTUDIANTE" = "Student")
  )

victim_war_stud %>%
  arrange(`Year`) %>%
  View


----------------------------------------
##Population Attack ACTIONS DATASET
##Loading the dataset

victim_pop<-read_excel("Victim Dataset/Population Attacks.xlsx")
colnames(victim_pop)
library(dplyr)

victim_pop_new <- victim_pop %>%
  rename(
    Case_ID = `ID Caso`,
    Municipality_DANE_Code = `Código DANE de Municipio`,
    Municipality = Municipio,
    Department = Departamento,
    Year = Año,
    Month = Mes,
    Day = Día,
    Person_ID = `ID Persona`,
    Sex = Sexo,
    Ethnicity = Etnia,
    Occupation = Ocupación,
    Victim_Status = `Calidad de la Víctima o la Baja`,
    Vulnerable_Population_Type = `Tipo de Población Vulnerable`,
    Armed_Group = `Fuerza o Grupo Armado Organizado al que Pertenece el Combatiente`,
    Armed_Group_Description = `Descripción Fuerza o Grupo Armado Organizado al que Pertenece el Combatiente`,
    Age = Edad,
    Latitude = Latitud,
    Longitude = Longitud
  )
colnames(victim_pop_new)

victim_pop_new <- victim_pop_new %>%
  select(
    -Month,                   
    -Day,     
    -Ethnicity,
    -Victim_Status,
    -Vulnerable_Population_Type,
    -Armed_Group,
    -Armed_Group_Description,
    -Latitude,
    -Longitude,
  )


victim_pop_stud<- victim_pop_new %>% 
  filter(Occupation== "ESTUDIANTE") 


(unique(victim_pop_stud$Sex))
(unique(victim_pop_stud$Occupation))

victim_pop_stud <- victim_pop_stud %>%
  mutate(
    Sex = recode(Sex,
                 "HOMBRE" = "man",
                 "MUJER" = "woman",
                 "SIN INFORMACION" ="No information"),
    Occupation = recode(Occupation,
                        "ESTUDIANTE" = "Student")
  )

victim_pop_stud %>%
  arrange(`Year`) %>%
  View

----------------------------------------
  ##SELECTIVE MURDERS DATASET
  ##Loading the dataset
  
victim_mur<-read_excel("Victim Dataset/Selective Murders.xlsx")
colnames(victim_mur)
library(dplyr)

victim_mur_new <- victim_mur %>%
  rename(
    Case_ID = `ID Caso`,
    Municipality_DANE_Code = `Código DANE de Municipio`,
    Municipality = Municipio,
    Department = Departamento,
    Year = Año,
    Month = Mes,
    Day = Día,
    Person_ID = `ID Persona`,
    Sex = Sexo,
    Ethnicity = Etnia,
    Occupation = Ocupación,
    Victim_Status = `Calidad de la Víctima o la Baja`,
    Political_Militant = `Militante Político`,
    Vulnerable_Population_Type = `Tipo de Población Vulnerable`,
    Armed_Group = `Fuerza o Grupo Armado Organizado al que Pertenece el Combatiente`,
    Armed_Group_Description = `Descripción Fuerza o Grupo Armado Organizado al que Pertenece el Combatiente`,
    Age = Edad,
    Latitude = Latitud,
    Longitude = Longitud
  )
colnames(victim_mur_new)

victim_mur_new <- victim_mur_new %>%
  select(
    -Month,                   
    -Day,     
    -Ethnicity,
    -Victim_Status,
    -Vulnerable_Population_Type,
    -Political_Militant,
    -Armed_Group,
    -Armed_Group_Description,
    -Latitude,
    -Longitude,
  )


victim_mur_stud<- victim_mur_new %>% 
  filter(Occupation== "ESTUDIANTE") 


(unique(victim_mur_stud$Sex))
(unique(victim_mur_stud$Occupation))
(unique(victim_mur_stud$Year))

victim_mur_stud <- victim_mur_stud %>%
  mutate(
    Sex = recode(Sex,
                 "HOMBRE" = "man",
                 "MUJER" = "woman",
                 "SIN INFORMACION" ="No information"),
    Occupation = recode(Occupation,
                        "ESTUDIANTE" = "Student")
  )

victim_mur_stud %>%
  arrange(`Year`) %>%
  View


----------------------------------------
  ##TERRORIST ATTACKS DATASET
  ##Loading the dataset
  
victim_ter<-read_excel("Victim Dataset/Terrorist Attacks.xlsx")
colnames(victim_ter)
library(dplyr)

victim_ter_new <- victim_ter %>%
  rename(
    Case_ID = `ID Caso`,
    Municipality_DANE_Code = `Código DANE de Municipio`,
    Municipality = Municipio,
    Department = Departamento,
    Year = Año,
    Month = Mes,
    Day = Día,
    Person_ID = `ID Persona`,
    Sex = Sexo,
    Ethnicity = Etnia,
    Occupation = Ocupación,
    Victim_Status = `Calidad de la Víctima o la Baja`,
    Vulnerable_Population_Type = `Tipo de Población Vulnerable`,
    Armed_Group = `Fuerza o Grupo Armado Organizado al que Pertenece el Combatiente`,
    Armed_Group_Description = `Descripción Fuerza o Grupo Armado Organizado al que Pertenece el Combatiente`,
    Age = Edad,
    Latitude = Latitud,
    Longitude = Longitud
  )
colnames(victim_ter_new)

victim_ter_new <- victim_ter_new %>%
  select(
    -Month,                   
    -Day,     
    -Ethnicity,
    -Victim_Status,
    -Vulnerable_Population_Type,
    -Armed_Group,
    -Armed_Group_Description,
    -Latitude,
    -Longitude,
  )


victim_ter_stud<- victim_ter_new %>% 
  filter(Occupation== "ESTUDIANTE") 


(unique(victim_ter_stud$Sex))
(unique(victim_ter_stud$Occupation))
(unique(victim_ter_stud$Year))

victim_ter_stud <- victim_ter_stud %>%
  mutate(
    Sex = recode(Sex,
                 "HOMBRE" = "man",
                 "MUJER" = "woman",
                 "SIN INFORMACION" ="No information"),
    Occupation = recode(Occupation,
                        "ESTUDIANTE" = "Student")
  )

victim_ter_stud %>%
  arrange(`Year`) %>%
  View


----------------------------------------
  ##Civil Property DATASET
  ##Loading the dataset
  
victim_civ<-read_excel("Victim Dataset/Civil Property .xlsx")
colnames(victim_civ)
library(dplyr)

victim_civ_new <- victim_civ %>%
  rename(
    Case_ID = `ID Caso`,
    Municipality_DANE_Code = `Código DANE de Municipio`,
    Municipality = Municipio,
    Department = Departamento,
    Year = Año,
    Month = Mes,
    Day = Día,
    Person_ID = `ID Persona`,
    Sex = Sexo,
    Ethnicity = Etnia,
    Occupation = Ocupación,
    Victim_Status = `Calidad de la Víctima o la Baja`,
    Vulnerable_Population_Type = `Tipo de Población Vulnerable`,
    Political_Militant = `Militante Político`,
    Armed_Group = `Fuerza o Grupo Armado Organizado al que Pertenece el Combatiente`,
    Armed_Group_Description = `Descripción Fuerza o Grupo Armado Organizado al que Pertenece el Combatiente`,
    Age = Edad,
    Latitude = Latitud,
    Longitude = Longitud
  )
colnames(victim_civ_new)

victim_civ_new <- victim_civ_new %>%
  select(
    -Month,                   
    -Day,     
    -Ethnicity,
    -Victim_Status,
    -Vulnerable_Population_Type,
    -Armed_Group,
    -Political_Militant,
    -Armed_Group_Description,
    -Latitude,
    -Longitude,
  )


victim_civ_stud<- victim_civ_new %>% 
  filter(Occupation== "ESTUDIANTE") 


(unique(victim_civ_stud$Sex))
(unique(victim_civ_stud$Occupation))
(unique(victim_civ_stud$Year))

victim_civ_stud <- victim_civ_stud %>%
  mutate(
    Sex = recode(Sex,
                 "HOMBRE" = "man",
                 "MUJER" = "woman",
                 "SIN INFORMACION" ="No information"),
    Occupation = recode(Occupation,
                        "ESTUDIANTE" = "Student")
  )

victim_civ_stud %>%
  arrange(`Year`) %>%
  View


----------------------------------------
  ##Forced Disappearance DATASET
  ##Loading the dataset
  
victim_dis<-read_excel("Victim Dataset/Forced Disappearance.xlsx")
colnames(victim_dis)
library(dplyr)

victim_dis_new <- victim_dis %>%
  rename(
    Case_ID = `ID Caso`,
    Municipality_DANE_Code = `Código DANE de Municipio`,
    Municipality = Municipio,
    Department = Departamento,
    Year = Año,
    Month = Mes,
    Day = Día,
    Person_ID = `ID Persona`,
    Sex = Sexo,
    Ethnicity = Etnia,
    Occupation = Ocupación,
    Victim_Status = `Calidad de la Víctima o la Baja`,
    Vulnerable_Population_Type = `Tipo de Población Vulnerable`,
    Political_Militant = `Militante Político`,
    Armed_Group = `Fuerza o Grupo Armado Organizado al que Pertenece el Combatiente`,
    Armed_Group_Description = `Descripción Fuerza o Grupo Armado Organizado al que Pertenece el Combatiente`,
    Current_Situation = `Situación Actual de la Víctima`,                                             
    Source_of_Information = `Fuente de Información de la Desaparición`,
    Age = Edad,
    Latitude = Latitud,
    Longitude = Longitud
  )
colnames(victim_dis_new)

victim_dis_new <- victim_dis_new %>%
  select(
    -Month,                   
    -Day,     
    -Ethnicity,
    -Victim_Status,
    -Vulnerable_Population_Type,
    -Armed_Group,
    -Political_Militant,
    -Armed_Group_Description,
    -Current_Situation,                                     
    -Source_of_Information,
    -Latitude,
    -Longitude,
  )


victim_dis_stud<- victim_dis_new %>% 
  filter(Occupation== "ESTUDIANTE") 


(unique(victim_dis_stud$Sex))
(unique(victim_dis_stud$Occupation))
(unique(victim_dis_stud$Year))

victim_dis_stud <- victim_dis_stud %>%
  mutate(
    Sex = recode(Sex,
                 "HOMBRE" = "man",
                 "MUJER" = "woman",
                 "SIN INFORMACION" ="No information"),
    Occupation = recode(Occupation,
                        "ESTUDIANTE" = "Student")
  )

victim_dis_stud %>%
  arrange(`Year`) %>%
  View


----------------------------------------
  ##Massacres DATASET
  ##Loading the dataset
  
victim_mas<-read_excel("Victim Dataset/Massacres.xlsx")
colnames(victim_mas)
library(dplyr)

victim_mas_new <- victim_mas %>%
  rename(
    Case_ID = `ID Caso`,
    Municipality_DANE_Code = `Código DANE de Municipio`,
    Municipality = Municipio,
    Department = Departamento,
    Year = Año,
    Month = Mes,
    Day = Día,
    Person_ID = `ID Persona`,
    Sex = Sexo,
    Ethnicity = Etnia,
    Occupation = Ocupación,
    Victim_Status = `Calidad de la Víctima o la Baja`,
    Vulnerable_Population_Type = `Tipo de Población Vulnerable`,
    Political_Militant = `Militante Político`,
    Armed_Group = `Fuerza o Grupo Armado Organizado al que Pertenece el Combatiente`,
    Armed_Group_Description = `Descripción Fuerza o Grupo Armado Organizado al que Pertenece el Combatiente`,
    Age = Edad,
    Latitude = Latitud,
    Longitude = Longitud
  )
colnames(victim_mas_new)

victim_mas_new <- victim_mas_new %>%
  select(
    -Month,                   
    -Day,     
    -Ethnicity,
    -Victim_Status,
    -Vulnerable_Population_Type,
    -Armed_Group,
    -Political_Militant,
    -Armed_Group_Description,
    -Latitude,
    -Longitude,
  )


victim_mas_stud<- victim_mas_new %>% 
  filter(Occupation== "ESTUDIANTE") 


(unique(victim_mas_stud$Sex))
(unique(victim_mas_stud$Occupation))
(unique(victim_mas_stud$Year))

victim_mas_stud <- victim_mas_stud %>%
  mutate(
    Sex = recode(Sex,
                 "HOMBRE" = "man",
                 "MUJER" = "woman",
                 "SIN INFORMACION" ="No information"),
    Occupation = recode(Occupation,
                        "ESTUDIANTE" = "Student")
  )

victim_mas_stud %>%
  arrange(`Year`) %>%
  View

----------------------------------------
  ##Mines DATASET
  ##Loading the dataset
  
victim_mine<-read_excel("Victim Dataset/Mines .xlsx")
colnames(victim_mine)
library(dplyr)

victim_mine_new <- victim_mine %>%
  rename(
    Case_ID = `ID Caso`,
    Municipality_DANE_Code = `Código DANE de Municipio`,
    Municipality = Municipio,
    Department = Departamento,
    Year = Año,
    Month = Mes,
    Day = Día,
    Person_ID = `ID Persona`,
    Sex = Sexo,
    Ethnicity = Etnia,
    Occupation = Ocupación,
    Victim_Status = `Calidad de la Víctima o la Baja`,
    Vulnerable_Population_Type = `Tipo de Población Vulnerable`,
    Armed_Group = `Fuerza o Grupo Armado Organizado al que Pertenece el Combatiente`,
    Armed_Group_Description = `Descripción Fuerza o Grupo Armado Organizado al que Pertenece el Combatiente`,
    Current_Situtaion = `Situación Actual de la Víctima`,                                             
    Injuries = `Afectación Heridos`,
    Circumstances_of_Death =`Circunstancias de la Muerte de la Víctima`,
    Activity = `Actividad Desarrollada en el Momento de la Afectación`,
    Age = Edad,
    Latitude = Latitud,
    Longitude = Longitud
  )
colnames(victim_mine_new)

victim_mine_new <- victim_mine_new %>%
  select(
    -Month,                   
    -Day,     
    -Ethnicity,
    -Victim_Status,
    -Vulnerable_Population_Type,
    -Armed_Group,
    -Armed_Group_Description,
    -Current_Situtaion,
    -Injuries,
    -Circumstances_of_Death,
    -Activity,
    -Latitude,
    -Longitude,
  )


victim_mine_stud<- victim_mine_new %>% 
  filter(Occupation== "ESTUDIANTE") 


(unique(victim_mine_stud$Sex))
(unique(victim_mine_stud$Occupation))
(unique(victim_mine_stud$Year))

victim_mine_stud <- victim_mine_stud %>%
  mutate(
    Sex = recode(Sex,
                 "HOMBRE" = "man",
                 "MUJER" = "woman",
                 "SIN INFORMACION" ="No information"),
    Occupation = recode(Occupation,
                        "ESTUDIANTE" = "Student")
  )

victim_mine_stud %>%
  arrange(`Year`) %>%
  View