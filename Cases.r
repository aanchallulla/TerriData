##Load libraries
library(dplyr)    # Data manipulation
library(stringr)  # String cleaning
library(ggplot2)  # Visualization
library(tidyr)    # Data tidying
library(readxl)

##Loading Datasets
cases_pop <- read_excel("Conflict data/Cases/Attacks Populations.xlsx")
cases_dis <- read_excel("Conflict data/Cases/Forced Disappearance.xlsx")
cases_kid <- read_excel("Conflict data/Cases/Kidnapping.xlsx")
cases_mas <- read_excel("Conflict data/Cases/Massacre.xlsx")
cases_min <- read_excel("Conflict data/Cases/Mines.xlsx")
cases_civ <- read_excel("Conflict data/Cases/Property Damage.xlsx")
cases_mur <- read_excel("Conflict data/Cases/Selective Murder.xlsx")
cases_sv <- read_excel("Conflict data/Cases/Sexual Violence .xlsx")
cases_ter <- read_excel("Conflict data/Cases/Terrorist Attacks.xlsx")
cases_war <- read_excel("Conflict data/Cases/War Actions.xlsx")
cases_rec <- read_excel("Conflict data/Cases/Recruitment and use of Children and Adolescents.xlsx")

##########
##POPULATION ATTACK DATASET
colnames(cases_pop)

cases_pop <- cases_pop %>%
  rename(
    Case_ID = `ID Caso`,
    Related_Case_ID = `ID Caso Relacionado`,
    Year = Año,
    Month = Mes,
    Day = Día,
    Municipality_DANE_Code = `Código DANE de Municipio`,
    Municipality = Municipio,
    Department = Departamento,
    Region = Región,
    Armed_Group_1 = `Grupo Armado 1`,
    Armed_Group_1_Description = `Descripción Grupo Armado 1`,
    Armed_Group_2 = `Grupo Armado 2`,
    Armed_Group_2_Description = `Descripción Grupo Armado 2`,
    Armed_Group_3 = `Grupo Armado 3`,
    Armed_Group_3_Description = `Descripción Grupo Armado 3`,
    Forced_Land_Dispossession = `Abandono o Despojo Forzado de Tierras`,
    Threat_or_Intimidation = `Amenaza o Intimidación`,
    Attack_Against_Medical_Mission = `Ataque Contra Misión Médica`,
    Confinement_or_Mobility_Restriction = `Confinamiento o Restricción a la Movilidad`,
    Forced_Displacement = `Desplazamiento Forzado`,
    Extortion = Extorsión,
    Civilian_Injuries = `Lesionados Civiles`,
    Looting = Pillaje,
    Torture = Tortura,
    Gender_Based_Violence = `Violencia Basada en Género`,
    Other_Simultaneous_Event = `Otro Hecho Simultáneo`,
    Captured_Group_Affiliation = `Grupo al que Pertenecen los Capturados`,
    Captured = Capturados,
    Human_Shield = `Escudo Humano`,
    Combatant_Injuries = `Lesionados Combatientes`,
    Military = Militares,
    Police = Policías,
    Other_State_Armed_Forces = `Otras Fuerzas Armadas Estatales`,
    Unspecified_State_Agents = `Agentes del Estado Sin Información`,
    Total_State_Agents = `Total Agentes del Estado`,
    Guerrillas = Guerrilleros,
    Paramilitaries = Paramilitares,
    Post_Demobilization_Groups = `Grupos Posdesmovilización`,
    Unspecified_Combatants = `Combatientes Sin Información`,
    Other_Armed_Groups = `Otros Grupos Armados Organizados`,
    Total_Organized_Armed_Group_Combatants = `Total Combatientes de Grupos Armados Organizados`,
    Total_Combatants = `Total Combatientes`,
    Unspecified_Persons = `Personas Sin Información`,
    Total_Civilians = `Total Civiles`,
    Military_Advantage = `Ventaja Militar`,
    Total_Case_Victims = `Total de Víctimas del Caso`,
    Latitude = Latitud,
    Longitude = Longitud
  )


colnames(cases_pop)


##Filtering the dataset

cases_pop_new <- cases_pop %>%
  select(
    -Month,
    -Day,
    -Armed_Group_1,
    -Armed_Group_1_Description,
    -Armed_Group_2,
    -Armed_Group_2_Description,
    -Armed_Group_3,
    -Armed_Group_3_Description,
    -Forced_Land_Dispossession,
    -Threat_or_Intimidation,
    -Attack_Against_Medical_Mission,      
    -Confinement_or_Mobility_Restriction,
    -Forced_Displacement,
    -Extortion,                           
    -Civilian_Injuries,                      
    -Looting,
    -Torture,
    -Gender_Based_Violence,
    -Other_Simultaneous_Event,
    -Captured_Group_Affiliation,           
    -Captured,
    -Human_Shield,
    -Combatant_Injuries,            
    -Military,
    -Police,  
    -Other_State_Armed_Forces,        
    -Unspecified_State_Agents,
    -Total_State_Agents,
    -Guerrillas,                          
    -Paramilitaries,
    -Post_Demobilization_Groups,
    -Unspecified_Combatants,               
    -Other_Armed_Groups,
    -Total_Organized_Armed_Group_Combatants,
    -Total_Combatants,                      
    -Unspecified_Persons,
    -Total_Civilians,
    -Military_Advantage,
    -Latitude,
    -Longitude
  )
sort(unique(cases_pop_new$Year))
  

##############
##FORCED DISPLACEMENT DATASET

colnames(cases_dis)

cases_dis <- cases_dis %>%
  rename(
    Case_ID = `ID Caso`,
    Related_Case_ID = `ID Caso Relacionado`,
    Year = Año,
    Month = Mes,
    Day = Día,
    Municipality_DANE_Code = `Código DANE de Municipio`,
    Municipality = Municipio,
    Department = Departamento,
    Region = Región,
    Modality = Modalidad,
    Alleged_Responsible = `Presunto Responsable`,
    Alleged_Responsible_Description = `Descripción Presunto Responsable`,
    Forced_Land_Dispossession = `Abandono o Despojo Forzado de Tierras`,
    Threat_or_Intimidation = `Amenaza o Intimidación`,
    Attack_Against_Medical_Mission = `Ataque Contra Misión Médica`,
    Confinement_or_Mobility_Restriction = `Confinamiento o Restricción a la Movilidad`,
    Forced_Displacement = `Desplazamiento Forzado`,
    Extortion = Extorsión,
    Civilian_Injuries = `Lesionados Civiles`,
    Looting = Pillaje,
    Torture = Tortura,
    Gender_Based_Violence = `Violencia Basada en Género`,
    Other_Simultaneous_Event = `Otro Hecho Simultáneo`,
    Total_Case_Victims = `Total de Víctimas del Caso`,
    Weapons_Type = `Tipo de Armas`,
    Latitude = Latitud,
    Longitude = Longitud
  )

cases_dis_new <- cases_dis %>%
  select(
    -Month, 
    -Day, 
    -Modality, 
    -Alleged_Responsible,
    -Alleged_Responsible_Description,
    -Forced_Land_Dispossession,
    -Threat_or_Intimidation, 
    -Attack_Against_Medical_Mission, 
    -Confinement_or_Mobility_Restriction,
    -Forced_Displacement,
    -Extortion,
    -Civilian_Injuries,
    -Looting,
    -Torture,
    -Gender_Based_Violence,
    -Other_Simultaneous_Event,
    -Weapons_Type,
    -Latitude,
    -Longitude
  )
sort(unique(cases_dis_new$Year))


#######
##KIDNAPPING DATASET

colnames(cases_kid)

cases_kid <- cases_kid %>%
  rename(
    Case_ID = `ID Caso`,
    Related_Case_ID = `ID Caso Relacionado`,
    Year = Año,
    Month = Mes,
    Day = Día,
    Municipality_DANE_Code = `Código DANE de Municipio`,
    Municipality = Municipio,
    Department = Departamento,
    Region = Región,
    Modality = Modalidad,
    Alleged_Responsible = `Presunto Responsable`,
    Alleged_Responsible_Description = `Descripción Presunto Responsable`,
    Forced_Land_Dispossession = `Abandono o Despojo Forzado de Tierras`,
    Threat_or_Intimidation = `Amenaza o Intimidación`,
    Attack_Against_Medical_Mission = `Ataque Contra Misión Médica`,
    Confinement_or_Mobility_Restriction = `Confinamiento o Restricción a la Movilidad`,
    Forced_Displacement = `Desplazamiento Forzado`,
    Extortion = Extorsión,
    Civilian_Injuries = `Lesionados Civiles`,
    Looting = Pillaje,
    Torture = Tortura,
    Gender_Based_Violence = `Violencia Basada en Género`,
    Other_Simultaneous_Event = `Otro Hecho Simultáneo`,
    Total_Case_Victims = `Total de Víctimas del Caso`,
    Kidnapping_Modality = `Modalidad de Secuestro`,
    Kidnapping_Type = `Tipo de Secuestro`,
    Kidnapping_Purpose = `Finalidad del Secuestro`,
    Release_Demand = `Exigencia para la Liberación`,
    Latitude = Latitud,
    Longitude = Longitud
  )

cases_kid_new <- cases_kid %>%
select(
    -Month,
    -Day,
    -Modality,
    -Alleged_Responsible,
    -Alleged_Responsible_Description,
    -Forced_Land_Dispossession,
    -Threat_or_Intimidation,
    -Attack_Against_Medical_Mission,
    -Confinement_or_Mobility_Restriction,
    -Forced_Displacement,
    -Extortion,
    -Civilian_Injuries,
    -Looting,
    -Torture,
    -Gender_Based_Violence,
    -Other_Simultaneous_Event,
    -Kidnapping_Modality,
    -Kidnapping_Type,
    -Kidnapping_Purpose,
    -Release_Demand,
    -Latitude,
    -Longitude,
  )

sort(unique(cases_kid_new$Year))

############
##MASSCARE DATASET

colnames(cases_mas)

cases_mas <- cases_mas %>%
  rename(
    Case_ID = `ID Caso`,
    Related_Case_ID = `ID Caso Relacionado`,
    Year = Año,
    Month = Mes,
    Day = Día,
    Municipality_DANE_Code = `Código DANE de Municipio`,
    Municipality = Municipio,
    Department = Departamento,
    Region = Región,
    Modality = Modalidad,
    Alleged_Responsible = `Presunto Responsable`,
    Alleged_Responsible_Description = `Descripción Presunto Responsable`,
    Forced_Land_Dispossession = `Abandono o Despojo Forzado de Tierras`,
    Threat_or_Intimidation = `Amenaza o Intimidación`,
    Attack_Against_Medical_Mission = `Ataque Contra Misión Médica`,
    Confinement_or_Mobility_Restriction = `Confinamiento o Restricción a la Movilidad`,
    Forced_Displacement = `Desplazamiento Forzado`,
    Extortion = Extorsión,
    Civilian_Injuries = `Lesionados Civiles`,
    Looting = Pillaje,
    Torture = Tortura,
    Gender_Based_Violence = `Violencia Basada en Género`,
    Other_Simultaneous_Event = `Otro Hecho Simultáneo`,
    Total_Case_Victims = `Total de Víctimas del Caso`,
    Weapon_Type = `Tipo de Armas`,
    Latitude = Latitud,
    Longitude = Longitud
  )

cases_mas_new <- cases_mas %>%
  select(
    -Month,
    -Day,
    -Modality,
    -Alleged_Responsible,
    -Alleged_Responsible_Description,
    -Forced_Land_Dispossession,
    -Threat_or_Intimidation,
    -Attack_Against_Medical_Mission,
    -Confinement_or_Mobility_Restriction,
    -Forced_Displacement,
    -Extortion,
    -Civilian_Injuries ,
    -Looting,
    -Torture,
    -Gender_Based_Violence,
    -Other_Simultaneous_Event,
    -Weapon_Type,
    -Latitude,
    -Longitude
  )

sort(unique(cases_mas_new$Year))

########
##MINES DATASET

colnames(cases_min)

library(dplyr)

cases_min <- cases_min %>%
  rename(
    Case_ID = `ID Caso`,
    Related_Case_ID = `ID Caso Relacionado`,
    Year = Año,
    Month = Mes,
    Day = Día,
    Municipality_DANE_Code = `Código DANE de Municipio`,
    Municipality = Municipio,
    Department = Departamento,
    Region = Región,
    Modality = Modalidad,
    Alleged_Responsible = `Presunto Responsable`,
    Alleged_Responsible_Description = `Descripción Presunto Responsable`,
    Forced_Land_Dispossession = `Abandono o Despojo Forzado de Tierras`,
    Threat_or_Intimidation = `Amenaza o Intimidación`,
    Attack_Against_Medical_Mission = `Ataque Contra Misión Médica`,
    Confinement_or_Mobility_Restriction = `Confinamiento o Restricción a la Movilidad`,
    Forced_Displacement = `Desplazamiento Forzado`,
    Extortion = Extorsión,
    Civilian_Injuries = `Lesionados Civiles`,
    Looting = Pillaje,
    Torture = Tortura,
    Gender_Based_Violence = `Violencia Basada en Género`,
    Other_Simultaneous_Event = `Otro Hecho Simultáneo`,
    Total_Combatants = `Total Combatientes`,
    Total_Civilians = `Total Civiles`,
    Total_Civilians_and_Combatants = `Total Civiles y Combatientes`,
    Total_Case_Victims = `Total de Víctimas del Caso`,
    Weapons_Type = `Tipo de Armas`,
    Event_Type = `Tipo de Evento`,
    Total_Civilians_and_Combatants_Injured = `Total Civiles y Combatientes Heridos`,
    Latitude = Latitud,
    Longitude = Longitud
  )

cases_min_new <- cases_min %>%
  select(
    -Month,
    -Day,
    -Modality,
    -Alleged_Responsible,
    -Alleged_Responsible_Description,
    -Forced_Land_Dispossession,
    -Threat_or_Intimidation,
    -Attack_Against_Medical_Mission,
    -Confinement_or_Mobility_Restriction,
    -Forced_Displacement,
    -Extortion,
    -Civilian_Injuries,
    -Looting,
    -Torture,
    -Gender_Based_Violence,
    -Other_Simultaneous_Event,
    -Total_Combatants,
    -Total_Civilians,
    -Total_Civilians_and_Combatants,
    -Weapons_Type,
    -Event_Type,
    -Total_Civilians_and_Combatants_Injured,
    -Latitude,
    -Longitude
  )
sort(unique(cases_min_new$Year))

########
##SELECTIVE MURDER DATASET

colnames(cases_mur)
cases_mur <- cases_mur %>%
  rename(
    Case_ID = `ID Caso`,
    Related_Case_ID = `ID Caso Relacionado`,
    Year = Año,
    Month = Mes,
    Day = Día,
    Municipality_DANE_Code = `Código DANE de Municipio`,
    Municipality = Municipio,
    Department = Departamento,
    Region = Región,
    Modality = Modalidad,
    Alleged_Responsible = `Presunto Responsable`,
    Alleged_Responsible_Description = `Descripción Presunto Responsable`,
    Forced_Land_Dispossession = `Abandono o Despojo Forzado de Tierras`,
    Threat_or_Intimidation = `Amenaza o Intimidación`,
    Attack_Against_Medical_Mission = `Ataque Contra Misión Médica`,
    Confinement_or_Mobility_Restriction = `Confinamiento o Restricción a la Movilidad`,
    Forced_Displacement = `Desplazamiento Forzado`,
    Extortion = Extorsión,
    Torture = Tortura,
    Gender_Based_Violence = `Violencia Basada en Género`,
    Other_Simultaneous_Event = `Otro Hecho Simultáneo`,
    Graffiti_Signs    = `Grafitis Letreros` ,                     
    Family_Ties = `Vínculos Familiares` ,
    Pregnant_Women = `Mujeres Embarazadas`, 
    Total_Case_Victims = `Total de Víctimas del Caso`,
    Weapons_Type = `Tipo de Armas`,
    Latitude = Latitud,
    Longitude = Longitud
  )

cases_mur_new <- cases_mur %>%
  select(
    -Month,
    -Day,
    -Modality,
    -Alleged_Responsible,
    -Alleged_Responsible_Description,
    -Forced_Land_Dispossession,
    -Threat_or_Intimidation,
    -Attack_Against_Medical_Mission,
    -Confinement_or_Mobility_Restriction,
    -Forced_Displacement,
    -Extortion,
    -Torture,
    -Gender_Based_Violence,
    -Other_Simultaneous_Event,
    -Graffiti_Signs,                     
    -Family_Ties,
    -Pregnant_Women, 
    -Weapons_Type,
    -Latitude,
    -Longitude
  )

sort(unique(cases_mur_new$Year))



##############
##Sexual Violence

colnames(cases_sv)

cases_sv <- cases_sv %>%
  rename(
    Case_ID = `ID Caso`,
    Related_Case_ID = `ID Caso Relacionado`,
    Year = Año,
    Month = Mes,
    Day = Día,
    Municipality_DANE_Code = `Código DANE de Municipio`,
    Municipality = Municipio,
    Department = Departamento,
    Region = Región,
    Modality = Modalidad,
    Alleged_Responsible = `Presunto Responsable`,
    Alleged_Responsible_Description = `Descripción Presunto Responsable`,
    Forced_Land_Dispossession = `Abandono o Despojo Forzado de Tierras`,
    Threat_or_Intimidation = `Amenaza o Intimidación`,
    Attack_Against_Medical_Mission = `Ataque Contra Misión Médica`,
    Confinement_or_Mobility_Restriction = `Confinamiento o Restricción a la Movilidad`,
    Forced_Displacement = `Desplazamiento Forzado`,
    Extortion = Extorsión,
    Torture = Tortura,
    Civilian_Injuries = `Lesionados Civiles`,
    Looting = Pillaje,
    Gender_Based_Violence = `Violencia Basada en Género`,
    Other_Simultaneous_Event = `Otro Hecho Simultáneo`,
    Total_Case_Victims = `Total de Víctimas del Caso`,
    Weapons_Type = `Tipo de Armas`,
    Latitude = Latitud,
    Longitude = Longitud
  )

colnames(cases_sv)
cases_sv_new <- cases_sv %>%
  select(
    -Month,
    -Day,
    -Modality,
    -Alleged_Responsible,
    -Alleged_Responsible_Description,
    -Forced_Land_Dispossession,
    -Threat_or_Intimidation,
    -Attack_Against_Medical_Mission,
    -Confinement_or_Mobility_Restriction,
    -Forced_Displacement,
    -Extortion,
    -Torture,
    -Civilian_Injuries,
    -Looting,
    -Gender_Based_Violence,
    -Other_Simultaneous_Event,
    -Weapons_Type,
    -Latitude,
    -Longitude
  )

sort(unique(cases_sv_new$Year))


##############
##Terrorist attacks

colnames(cases_ter)

cases_ter <- cases_ter %>%
  rename(
    Case_ID = `ID Caso`,
    Related_Case_ID = `ID Caso Relacionado`,
    Year = Año,
    Month = Mes,
    Day = Día,
    Municipality_DANE_Code = `Código DANE de Municipio`,
    Municipality = Municipio,
    Department = Departamento,
    Region = Región,
    Modality = Modalidad,
    Alleged_Responsible = `Presunto Responsable`,
    Alleged_Responsible_Description = `Descripción Presunto Responsable`,
    Forced_Land_Dispossession = `Abandono o Despojo Forzado de Tierras`,
    Threat_or_Intimidation = `Amenaza o Intimidación`,
    Attack_Against_Medical_Mission = `Ataque Contra Misión Médica`,
    Confinement_or_Mobility_Restriction = `Confinamiento o Restricción a la Movilidad`,
    Forced_Displacement = `Desplazamiento Forzado`,
    Extortion = Extorsión,
    Torture = Tortura,
    Civilian_Injuries = `Lesionados Civiles`,
    Looting = Pillaje,
    Gender_Based_Violence = `Violencia Basada en Género`,
    Other_Simultaneous_Event = `Otro Hecho Simultáneo`,
    Total_Civilians =`Total Civiles`,
    Total_Case_Victims = `Total de Víctimas del Caso`,
    Weapons_Type = `Tipo de Armas`,
    Latitude = Latitud,
    Longitude = Longitud
  )

colnames(cases_ter)
cases_ter_new <- cases_ter %>%
  select(
    -Month,
    -Day,
    -Modality,
    -Alleged_Responsible,
    -Alleged_Responsible_Description,
    -Forced_Land_Dispossession,
    -Threat_or_Intimidation,
    -Attack_Against_Medical_Mission,
    -Confinement_or_Mobility_Restriction,
    -Forced_Displacement,
    -Extortion,
    -Torture,
    -Civilian_Injuries,
    -Looting,
    -Gender_Based_Violence,
    -Other_Simultaneous_Event,
    -Weapons_Type,
    -Total_Civilians,
    -`Total Combatientes`,
    -Latitude,
    -Longitude
  )

sort(unique(cases_sv_new$Year))


########
###War Actions
colnames(cases_war)

cases_war <- cases_war %>%
  rename(
    Case_ID = `ID Caso`,
    Related_Case_ID = `ID Caso Relacionado`,
    Year = Año,
    Month = Mes,
    Day = Día,
    Municipality_DANE_Code = `Código DANE de Municipio`,
    Municipality = Municipio,
    Department = Departamento,
    Region = Región,
    Modality = Modalidad,
    Initiative = Iniciativa,
    Attacked_Unit_Type = `Tipo de Unidad Atacada`,
    Armed_Group_1 = `Grupo Armado 1`,
    Armed_Group_1_Description = `Descripción Grupo Armado 1`,
    Armed_Group_2 = `Grupo Armado 2`,
    Armed_Group_2_Description = `Descripción Grupo Armado 2`,
    Armed_Group_3 = `Grupo Armado 3`,
    Armed_Group_3_Description = `Descripción Grupo Armado 3`,
    Civilian_Injuries = `Lesionados Civiles`,
    Captured_Group_Affiliation = `Grupo al que Pertenecen los Capturados`,
    Captured = Capturados,
    Combatant_Injuries = `Lesionados Combatientes`,
    Military = Militares,
    Police = Policías,
    Other_State_Armed_Forces = `Otras Fuerzas Armadas Estatales`,
    State_Agents_No_Info = `Agentes del Estado Sin Información`,
    Total_State_Agents = `Total Agentes del Estado`,
    Guerrillas = Guerrilleros,
    Paramilitaries = Paramilitares,
    Post_Demobilization_Groups = `Grupos Posdesmovilización`,
    Combatants_No_Info = `Combatientes Sin Información`,
    Other_Organized_Armed_Groups = `Otros Grupos Armados Organizados`,
    Total_Organized_Armed_Group_Combatants = `Total Combatientes de Grupos Armados Organizados`,
    Total_Combatants = `Total Combatientes`,
    Persons_No_Info = `Personas Sin Información`,
    Total_Civilians = `Total Civiles`,
    Military_Advantage = `Ventaja Militar`,
    Total_Case_Victims = `Total de Víctimas del Caso`,
    Latitude = Latitud,
    Longitude = Longitud
  )


colnames(cases_war)
    
cases_war_new <- cases_war %>%
      select(
        -Month,
        -Day,
        -Modality,
        -Initiative,
        -Attacked_Unit_Type,
        -Armed_Group_1,
        -Armed_Group_1_Description,
        -Armed_Group_2,
        -Armed_Group_2_Description,
        -Armed_Group_3,
        -Armed_Group_3_Description,
        -Civilian_Injuries,
       -Captured_Group_Affiliation,
        -Captured,
        -Combatant_Injuries,
        -Military,
        -Police,
        -Other_State_Armed_Forces,
        -State_Agents_No_Info,
        -Total_State_Agents,
        -Guerrillas,
        -Paramilitaries,
        -Post_Demobilization_Groups,
        -Combatants_No_Info,
        -Other_Organized_Armed_Groups,
        -Total_Organized_Armed_Group_Combatants,
        -Total_Combatants,
        -Persons_No_Info,
        -Total_Civilians,
        -Military_Advantage,
        -Latitude,
        -Longitude
      )
    

sort(unique(cases_war_new$Year))

########
###PROPERTY DAMAGE
colnames(cases_civ)

cases_civ <- cases_civ %>%
  rename(
    Case_ID = `ID Caso`,
    Related_Case_ID = `ID Caso Relacionado`,
    Year = Año,
    Month = Mes,
    Day = Día,
    Municipality_DANE_Code = `Código DANE de Municipio`,
    Municipality = Municipio,
    Department = Departamento,
    Region = Región,
    Modality = Modalidad,
    Alleged_Responsible = `Presunto Responsable`,
    Alleged_Responsible_Description = `Descripción Presunto Responsable`,
    Forced_Land_Dispossession = `Abandono o Despojo Forzado de Tierras`,
    Threat_or_Intimidation = `Amenaza o Intimidación`,
    Attack_Against_Medical_Mission = `Ataque Contra Misión Médica`,
    Confinement_or_Mobility_Restriction = `Confinamiento o Restricción a la Movilidad`,
    Forced_Displacement = `Desplazamiento Forzado`,
    Extortion = Extorsión,
    Civilian_Injuries = `Lesionados Civiles`,
    Looting = Pillaje,
    Torture = Tortura,
    Gender_Based_Violence = `Violencia Basada en Género`,
    Other_Simultaneous_Event = `Otro Hecho Simultáneo`,
    Total_Case_Victims = `Total de Víctimas del Caso`,
    Weapons_Type = `Tipo de Armas`,
    Cultural_Property_and_Places_of_Worship = `Bienes Culturales y Lugares de Culto`,
    Bank_Entity = `Entidad Bancaria`,
    Public_Entities = `Entidades Públicas`,
    Commercial_Establishment = `Establecimiento Comercial`,
    Communication_Infrastructure = `Infraestructura de Comunicaciones`,
    Educational_Infrastructure = `Infraestructura Educativa`,
    Medical_Infrastructure = `Infraestructura Médica`,
    Electric_Infrastructure = `Infraestructura Eléctrica`,
    Business_Infrastructure = `Infraestructura Empresarial`,
    Energy_Infrastructure = `Infraestructura Energética`,
    Road_Infrastructure = `Infraestructura Vial`,
    Electoral_Materials = `Material Electoral`,
    Transportation_Means = `Medios de Transporte`,
    Goods_or_Materials = `Mercancías u Objetos Materiales`,
    Rural_Property = `Propiedad Rural`,
    Union_Headquarters = `Sede de Gremio/Sindicato`,
    Political_Party_Headquarters = `Sede de Partido Político`,
    Urban_Housing = `Vivienda Urbana`,
    Other_Property_Type = `Otro Tipo de Bien`,
    Other_Property_Description = `Descripción Tipo de Bien`,
    Latitude = Latitud,
    Longitude = Longitud
  )

colnames(cases_civ)

cases_civ_new <- cases_civ %>%
  select(
    Case_ID,
    Related_Case_ID,
    Year,
    Municipality_DANE_Code,
    Municipality,
    Department,
    Region,
    Total_Case_Victims
  )

sort(unique(cases_civ_new$Year))


##########
##Recruitment and use of Children and Adolescents
colnames(cases_rec)

cases_rec <- cases_rec %>%
  rename(
    Case_ID = `ID Caso`,
    Related_Case_ID = `ID Caso Relacionado`,
    Year = Año,
    Month = Mes,
    Day = Día,
    Municipality_DANE_Code = `Código DANE de Municipio`,
    Municipality = Municipio,
    Department = Departamento,
    Region = Región,
    Modality = Modalidad,
    Alleged_Responsible = `Presunto Responsable`,
    Alleged_Responsible_Description = `Descripción Presunto Responsable`,
    Forced_Land_Dispossession = `Abandono o Despojo Forzado de Tierras`,
    Threat_or_Intimidation = `Amenaza o Intimidación`,
    Attack_Against_Medical_Mission = `Ataque Contra Misión Médica`,
    Confinement_or_Mobility_Restriction = `Confinamiento o Restricción a la Movilidad`,
    Forced_Displacement = `Desplazamiento Forzado`,
    Extortion = Extorsión,
    Civilian_Injuries = `Lesionados Civiles`,
    Looting = Pillaje,
    Torture = Tortura,
    Gender_Based_Violence = `Violencia Basada en Género`,
    Other_Simultaneous_Event = `Otro Hecho Simultáneo`,
    Total_Case_Victims = `Total de Víctimas del Caso`,
    Linkage_Form = `Forma de Vinculación`,
    Linkage_Type = `Tipo de Vinculación`,
    Latitude = Latitud,
    Longitude = Longitud
  )

cases_rec_new <- cases_rec %>%
  select(
    Case_ID,
    Related_Case_ID,
    Year,
    Municipality_DANE_Code,
    Municipality,
    Department,
    Region,
    Total_Case_Victims
  )
sort(unique(cases_rec_new$Year))
