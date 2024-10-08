#load packages
library(tidyverse)
library(gtsummary)
library(easystats)
library(gt)
library(dplyr)
library(naniar)

##read data
data2 <- readxl::read_excel("raw_data/AMR_KAP_Data.xlsx", sheet = 3)
names(data2)
colnames(data2)
##Knowledge Level Grouping

data2 <- data2 |>
  mutate(Knowledge_Level = case_when(
    KnowledgePCT < 49 ~ "Poor",                  
    KnowledgePCT >= 50 & KnowledgePCT <= 79 ~ "Moderate",  
    KnowledgePCT >= 80 & KnowledgePCT <= 100 ~ "Good",      
    TRUE ~ NA_character_  
  ))

##Attitude Level Grouping

data2 <- data2 |>
  mutate(Attitude_Level = case_when(
    AttitudePCT < 49 ~ "Negative",                  
    AttitudePCT >= 50 & AttitudePCT <= 79 ~ "Uncertain",  
    AttitudePCT >= 80 & AttitudePCT <= 100 ~ "Postive",      
    TRUE ~ NA_character_  
  ))

##Practice Level Grouping

data2 <- data2 |>
  mutate(Practice_Level = case_when(
    PracticePCT < 79 ~ "Misuse",                  
    PracticePCT >= 80 & PracticePCT <= 100 ~ "Good",      
    TRUE ~ NA_character_  
  ))


#check and remove missing values

sum(is.na(data2$Knowledge_Level))
sum(is.na(data2$Attitude_Level))
sum(is.na(data2$Practice_Level))


#export the new data set
library(writexl)
write_xlsx(data2, "raw_data/AMR_KAP_Updated.xlsx")



##Level of KAP Summary characteristics of study participants

#reload necessary packages

library(tidyverse)
library(gtsummary)
library(easystats)
library(gt)

# read data
colnames(data3)

data3 <- readxl::read_excel("raw_data/AMR_KAP_Updated.xlsx", sheet = 1)
 data3 |>
   select(Knowledge_Level, Attitude_Level, Practice_Level) |>
   tbl_summary() |>
   as_gt() |>
   gtsave("Tables/Table3_KAP_Levels.docx")
 
   
 
 

  
  
