library(tidyverse)
library(gtsummary)
library(easystats)
library(gt)

# read data
data <- readxl::read_excel("raw_data/AMR_KAP_Data.xlsx", sheet = 1)



##Demographic characteristics of study participants
data |>
  select(1:11) |>
  tbl_summary() |>
  as_gt() |>
  gtsave("Tables/Table1_Demographics.docx")
  
#read data sheet2
data2 <- readxl::read_excel("raw_data/AMR_KAP_Data.xlsx", sheet = 3)

#Impact of education on level of knowledge of antibiotics
names(data2)
y = mx + c

model <- lm(TotalKnowledgeScore ~ `Parent’s education level`, data = data2)

summary(model)
report(model)

##Major sources of information about antibiotic parents

# Load required libraries
library(readxl)     
library(dplyr)      
library(gt)
library(tidyverse)
library(gtsummary)


# Read data
data <- readxl::read_excel("raw_data/AMR_KAP_Data.xlsx", sheet = 1)

names(data)
data |>
  select(40:49) |>
  tbl_summary() |>
  as_gt() |>
  gtsave("Tables/Table2_MajorSources.docx")

##Factors associated with level of knowledge

# load packages
library(tidyverse)
library(easystats)
library(gtsummary)
library(gt)


# import data 
data2 <- readxl::read_excel("raw_data/AMR_KAP_Data.xlsx", sheet = 3)
names(data2)

# Build the multivariate logistic regression model
mv_model<- glm(`TotalKnowledgeScore` ~ `Parent’s age (years)` + `Parent’s sex` + `Parent’s education level` + `Employment status` + 
                  `Family type` + `Your average household income per month (BDT)` + `Child’s sex` + `Child’s age (years)` + `Number of children`, data = data2)

summary(mv_model)
report(mv_model)
# tbl_regression 
mv_model |> 
  tbl_regression() |> 
  bold_p(t = 0.05) |> 
  as_gt() |> 
  gtsave("tables/Table4_Factors_Level_of_Knowledge.docx")

##Factors associated with level of attitude


# Build the multivariate logistic regression model
mv_model<- glm(`TotalAttitudeScore` ~ `Parent’s age (years)` + `Parent’s sex` + `Parent’s education level` + `Employment status` + 
                 `Family type` + `Your average household income per month (BDT)` + `Child’s sex` + `Child’s age (years)` + `Number of children`, data = data2)

summary(mv_model)
report(mv_model)
# tbl_regression 
mv_model |> 
  tbl_regression() |> 
  bold_p(t = 0.05) |> 
  as_gt() |> 
  gtsave("tables/Table5_Factors_Level_of_Attitude.docx")

##Factors associated with level of pratice

# Build the multivariate logistic regression model

mv_model<- glm(`TotalAttitudeScore` ~ `Parent’s age (years)` + `Parent’s sex` + `Parent’s education level` + `Employment status` + 
                 `Family type` + `Your average household income per month (BDT)` + `Child’s sex` + `Child’s age (years)` + `Number of children`, data = data2)


# Summary of the model
summary(mv_model)
report(mv_model)
# tbl_regression 
mv_model |> 
  tbl_regression() |> 
  bold_p(t = 0.05) |> 
  as_gt() |> 
  gtsave("tables/Table6_Factors_Level_of_Practice.docx")



  



