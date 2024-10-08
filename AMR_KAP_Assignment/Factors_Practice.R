# load packages
library(tidyverse)
library(easystats)
library(gtsummary)
library(gt)

##Factors Affecting Level of Practice 

# import data 
data3 <- readxl::read_excel("raw_data/AMR_KAP_Updated.xlsx", sheet = 1)
colnames(data3)

# Build the multivariate logistic regression model 

mv_model <- glm(`Practice_Level` ~ `Parent’s age (years)` + `Parent’s sex` + `Parent’s education level` + 
                  `Employment status` + `Family type` + `Your average household income per month (BDT)` + 
                  `Child’s sex` + `Child’s age (years)` + `Number of children`, family = binomial(link = "logit"), data = data3)

summary(mv_model)
report(mv_model)

# tbl_regression 
mv_model |> 
  tbl_regression(exponentiate = T) |> 
  bold_p(t = 0.05) |> 
  as_gt() |> 
  gtsave("Tables/Table6_Factors_Practice.docx")
