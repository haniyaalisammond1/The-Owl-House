## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2023_11_16
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################
mean(data$Number_of_Q_Characters_Var_1)
sd(data$Number_of_Q_Characters_Var_1)
table(data$Number_of_Q_Characters_Var_1)
describe(data$Number_of_Q_Characters_Var_1)
summary(data$Number_of_Q_Characters_Var_1)

mean(data$Q_Rel_Crsh_Port_Var_2)
sd(data$Q_Rel_Crsh_Port_Var_2)
table(data$Q_Rel_Crsh_Port_Var_2)
describe(data$Q_Rel_Crsh_Port_Var_2)
summary(data$Q_Rel_Crsh_Port_Var_2)

table(data$Q_Character_Present_Var_1)

table(data$Q_Rel_Crsh_Ment_Var_2)

##################################################################################
####################  STEP 2: Table  2             ####################   
##################################################################################
table(data$Q_Character_Present_Var_1, data$Q_Rel_Crsh_Ment_Var_2)

##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################
chisq.test(table(data$Q_Character_Present_Var_1, data$Q_Rel_Crsh_Ment_Var_2))

##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################
anova <- aov(Number_of_Q_Characters_Var_1 ~ Q_Character_Present_Var_1, data = data)
summary(anova)
anova_adapted <- aov(Number_of_Q_Characters_Var_1 ~ Q_Character_Present_Var_1, data = data)
summary(anova_adapted)

##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################
cor(data$Number_of_Q_Characters_Var_1, data$Q_Rel_Crsh_Port_Var_2)

##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################
linear_relationship <- lm(Q_Rel_Crsh_Port_Var_2 ~ Number_of_Q_Characters_Var_1, data = data)
summary(linear_relationship)
##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################
plot(data$Number_of_Q_Characters_Var_1, data$Q_Rel_Crsh_Port_Var_2)
abline(linear_relationship, col = "red")
abline(h=0.5, col = "green")
abline(v = 2.7, col = "green")
##################################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################
plot(data$Number_of_Q_Characters_Var_1, residuals(linear_relationship))
abline(v = 2.7, col = "green")
mean(residuals(linear_relationship))
abline(h = -1.895932e-17, col = "green")
abline(linear_relationship, col = "red")