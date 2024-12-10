install.packages("tidyverse")
install.packages("ggplot2")
install.packages("texreg")
install.packages("utils")
install.packages("dplyr")
install.packages("car")
install.packages("sandwich")
install.packages("lmtest")
install.packages("clubSandwich")
install.packages("xtable")
install.packages("stargazer") 
install.packages("plotly")
install.packages("IRdisplay")
install.packages("ggstatsplot")
install.packages("vtable")
install.packages("reshape2")
install.packages("corrplot")
install.packages("coefplot")

library(ggstatsplot)
library(tidyverse)
library(ggplot2)
library(xtable)
library(texreg)
library(utils)
library(dplyr)
library(car)
library(lmtest)
library(clubSandwich)
library(sandwich)
library(stargazer)
library(plotly)
library(IRdisplay)
library(vtable)
library(reshape2)
library(corrplot)
library(coefplot)




### DATASET ### 

# upload full dataset precedently built on excel

# create vector for each variable 
opioids_deaths <- full_data$opioids_deaths
imfs_deaths <- full_data$imfs_deaths
rxopioids_deaths <- full_data$rxopioids_deaths
gini_coeff <- full_data$gini_coeff
gini_coeff_100 <- full_data$gini_coeff_100 
life_expectancy <- full_data$life_expectancy_2020
school_enrollment_percent <- full_data$school_enrollment_percent
unemployment_rate <- full_data$unemployment_rate
median_household_income <- full_data$median_household_income



# construct a new data frame with the variables of interest 
mydata <- data.frame(opioids_deaths,imfs_deaths,rxopioids_deaths,
                     gini_coeff_100,life_expectancy,school_enrollment_percent,
                     unemployment_rate,median_household_income)



### DESCRIPTIVE STATISCTICS ###

#  create a table for the appendix
summary(mydata)


# create boxplots for each variables

mydata_long <- reshape2::melt(mydata, variable.name = "variable", 
                              value.name = "value")


ggplot(mydata_long, aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(x = "Variable", y = "Value") +
  facet_wrap(~variable, scales = "free")







#### STATITICAL ANALYSIS ####

# model for drug overdose deaths involving any opioids

model1 <- lm(opioids_deaths ~ 
               gini_coeff_100 
             + life_expectancy  
             + school_enrollment_percent 
             + unemployment_rate
             + median_household_income 
             , data = mydata)

summary(model1)




# control assumptions

par(mfrow=c(2,2))

plot(model1)


# model for drug overdose deaths involving prescription opioids

model2 <- lm(rxopioids_deaths ~ 
               gini_coeff_100 
             + life_expectancy  
             + school_enrollment_percent 
             + unemployment_rate
             + median_household_income 
             , data = mydata)

summary(model2)


# control assumptions 
par(mfrow=c(2,2))

plot(model2)


# model for drug overdose deaths involving illicitly manufactured fentanyls

model3 <- lm(imfs_deaths ~ 
               gini_coeff_100 
             + life_expectancy  
             + school_enrollment_percent 
             + unemployment_rate
             + median_household_income 
             , data = mydata)

summary(model3)


# control assumptions 
par(mfrow=c(2,2))

plot(model3)


# generate a table for latex

stargazer(model1, model2, model3)



#### log models ###

# model for drug overdose deaths involving any opioids

log_model1 <- lm(log(opioids_deaths) ~ 
               gini_coeff_100 
             + life_expectancy  
             + school_enrollment_percent 
             + unemployment_rate
             + median_household_income 
             , data = mydata)

summary(model1_log)




# control assumptions

par(mfrow=c(2,2))

plot(log_model1)


# model for drug overdose deaths involving prescription opioids

log_model2 <- lm(log(rxopioids_deaths) ~ 
               gini_coeff_100 
             + life_expectancy  
             + school_enrollment_percent 
             + unemployment_rate
             + median_household_income 
             , data = mydata)

summary(log_model2)


# control assumptions 
par(mfrow=c(2,2))

plot(log_model2)


# model for drug overdose deaths involving illicitly manufactured fentanyls

log_model3 <- lm(log(imfs_deaths) ~ 
               gini_coeff_100 
             + life_expectancy  
             + school_enrollment_percent 
             + unemployment_rate
             + median_household_income 
             , data = mydata)

summary(log_model3)


# control assumptions 
par(mfrow=c(2,2))

plot(log_model3)


# generate a table for latex

stargazer(log_model1, log_model2, log_model3)









## APPENDIX ##

# table of the descriptive statisitcs 
summary(mydata)



### coefficient plot ###


#log_model1


log_coef_df <- data.frame(variable = names(log_model1$coefficients)[-1], 
                          estimate = log_model1$coefficients[-1], 
                          se = summary(log_model1)$coefficients[-1,2])

library(ggplot2)
ggplot(log_coef_df, aes(x = variable, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - se, ymax = estimate + se), width = 0.2) +
  coord_flip() +
  labs(x = "Variable", y = "Coefficient") +
  theme_bw()


