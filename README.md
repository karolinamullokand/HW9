# HW9

Creating usual environment

```
library(ggplot2)
library(tidyverse)
library(haven)

setwd("/Users/karolinamullokand/Desktop/ECO_B2000")
load("ACS_2021_couples.RData")
```


```
trad_data <- acs2021_couples %>% filter( (SEX == "Female") & (h_sex == "Male"))
trad_data$he_more_than_5yrs_than_her <- as.numeric(trad_data$age_diff < -5)
                                         
ols_out1 <- lm(he_more_than_5yrs_than_her ~ educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = trad_data)

pred_vals_ols1 <- predict(ols_out1, trad_data)
pred_model_ols1 <- (pred_vals_ols1 > mean(pred_vals_ols1))
table(pred = pred_model_ols1, true = trad_data$he_more_than_5yrs_than_her)

model_logit1 <- glm(he_more_than_5yrs_than_her ~ educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = trad_data, family = binomial)
summary(model_logit1)

pred_vals <- predict(model_logit1, trad_data, type = "response")
pred_model_logit1 <- (pred_vals > mean(pred_vals))
table(pred = pred_model_logit1, true = trad_data$he_more_than_5yrs_than_her)

# 1
ols_out2 <- lm(he_more_than_5yrs_than_her ~ EDUC + h_educ + POVERTY, data = trad_data)
pred_vals_ols2 <- predict(ols_out2, trad_data)
pred_model_ols2 <- (pred_vals_ols2 > mean(pred_vals_ols2))
table(pred = pred_model_ols2, true = trad_data$he_more_than_5yrs_than_her)
Result: The model is capturing both true positives and true negatives but has a 
high false positive rate, which impacts the models accuracy.
```

```
# 2
ols_out3 <- lm(he_more_than_5yrs_than_her ~ POVERTY + CITIZEN + YRSUSA1, 
               data = trad_data)
summary(ols_out3)
#Logit model with the same variables
model_logit <- glm(he_more_than_5yrs_than_her ~ POVERTY + CITIZEN + YRSUSA1, 
                   data = trad_data, family = binomial)
summary(model_logit)
pred_vals_logit <- predict(model_logit, trad_data, type = "response")


# cutoff 0.5
pred_model_logit <- (pred_vals_logit > 0.5)
table(pred = pred_model_logit, true = trad_data$he_more_than_5yrs_than_her)

Result: Since there are no rows for "Predicted Yes," it means that, with 
the 0.5 cutoff, the model is predicting no for almost all cases, which results 
in high false negatives and no true positives

# cutoff 0.3
pred_model_logit <- (pred_vals_logit > 0.3)
table(pred = pred_model_logit, true = trad_data$he_more_than_5yrs_than_her)

Result: Lowering the cutoff increased both true positives and false positives.

# cutoff 0.2
pred_model_logit <- (pred_vals_logit > 0.2)
table(pred = pred_model_logit, true = trad_data$he_more_than_5yrs_than_her)
Result: Using a 0.2 cutoff allows the model to capture more cases with a large age 
gap (true positives) but also results in more false positives.

# cutoff 0.25
pred_model_logit <- (pred_vals_logit > 0.25)
table(pred = pred_model_logit, true = trad_data$he_more_than_5yrs_than_her)

#By adjusting the cutoff, I balanced true positives and false positives, aiming 
for the best prediction accuracy.
```

# LEAVE OUT VARIABLES:

```
ols_out_reduced <- lm(he_more_than_5yrs_than_her ~ CITIZEN + YRSUSA1, data = trad_data)
summary(ols_out_reduced)


model_logit_reduced <- glm(he_more_than_5yrs_than_her ~ CITIZEN + YRSUSA1, 
                           data = trad_data, family = binomial)
summary(model_logit_reduced)


pred_vals_logit_reduced <- predict(model_logit_reduced, trad_data, type = "response")

# cutoff 0.25
pred_model_logit_reduced <- (pred_vals_logit_reduced > 0.25)
table(pred = pred_model_logit_reduced, true = trad_data$he_more_than_5yrs_than_her)

Result: Including 'poverty' adds a small increase in the models ability to predict the age gap.
```
