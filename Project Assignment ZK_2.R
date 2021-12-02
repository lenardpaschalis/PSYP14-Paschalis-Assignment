https://tinyurl.com/yxm5rd89

data_sample_assignment2  = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_1.csv")

library(gridExtra) 
library(psych) 
library(tidyverse) 
library(ggplot2)
library(lm.beta)
library(car)
library(lmtest)

data_sample_assignment2=data_sample_assignment2 %>% #Here we removed the participants that have false values
  slice(-c(34, 88))

data_sample_assignment2=data_sample_assignment2 %>%
  mutate(sex = factor(sex))
levels(data_sample_1$sex)

data_sample_assignment2
  
describe(data_sample_assignment2$IQ)

data_sample_assignment2%>%
  ggplot() + aes(x = ID, y = IQ) + geom_point() # Since we dont know about the sample and its structure, I do not exclude participants with unusual low and high IQ values.

describe(data_sample_assignment2$household_income)

data_sample_assignment2%>%
  ggplot() + aes(x = ID, y = household_income) + geom_point()

#now we create a variable that include all predictors (initial model)
initial_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_sample_assignment2)

#check the skewness and curtosis
describe(residuals(initial_model))

#cooks distance
plot(initial_model, 4) 

coef_table(initial_model) 
lm.beta(initial_model)

#linearity (of the relationship), 

initial_model %>%
  residualPlots() 


initial_model %>%
  plot(which = 3)

initial_model %>%
  ncvTest() 

initial_model %>%
  bptest() 

initial_model %>%
  plot(which=3) 

initial_model %>%
  vif() #multiocollinearity

backward_model = step(initial_model, direction = "backward") 

backward_model %>%
  residualPlots() 

#now I run the diagnosis again

backward_model %>%
  plot(which = 3)

backward_model %>%
  ncvTest() 

backward_model %>%
  bptest() 

backward_model %>%
  plot(which=3) 

backward_model %>%
  vif()

plot(backward_model, 4) #no values will be excluded
cooks.distance(backward_model)

backward_model <- lm(pain ~ age + pain_cat + mindfulness + cortisol_serum, data = data_sample_assignment2)

theory_based_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_assignment2)

summary(backward_model)
summary(initial_model)
summary(backward_model)

anova(backward_model, theory_based_model) # we dont base our decision on that ANOVA



AIC(backward_model)
AIC(initial_model) 
AIC(theory_based_model)

coef_table(backward_model)



# new data
data_sample_assignment2_2 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_2.csv")


data_sample_assignment2_2 %>%
  mutate(sex = factor(sex))
levels(data_sample_assignment2_2$sex)
#checking if our data set shows errors/outliers
describe(data_sample_assignment2_2)
summary(data_sample_assignment2_2)
#everything seems okay. except  IQ. We dont exclude anything
# now I want to check if the two models can be applied to the new data set in terms of their validity
pred_theory_model <- predict(theory_based_model, data_sample_assignment2_2, allow.new.levels=TRUE)
pred_backwards_model <- predict(backward_model, data_sample_assignment2_2, allow.new.levels = TRUE)


Squared_residuals_model <- sum((data_sample_assignment2_2[,"pain"]-pred_theory_model)^2)
Squared_residuals_model

Squared_residuals_model <- sum((data_sample_assignment2_2[,"pain"]-pred_backwards_model)^2)
Squared_residuals_model

backward_model



