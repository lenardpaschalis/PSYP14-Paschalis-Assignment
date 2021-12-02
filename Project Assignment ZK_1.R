library(psych)
library(sandwich) 
library(lmboot) 
library(tidyverse)
library(ggplot2)
library(car)
library(lm.beta)
library(lmtest)



coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}	

https://tinyurl.com/yxm5rd89

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

data_sample_1 = data_sample_1 %>%
  mutate(sex = factor(sex))
levels(data_sample_1$sex)

str(data_sample_1)
describe(data_sample_1)
summary(data_sample_1)

data_sample_1 %>% #Identify the outliers
  filter(pain > 10) 

data_sample_1 %>% 
  filter(STAI_trait < 20) #Identify the outliers

data_sample_2 <- data_sample_1 %>% #Here we removed the participants that have false values
  slice(-c(34, 88))

describe(data_sample_2)

model_1 <- lm(pain ~ age + sex, data = data_sample_2)
model_2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_sample_2)
model_3 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_2) #model 3 was added later, as model 2 violated assumptions.
summary(model_1)$adj.r.squared
summary(model_2)$adj.r.squared
summary(model_3)$adj.r.squared

#model diagnosis for both models:
model_1 %>%
  plot(which = 3)

model_1 %>%
  residualPlots() 

model_1 %>%
  ncvTest() 

model_1 %>%
  bptest() 

model_1 %>%
  vif()

model_1 %>%
  plot(which = 4)

model_2 %>%
  plot(which=3) 

model_2 %>%
  ncvTest() 

model_2 %>%
  bptest() 

model_2 %>%
  residualPlots() 

model_2 %>%
  vif() # cortisols are above 3, which is not good. 

#Now we run the model diagnosis for model 3 again, which only contains cortisol_serum and saliva
model_3 %>%
  plot(which = 3)

model_3 %>%
  ncvTest() 

model_3 %>%
  bptest() 

model_3 %>%
  vif()

model_3 %>%
  plot(which = 4)

model_3 %>%
  vif() #If we take out the cortisol saliva, all values are compliant, as expected. Therefore we will replace the old model.

model_3%>%
  residualPlots() 

plot(model_1, 4)
plot(model_3, 4) # even though we have outliers, we will not "slice" them out of the data set, since the function will always show new outliers that could possibly sliced out.

AIC(model_1)
AIC(model_3)

anova(model_1, model_3) # AIC is more established, but we stil run ANOVA

coef_table(model_1) #the higher the age, the lower pain is being experienced
lm.beta(model_1)
coef_table(model_2)
lm.beta(model_2)
coef_table(model_3)
lm.beta(model_3)


describe(residuals(model_1))
describe(residuals(model_3)) #seem to be normally distributed, not gonna use other testing methods such as "smirnov" since the sample size (n=158) is too big for these procedures

 
summary(model_1)
summary(model_3)


anova(model_1, model_3)
summary(model_3)$adj.r.squared

AIC(model_1)
AIC(model_3)

model_1
model_3


summary(model_1)$adj.r.squared
summary(model_3)$adj.r.squared

lm(model_1)
lm(model_3)

summary(model_1)
summary(model_3)
