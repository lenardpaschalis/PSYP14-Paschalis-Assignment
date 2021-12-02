library(lme4)
library(lmerTest)
library(stats)
library(gridExtra) 
library(psych) 
library(tidyverse) 
library(ggplot2)
library(lm.beta)
library(influence.ME)
library(stats)
library(psych) 
library(tidyverse) 
library(r2glmm)
library(lme4) 
library(lmerTest)
library(cAIC4)
library(dplyr)



data_file3 <- read.csv("https://tinyurl.com/b385chpu")
data_file4 <- read.csv("https://tinyurl.com/4f8thztv")

data_file3 <- data_file3 %>% #in order to check the data set we need to mutate sex as a factor
  mutate(sex = factor(sex))
levels(data_file3$sex)

data_file3%>%
  describe()

data_file3%>%
  summary()

data_file3 %>% # investigate on the false value
  filter(sex == "woman")

data_file3=data_file3 %>%
  mutate(sex=recode(sex,
                    "woman"="female"))

data_file3%>% # income can not be minus; we dont work with it, so we wont remove it 
  filter(household_income < 0)
  
data_file3 %>%
  summary()

mod_rand_int <- lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = data_file3)



stdCoef.merMod <- function(object) {	
  sdy <- sd(getME(object,"y"))	
  sdx <- apply(getME(object,"X"), 2, sd)	
  sc <- fixef(object)*sdx/sdy	
  se.fixef <- coef(summary(object))[,"Std. Error"]	
  se <- se.fixef*sdx/sdy	
  return(data.frame(stdcoef=sc, stdse=se))	
}	

summary(mod_rand_int)
confint(mod_rand_int) #CI
stdCoef.merMod (mod_rand_int) 


summary(theory_based_model) 
coef_table(theory_based_model) 

r.squaredGLMM(mod_rand_int) 

# in the next step I use the regression equation for predicting pain within the new data set

data_file4 <- data_file4 %>% #in order to check the data set we need to mutate sex as a factor
  mutate(sex = factor(sex))

data_file4 %>%
  describe()+
  
data_file4 %>%
  summary()

pred_mod_rand_int <-  predict(mod_rand_int, data_file4, allow.new.levels = TRUE) #function

RSS <- sum((data_file4[,"pain"]- pred_mod_rand_int)^2)
RSS

mod_mean <- lm (pain ~ 1, data=data_file4)
TSS <- sum((data_file4$pain - predict(mod_mean))^2)
TSS


R2 <- 1 - (RSS/TSS)
R2


#new linear mixed model with only the most influential predictor.
mod_rand_slope <- lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = data_file3)

data_file3 = data_file3 %>% 		
  mutate(pred_slope = predict(mod_rand_slope))

data_file3 %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, group = hospital)+		
  geom_point(aes(color = hospital), size = 3) +		
  geom_line(color='red', aes(y=pred_slope, x=cortisol_serum))+		
  facet_wrap( ~ hospital, ncol = 2)


cAIC(mod_rand_int)$caic
cAIC(mod_rand_slope)$caic 
anova(mod_rand_int, mod_rand_slope) # the result of the anova does not effect the decision 

summary(model_3)

