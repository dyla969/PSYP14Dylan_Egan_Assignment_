#PACKAGES FOR ASSIGNMENT####
library(cAIC4)
library(car)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lme4)
library(lm.beta)
library(lmerTest)
library(lmtest)
library(MuMIn)
library(psych)
library(sandwich)
library(sjlabelled)
library(sjmisc)
library(sjPlot)
library(tidyverse)#Installing packages
#ASSIGNMENT_1####
data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")#read data.frame
view(data_sample_1)#view data frame
summary(data_sample_1)#summary of data for out-liers
data_sample_correct <- data_sample_1 %>% 
  mutate(age = replace(age, age=="444", NA), STAI_trait = replace(STAI_trait, STAI_trait=="3.9", NA))#recode incorrectly entered values to NA
data_sample_corrected <- na.omit(data_sample_correct)
summary(data_sample_corrected)#Check corrected data 
model_age_sex = lm(pain ~ age + sex, data = data_sample_corrected)#Build a multiple linear regression with age and sex as predictors, and pain as outcome
model_psycholgical_hormonal = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_sample_corrected)#build a multiple linear regression with pain as outcome and age, sex, STAI trait, pain cat, mindfulness, cortisol serum, and cortisol saliva as predictors.
summary(model_psycholgical_hormonal)#Summary of model
data_graph = data_sample_corrected%>% 
  mutate(rownum = row.names(data_sample_corrected))#Creating shortcut for rownum
plot <- data_graph%>% 
  ggplot()+
  aes(x= pain, label = rownum)+
  geom_point()+
  geom_text()+
  geom_smooth(method = "lm")+
  labs (x= "Perioperative pain")#creating shortcut for making graphs (x axis = pain, no y axis(allows for reuse)
plot + aes(y = age)+ labs(y = "Age")#Graph y axis = Age
plot + aes(y = sex)+ labs(y = "Sex")#Graph y axis = Sex
plot + aes(y = STAI_trait)+ labs(y= "STAI")#Graph y axis = STAI
plot + aes(y = pain_cat) + labs (y= "Pain catastrophizing") #Graph y axis = Pain cat 
plot + aes(y = mindfulness) + labs (y= "Mindfulness")#Graph y axis = Mindfulness
plot + aes(y = cortisol_serum) + labs(y= "Cortisol serum")#Graph y axis = Corsitol serum
plot + aes(y = cortisol_saliva)+ labs(y= "Cortisol saliva")#Graph y axis = Cortisol saliva
model_psycholgical_hormonal %>% 
  vif()#Checking vif (Cortisols are above 3)
data_sample_corrected %>% 
  select(age, sex, STAI_trait, pain_cat, mindfulness, cortisol_serum, cortisol_saliva) %>% 
  pairs.panels(col = "red", lm = T)#looking at vif relation (cortisols related) 
model_p_h_drop = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_corrected)#Dropping cortisol saliva from model
summary(model_p_h_drop)#Summary of new model 
model_p_h_drop %>% 
  plot(which = 5)#Checking residuals 
model_p_h_drop %>% 
  plot(which = 4)#Checking Cook´s distance (All below .1)
data_sample_corrected %>%  
  slice(c(96, 100, 114))#Viewing cases identified (114 has low but valid cortisol serum, )
model_p_h_drop %>%  
  plot(which = 2) #QQ for normal distribution
residuals_model_p_h_drop = enframe(residuals(model_p_h_drop))#Preparing data for histogram
residuals_model_p_h_drop %>% 
  ggplot() + 
  aes(x = value) +
  geom_histogram()+
  labs(x ="Value", y= "Count")#Histogram for normal distribution
describe(residuals(model_p_h_drop))#Checking skew and kurtosis
model_p_h_drop %>%  
  plot(which = 3) #Checking homoscedasticity 
model_p_h_drop %>% 
  ncvTest()#Checking Non-constant variance score (Homoscedasticity)
model_p_h_drop %>% 
  bptest()#Checking Breusch-Pagan test
model_p_h_drop %>% 
  vif()#Checking vif
model_p_h_drop_homosc =  coeftest(model_p_h_drop, vcov = vcovHC, type = "HC")
model_p_h_drop_homosc #Viewing model
summary(model_p_h_drop_homosc)#Summary of model 
AIC(model_p_h_drop)#Model fit
summary(model_p_h_drop)$adj.r.squared #Adjusted R squared value 
coef(model_p_h_drop_homosc) #Coefficients 
confint(model_p_h_drop_homosc)#Confidence intervals 
summary(model_age_sex)$adj.r.squared #Adjusted R squared value 
coef(model_age_sex)#Coefficients 
confint(model_age_sex)#Confidence intervals
AIC(model_age_sex)#Model fit
anova(model_p_h_drop, model_age_sex)#Analysis of variance 
summary(model_age_sex)
summary(model_all)


#ASSIGNMENT_2#### 
model_all = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_sample_corrected)
model_all %>% 
  plot(which = 5) #checking residuals vs leverage in model_2. No need do this for model_1 as the predictors within model_1 are nested within model_2
model_all %>% 
  plot(which = 4)#checking Cook´s distance of model 4
 data_sample_corrected %>%  
  slice(c(3, 26, 96))#viewing cases identified by Cook´s distance. (All are valid data, so we leave them in the model, and are below 0.1.)
model_all %>%  
  plot(which = 2)# QQ checking for normal distribution in model 4 (slight diversion)
residuals_model_all = enframe(residuals(model_all))
residuals_model_all %>% 
  ggplot() + 
  aes(x=value)+
  geom_histogram()+
  labs (x= "Value", y= "Count") #histogram checking for normal distribution (histogram is bell shaped)
describe(residuals(model_all))#checking for skew and kurtosis (skew and kurtosis are between -0.5, and 0.5 so fairly symmetrical )
model_all %>%  
  plot(which = 3) #checking for homoscedasticty (line is curved, may indicate existence of heteroscedasticity)
model_all %>% 
  ncvTest()#Checking for homoscedasticty (p value less than 0.05 (p < .07), heteroscedasticity may not exist )
model_all %>% 
  bptest()#Checking for homoscedasticty (p greater than 0.05 (p<.2, heteroscadasticity may not exist))
model_all %>% 
  vif()
plot + aes(y = weight)+ labs(y = "Weight")#Creating graph where y axis = weight
plot + aes(y = IQ) + labs(y= "IQ") #Creating graph where y axis = IQ 
plot + aes(y = household_income) + labs(y= "Household income")#Creating graph where y axis = household income 
data_sample_corrected %>%  
  slice(c(3, 10, 36, 47))#viewing cases with potential extreme cases with high leverage
backward_model = step(model_all, direction = "backward")#Creating backward linear regression 
summary(backward_model)#viewing backward regression result 
coef(backward_model)#Coefficients 
confint(backward_model)#Confidence intervals
theory_based_model <- model_p_h_drop #Changing name 
coef(theory_based_model)#Coefficients 
confint(theory_based_model)#Confidence intervals
AIC(model_all)#model fit
AIC(backward_model) #Model fit
AIC(theory_based_model)#Model fit
data_sample_2 = read.csv("https://tinyurl.com/ha-dataset2")#loading data_sample_2
summary(data_sample_2)#summary of data_sample_2 (checking for errors
pred_test_backward <- predict(backward_model, data_sample_2)#Fitting model on new sample 
pred_test_theory <- predict(theory_based_model, data_sample_2)#Fitting model on new sample 
(RSS_test_backward = sum((data_sample_2[, "pain"]-pred_test_backward)^2))#Calculating RSS for backward model
(RSS_test_theory <- sum((data_sample_2[, "pain"]- pred_test_theory)^2))#calculating RSS for theory model 
summary(backward_model) #Summary of model 
summary(theory_based_model)#Summary of model 


#ASSIGNMENT_3######
data_sample_3 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_3.csv")
summary(data_sample_3)#Summary
e_model <- lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = data_sample_3)#Creating linear mixed model 
summary(e_model)#Summary
(d_model <- lmer(pain ~ cortisol_serum +(cortisol_serum|hospital), data = data_sample_3))
cAIC(e_model)
cAIC(d_model)#Model fit 
r.squaredGLMM(e_model)
r.squaredGLMM(d_model)#Conditional/Marginal r^2
data_sample_4 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_4.csv")
summary(data_sample_4)#Summary
view(data_sample_4)#View
pred_e <- predict(e_model, data_sample_4, allow.new.levels = TRUE)#Testing predictive power
(RSS_test_e = sum((data_sample_4$pain - pred_e)^2))#Testing sum of square residuals
mod_mean <- lm(pain ~ 1, data = data_sample_4)
(TSS = sum ((data_sample_3$pain - predict(mod_mean))^2))
(1-(RSS_test_e/TSS))
(slope = data_sample_3 %>%  
  ggplot()+
  aes(y = pain, x = cortisol_serum, color = hospital)+
  geom_point(size = 4)+
  geom_smooth(method = "lm", se = F, fullrange = TRUE)+
  xlim(-1, 50)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0))#Chart predictor Cortisol_serum (clustered) 
mod_rnd_int <- lmer(pain~ cortisol_serum + (1|hospital), data=data_sample_3)
data_sample_3 <- data_sample_3 %>% 
  mutate(pred_int= predict(mod_rnd_int), pred_slope = predict(d_model))
data_sample_3 %>% 
  ggplot()+
  aes(y = pain, x = cortisol_serum, group = hospital)+
  geom_point(aes(color = hospital), size= 4)+
  geom_line(color="red", aes(y=pred_int, x = cortisol_serum))+
  facet_wrap(~hospital, ncol = 2)#Random intercept model
data_sample_3 %>% 
  ggplot()+
  aes(y=pain, x=cortisol_serum, group = hospital)+
  geom_point(aes(color = hospital), size = 4)+
  geom_line(color="red", aes(y=pred_slope, x = cortisol_serum))+
  facet_wrap(~hospital, ncol= 2)#random slope model
View(data_sample_3)
sum(residuals(e_model)^2)
sum(residuals(d_model)^2)

#APA GRAPHS####
(as_plot <- tab_model (model_age_sex, show.est = TRUE, show.std = TRUE, show.p = TRUE))#Age_sex model
(ph_plot <- tab_model (model_psycholgical_hormonal, show.est = TRUE, show.std = TRUE, show.p = TRUE))#Psychological_hormonal model 
(drop_plot <- tab_model (model_p_h_drop, show.est = TRUE, show.std = TRUE, show.p = TRUE))#Psychological_hormonal_drop_model
(theory_plot <- tab_model(theory_based_model, show.est = TRUE, show.std = TRUE, show.p = TRUE))
(backwards_plot <- tab_model(backward_model, show.est = TRUE, show.std = TRUE, show.p = TRUE))
(all_plot <- tab_model(model_all, show.est = TRUE, show.std = TRUE, show.p = TRUE))
(e_plot <- tab_model(e_model, show.est = TRUE, show.std = TRUE, show.p = TRUE))
(dplot<- tab_model(d_model, show.est = TRUE, show.std = TRUE, show.p = TRUE))
summary(model_p_h_drop)
