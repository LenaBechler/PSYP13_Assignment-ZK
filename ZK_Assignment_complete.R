data_sample_1=read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_1.csv")

#STAI_trait: scale 20 to 80, higher scores = higher anxiety, correlates positevely with level of pain experienced, "trait anxiety" 
#pain_cat: scale 0 to 52, higher scores = higher catastrophizing, predictor of clinical pain, "tendency to magnify the threat value of a pain stimulus and to feel helpless in the presence of pain"
#mindfulness: scale 1 to 6, higher scores = higher dispositional mindfulness, "protective factor against pain"
#cortisol_serum: stress hormone, positively correlated to pain experience, more reliably related to stress than saliva
#cortisol_saliva: stress hormone, positively correlated to pain experience

#loading necessary packages
library(psych) # for describe
library(lm.beta) # for lm.beta
library(dplyr)
library(stats)
library(lsr)
library(lmtest)
library(gsheet)
library(car) # for scatter3d, residualPlots, vif, pairs.panels, ncvTest
library(ggplot2) # for ggplot
library(rgl) # for scatter3d


####-----------ASSIGNMENT 1----------###
########################################

#EXPLORING DATA
#descriptive data e.g. check data for coding errors      
who(data_sample_1) 
summary(data_sample_1)
describe(data_sample_1) #check for correct ranges of each scale, sex and mindfulness!

data_sample_2=data_sample_1 #copy of data set
data_sample_2=data_sample_2[!data_sample_2$sex==3,] #remove ID with incorrect coding

boxplot(data_sample_1$mindfulness) 
stem(data_sample_1$mindfulness) 
data_sample_2=data_sample_2[!data_sample_2$mindfulness<1,] #remove IDs mindfulness < 1

summary(data_sample_2)
describe(data_sample_2)
cor(data_sample_2[,c("pain", "age", "weight", "STAI_trait", "pain_cat", "mindfulness", "cortisol_serum", "cortisol_saliva")])


#Histograms: look at distribution
par(mfrow=c(3,3))
hist(data_sample_2$pain, breaks = 30)
hist(data_sample_2$age, breaks = 30)
hist(data_sample_2$STAI_trait, breaks = 30)
hist(data_sample_2$pain_cat, breaks = 30)
hist(data_sample_2$cortisol_serum, breaks = 30)
hist(data_sample_2$cortisol_saliva, breaks = 30)
hist(data_sample_2$mindfulness, breaks = 30)
hist(data_sample_2$weight, breaks= 30)
par(mfrow=c(1,1))

#Boxplots
par(mfrow=c(3,3))
boxplot(data_sample_2$pain, main="Boxplot data_sample_2$pain") #outliers: 1 max (ID 160) , 4 min (ID 112, ID 26, ID 69, ID 63)
boxplot.stats(data_sample_2$pain)
boxplot(data_sample_2$age, main="Boxplot data_sample_2$age") #outliers: 2 max (ID 70, ID 23), 1 min (ID 64)
boxplot.stats(data_sample_2$age)
boxplot(data_sample_2$STAI_trait, main="Boxplot data_sample_2$STAI_trait") #outliers 2 max (ID 26, ID 54), 2 min (ID 40, ID 136)
boxplot.stats(data_sample_2$STAI_trait)
boxplot(data_sample_2$pain_cat, main="Boxplot data_sample_2$pain_cat") #outliers 1 min (ID 26)
boxplot.stats(data_sample_2$pain_cat)
boxplot(data_sample_2$cortisol_serum, main="Boxplot data_sample_2$cortisol_serum")
boxplot.stats(data_sample_2$cortisol_serum)
boxplot(data_sample_2$cortisol_saliva, main="Boxplot data_sample_2$cortisol_saliva")
boxplot.stats(data_sample_2$cortisol_saliva)
boxplot(data_sample_2$mindfulness, main="Boxplot data_sample_2$mindfulness")
boxplot.stats(data_sample_2$mindfulness)
boxplot(data_sample_2$weight, main= "Boxplot data_sample_2$weight") #Outlier: 4 min (ID 43, ID 32, ID 38, ID 39)
boxplot.stats(data_sample_2$weight)

par(mfrow=c(1,1))

#----REGRESSION MODEL 1 and 2----
#Scatterplots each predictor with outcome "pain"
par(mfrow=c(3,3))
plot(pain ~ age, data=data_sample_2, main="Scatterplot pain ~ age")
abline(lm(pain ~ age, data=data_sample_2))
plot(pain ~ STAI_trait, data=data_sample_2, main="Scatterplot pain ~ STAI_trait")
abline(lm(pain ~ STAI_trait, data=data_sample_2))
plot(pain ~ pain_cat, data=data_sample_2, main="Scatterplot pain ~ pain_cat")
abline(lm(pain ~ pain_cat, data=data_sample_2))
plot(pain ~ cortisol_serum, data=data_sample_2, main="Scatterplot pain ~ cortisol_serum")
abline(lm(pain ~ cortisol_serum, data=data_sample_2))
plot(pain ~ cortisol_saliva, data=data_sample_2, main="Scatterplot pain ~ cortisol_saliva")
abline(lm(pain ~ cortisol_saliva, data=data_sample_2))
plot(pain ~ mindfulness, data=data_sample_2, main="Scatterplot pain ~ mindfulness")
abline(lm(pain ~ mindfulness, data=data_sample_2))
plot(pain ~ mindfulness, data=data_sample_2, main="Scatterplot pain ~ weight")
abline(lm(pain ~ weight, data=data_sample_2))
par(mfrow=c(1,1))

#Regression Model 1
mod1 = lm(pain ~ age + sex, data = data_sample_2)
summary(mod1)
confint(mod1)
standardCoefs(mod1)
lm.beta(mod1)


#Regression Model 2
mod2_pre = lm(pain ~ age + sex+ STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data=data_sample_2)
summary(mod2_pre)
confint(mod2_pre)
lm.beta(mod2_pre)
standardCoefs(mod2_pre)

#----DIAGNOSTICS MODEL 2----
#Check for assumptions of preliminary final model 2
#Check for collinearity, VIF=Variance inflaction factors, cut off = 4 
vif( mod = mod2_pre) #suspicious VIF,
cor( x = data_sample_2$cortisol_serum, y = data_sample_2$cortisol_saliva )  #serum and saliva highly correlated

pairs.panels(data_sample_2[,c("pain", "sex", "age", "STAI_trait", "pain-cat", "mindfulness", "cortisol_saliva", "cortisol_serum")], col="red", lm=T)


#---NEW RGERESSION MODEL 2---
#with only one cortisol predictor, saliva excluded based on theory
mod2=lm(pain ~ age + sex+ STAI_trait + pain_cat + mindfulness + cortisol_serum, data=data_sample_2)
summary(mod2)
confint(mod2)
standardCoefs(mod2)


#----MODEL DIAGNOSTICS MODEL 2 ---
#(1) Assumptions
#Multicollinarity
vif(mod2) 
par(mfrow=c(2,2))
plot(mod2)
par(mfrow=c(1,1))

#Test for normality
hist(x=residuals(mod2),xlab= "Value of residual", breaks=40) #Nr. 1 Histogram (ok)
shapiro.test(x=residuals(mod2)) #Nr. 2 ShapiroTest (ok, no evidence that these data depart from normality)
plot(x=mod2,which = 2) #Nr.3 QQ (ok)
describe(residuals(mod2)) #Nr 4 skew = -0.07 (ok)

#Test for linearity
pred.mod2<-predict(object=mod2)
yhat.2 <- fitted.values( object = mod2 )
plot( x = yhat.2,
      y = data_sample_2$pain,
      xlab = "Fitted Values",
      ylab = "Observed Values") #Test Nr.1 (ok)
plot(x=mod2, which=1) #Test Nr. 2 (ok)
residualPlots( model = mod2) #Test Nr. 3 (ok), p>0.05, so no indice for non-linearity

#Test for Homoscedasticity
plot(x = mod2, which = 3, main="Test for homoscedasticity") #Test Nr. 1 (ok)
ncvTest(mod2) # Test Nr. 2 (ok), p>0.05, so no violations of homogeneity of variance

#(2) Check for influential outliers with Cooks Distance (4/N=0,026)
cooks.distance.mod2 <- cooks.distance( model = mod2)  #Test Nr.1

plot(cooks.distance(mod2), main="Cook's distance", xlab="Particpant")
tail(sort(cooks.distance.mod2),10) #IDs 37, 112, 46, 99, 160, 5, 82, 54, 63

par(mfrow=c(1,2))
plot(x=mod2, which = 4) #Test Nr.2
plot(x=mod2, which = 5) # Test Nr.3
par(mfrow=c(1,1))

#---COMPARISON MODEL 1 & 2 ---
#explained variance
summary(mod1)
confint(mod1)
standardCoefs(mod1)
lm.beta(mod1)

summary(mod2)
confint(mod2)
standardCoefs(mod2)
lm.beta(mod2)

#AIC
AIC(mod1) #527.5995
AIC(mod2) #460.0452

#Anova
anova(mod1, mod2) 

#choose model 2 based on R^2, AIC, anova







###----------ASSIGNMENT 2----------###
######################################

#----2A----REGRESSION AND COMPARISON OF FULL.MODEL, BACKWARD.MODEL, THEORY-BASED-MODEL

#----DATA EXPLORATION ----
#Descriptive data + scatterplot for variable "weight" as it has not been used before
boxplot(data_sample_2$weight, main="Boxplot data_sample_2$weight") #Outliers: 4 min (ID 122, ID 73, ID 56, ID 53)
hist(data_sample_2$weight)
plot(pain ~ weight, data=data_sample_2, main="Scatterplot pain ~ weight")
abline(lm(pain ~ weight, data=data_sample_2))

#---REGRESSION FULL.MODEL---
#Regression full.model: setting up full model with all predictors except "cortisol_saliva"
full.model=lm(pain ~ age + sex + STAI_trait + cortisol_serum + pain_cat + mindfulness  + weight, data=data_sample_2)
summary(full.model)

#MODEL DIAGNOSTICS
#Test for normality of residuals
hist(x=residuals(full.model),xlab= "Value of residual", breaks=30) #Nr. 1, 
shapiro.test(x=residuals(full.model)) #Nr. 2 (ok, p> 0.05)
plot(x=full.model,which = 2) #Nr.3 QQ (ok)
describe(residuals(full.model)) #Nr.4 (ok), skew=-0.07

#Test for Linearity
yhat.full <- fitted.values( object = full.model )
plot( x = yhat.full,
      y = data_sample_2$pain,
      xlab = "Fitted Values",
      ylab = "Observed Values",
      main= "Regression full.model") #Nr.1
plot(x=full.model, which=1) #Nr.2 (ok)
residualPlots( model = full.model ) #Nr. 3 (ok)

#Test for Homoscedasticity
plot(x = full.model, which = 3, main="Test for homoscedasticity") #Nr.1
ncvTest(full.model) #Nr.2 (ok)

#Check for collinearity, VIF=Variance inflaction factors, cut off = 4 
vif( mod = full.model)# (ok)
 
par(mfrow=c(2,2))
plot(full.model)
par(mfrow=c(1,1))

#Check for influential outliers
cooks.distance.full.model <- cooks.distance( model = full.model )  #Test Nr.1
plot(cooks.distance(full.model), main="Cook's distance full.model", xlab="Particpant")
tail(sort(cooks.distance.full.model),13) #IDs 37, 112, 160, 46, 73, 99, 56, 54, 63, 115, 82, 5 are above 4/N

plot(x=full.model, which = 4) #Nr.2
plot(x=full.model, which = 5) #Nr.3

#---BACKWARD REGRESSION---
#run backwards regression
step(object=full.model, direction="backward")
?step
#final backward.model
backward.model=lm(pain ~ age + cortisol_serum + pain_cat + mindfulness, 
                  data = data_sample_2)
summary(backward.model)
confint(backward.model)
standardCoefs(backward.model)

#--MODEL DIAGNOSTICS BACKWARDS REGRESSION
#Test for normality of residuals
hist(x=residuals(backward.model),xlab= "Value of residual", breaks=20) #Nr. 1
shapiro.test(x=residuals(backward.model)) #Nr. 2 (ok)
plot(x=backward.model,which = 2, main="QQ plot backward.model") #Nr.3
describe(residuals(backward.model)) #Nr.4 (ok)

#Test for linearity
yhat.back <- fitted.values( object = backward.model )
plot( x = yhat.back,
      y = data_sample_2$pain,
      xlab = "Fitted Values",
      ylab = "Observed Values",
      main="Values backward.model") #Nr.1
plot(x=backward.model, which=1, main="Regression backward.model") #Nr. 2
residualPlots(model = backward.model ) #Nr. 3 incl. Tuckey Test last line

#Test for Homoscedasticity
plot(x = backward.model, which = 3) #Nr. 1
ncvTest(backward.model) #Nr. 2


#Check for collinearity, VIF=Variance inflaction factors, cut off = 4 
vif( mod = backward.model)

par(mfrow=c(2,2))
plot(backward.model)
par(mfrow=c(1,1))

#Check for influential outliers
cooks.distance.backward.model <- cooks.distance( model = backward.model )  #Test Nr.1
plot(cooks.distance(backward.model), main="Cook's distance backward.model", xlab="Particpant")
tail(sort(cooks.distance.backward.model),13) #IDs 112, 37, 160, 82, 5, 99, 63, 46 are above 4/N
plot(x=backward.model, which = 4) #Nr.2
plot(x=backward.model, which = 5) #Nr.3

#MODEL COMPARISON FULLMODEL AND BACKWARD MODEL
summary(full.model)
summary(backward.model)
anova(full.model, backward.model)
AIC(full.model, backward.model)
BIC(full.model, backward.model)

#----MODEL COMPARISON THEORY BASED MODEL AND BACKWARD BACKWARD---
#Regression theory.based.model 
theory.based.model=lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data=data_sample_2)
summary(theory.based.model)
confint(theory.based.model)
standardCoefs(theory.based.model)

#Comparison theory.based.model and backward.model
summary(theory.based.model)
summary(backward.model)
anova(theory.based.model, backward.model)
AIC(theory.based.model, backward.model)
BIC(theory.based.model, backward.model)

#----2B----MODEL TESTING ON NEW DATA----#

data_sample_3=read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_2.csv")

#DATA EXPLORATION
#Descriptive analysis for coding errors etc.
who(data_sample_3) 
summary(data_sample_3)
describe(data_sample_3) #cases with mindfulness<1

boxplot(data_sample_3$mindfulness)
stem(data_sample_3$mindfulness) #remove 6 cases mindfulness<1

data_sample_4=data_sample_3 #copy of data set
data_sample_4=data_sample_4[!data_sample_4$mindfulness<1,] #remove all cased mindfulness<1
summary(data_sample_4)

#Histograms: look at distribution
hist(data_sample_4$pain, breaks = 30)
hist(data_sample_4$age, breaks = 30)
hist(data_sample_4$STAI_trait, breaks = 30)
hist(data_sample_4$pain_cat, breaks = 30)
hist(data_sample_4$cortisol_serum, breaks = 30)
hist(data_sample_4$cortisol_saliva, breaks = 30)
hist(data_sample_4$mindfulness, breaks = 30)
hist(data_sample_4$weight, breaks = 30)

#Boxplots
boxplot(data_sample_4$pain, main="Boxplot data_sample_4$pain")
boxplot(data_sample_4$age, main="Boxplot data_sample_4$age")
boxplot(data_sample_4$STAI_trait, main="Boxplot data_sample_4$STAI_trait") #Outlier 1 min (ID 155), 1 max (ID 46)
boxplot(data_sample_4$pain_cat, main="Boxplot data_sample_4$pain_cat")
boxplot(data_sample_4$cortisol_serum, main="Boxplot data_sample_4$cortisol_serum") #Outlier 2 max (ID 46, ID 83), 2 min (ID33, ID102)
boxplot(data_sample_4$cortisol_saliva, main="Boxplot data_sample_4$cortisol_saliva") #Outlier 3 min (ID 33, ID 118, ID 91)
boxplot(data_sample_4$mindfulness, main="Boxplot data_sample_4$mindfulness")
boxplot(data_sample_4$weight, main="Boxplot data_sample_4$weight") #Outlier 2 min (ID 15, ID 5 )

#APPLICATION OLD MODELS ON NEW DATA SET
#predict values of new data set based on models
pred.theory.based.model <- predict( object = theory.based.model, newdata = data_sample_4)
plot( x = pred.theory.based.model, y = data_sample_4$pain, 
      xlab = "Fitted Values", ylab = "Observed Values")

pred.backward.model <- predict( object = backward.model, newdata = data_sample_4)
plot( x = pred.backward.model, y = data_sample_4$pain, 
      xlab = "Fitted Values", ylab = "Observed Values")

#residuals: amount of error produced by application of models on new data
RSS_theory.based.model = sum((data_sample_4$pain - pred.theory.based.model)^2)
RSS_theory.based.model

RSS_backward.model = sum((data_sample_4$pain - pred.backward.model)^2)
RSS_backward.model

#occam razor


###----------ASSIGNMENT 3----------###
######################################
#load further packages
library(lme4) # for lmer, mask step function
library(lmerTest) # to get singificance test in lmer
library(reshape2)
library(r2glmm)
library(cAIC4)

data_sample_4=read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_3.csv")

# asign ID as factors
data_sample_4$ID = factor(data_sample_4$ID)

#----EXPLORATION OF DATA
#check for missing data & outliers
who(data_sample_4)
summary(data_sample_4)
describe(data_sample_4)

#check for outliers
par(mfrow=c(2,4))
boxplot(data_sample_4$age, main="Boxplot data_sample_4$age")
boxplot(data_sample_4$weight, main="Boxplot data_sample_4$weight")
boxplot(data_sample_4$STAI_trait, main="Boxplot data_sample_4$STAI_trait") #Outliers 1 min (ID 10) $ 1 max (ID 13)
boxplot(data_sample_4$pain_cat, main="Boxplot data_sample_4$pain_cat") #Outlier 1 max (ID 11)
boxplot(data_sample_4$cortisol_serum,"Boxplot data_sample_4$cortisol_serum") #1Outlier
boxplot(data_sample_4$mindfulness, main="Boxplot data_sample_4$mindfulness")
boxplot(data_sample_4$pain_rating, main="Boxplot data_sample_4$pain_rating") #Outlier: 1 max (ID 11)
par(mfrow=c(1,1))

# histograms
par(mfrow=c(2,2))
hist(data_sample_4$pain1)
hist(data_sample_4$pain2)
hist(data_sample_4$pain3)
hist(data_sample_4$pain4)
par(mfrow=c(1,1))

#designate which are the repeated varibales & correlation
repeated_variables = c("pain1",	"pain2", "pain3",	"pain4")
cor(data_sample_4[,repeated_variables]) #correlation between close times higher.

#----TRANSFORMATION FORMAT WIDE TO LONG
# id.vars should be all non-repeated variables
data_sample_4_long = melt(data_sample_4, measure.vars=repeated_variables, variable.name = "time", value.name = "pain_rating")
# order data frame by participant ID(not necessary, just makes the dataframe look more intuitive)
data_sample_4_long = data_sample_4_long[order(data_sample_4_long[,"ID"]),]
# change the time variable to a numerical vector
data_sample_4_long$time = as.numeric(data_sample_4_long$time)

#check data again
data_sample_4_long
summary(data_sample_4_long)



#---REGRESSION MODELS
#fixed model (1)
mod_fix=lm(pain_rating ~ sex + age + weight + STAI_trait + pain_cat + cortisol_serum + mindfulness + time, data=data_sample_4_long)
summary(mod_fix)

#model with random intercept for participant ID (2)
mod_random_int = lmer(pain_rating ~ sex + age + weight + STAI_trait + pain_cat + cortisol_serum + mindfulness + time +  (1|ID), data = data_sample_4_long)
summary(mod_random_int)

#model with random intercepts and slopes (3)
mod_random_both = lmer(pain_rating ~ sex + age + weight + STAI_trait + pain_cat + cortisol_serum + mindfulness + time + (time|ID), data = data_sample_4_long)
summary(mod_random_both)


#----PLOTS MODEL 1, 2, 3 
# plot with regression line individually for each particpant (1)
data_sample_4_long$pred_mod_fix = predict(mod_fix)
ggplot(data_sample_4_long, aes(y = pain_rating, x = time, group = ID))+
  geom_point(size = 2) +
  geom_line(color='red', aes(y=pain_rating, x=time))+
  facet_wrap( ~ ID, ncol = 5)

# random intercept model (2)
data_sample_4_long$pred_random_int = predict(mod_random_int)
ggplot(data_sample_4_long, aes(y = pain_rating, x = time, group = ID))+
  geom_point(size = 2)+
  geom_line(color='red', aes(y=pred_random_int, x=time))+
  facet_wrap( ~ ID, ncol = 5)

# random slope and intercept model (3)
data_sample_4_long$pred_random_both = predict(mod_random_both)
ggplot(data_sample_4_long, aes(y = pain_rating, x = time, group = ID))+
  geom_point(size = 2)+
  geom_line(color='red', aes(y=pred_random_both, x=time))+
  facet_wrap( ~ ID, ncol = 5)

#----LINEAR MODEL COMPARISON (2) & (3)----
#AIC
AIC(mod_random_int, mod_random_both)

#summary
summary(mod_random_int) #Model 2
summary(mod_random_both) #Model 3

#cAIC
cAIC(mod_random_int)$caic
cAIC(mod_random_both)$caic
?cAIC #-->method

r2beta(mod_random_int, method="nsj")
r2beta(mod_random_both, method="nsj")

# anova
anova(mod_random_int, mod_random_both)

#----REGRESSION QUADRATIC MODEL (4)----
#model and summary
mod_random_int_quad = lmer(pain_rating ~ sex + age + weight + STAI_trait + pain_cat + cortisol_serum + mindfulness + time + I(time^2) + (1+time|ID), data = data_sample_4_long)
summary(mod_random_int_quad)
r2beta(mod_random_int_quad, method="nsj")
cAIC(mod_random_int_quad)
AIC(mod_random_int_quad)


stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

stdCoef.merMod(mod_random_int_quad)
confint(mod_random_int_quad) #no overlap with 0 --> so ok

#Plot
data_sample_4_long$pred_random_int_quad = predict(mod_random_int_quad) 
ggplot(data_sample_5_long, aes(y = pain_rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_random_int_quad, x=time))+
  facet_wrap( ~ ID, ncol = 5)



#----MODEL COMPARISON RANDOM INTERCEPTS (2) & (4)
#summary
summary(mod_random_int)
summary(mod_random_int_quad)

#anova
anova(mod_random_int, mod_random_int_quad) 

#cAIC
cAIC(mod_random_int)$caic
cAIC(mod_random_int_quad)$caic


#----MODEL DIAGNOSTICS MOD_RANDOM_INT_QUAD----
#Influential outliers (exludes groups and cases one my one, should not differ)
require(influence.ME)
influence(mod_random_int_quad, group = "ID")$alt.fixed
influence(mod_random_int_quad, obs = T)$alt.fixed

#Normality of residuals
describe(residuals(mod_random_int_quad)) #Test Nr. 1: skew
hist(residuals(mod_random_int_quad), breaks=40) #Test Nr.2 (ok)
shapiro.test(residuals(mod_random_int_quad))  #Test Nr. 3
qqmath(mod_random_int_quad, id=0.1) #Test Nr. 4 (IDs 2, 20, 8?)


#Linearity assumption of prediction and standardized residuals
plot(mod_random_int_quad)

  #linearity of each predictor and the standardized residual
  # visualize the linearity of connection of each predictor

par(mfrow=c(2,4))
predictors=c("age")
for(i in 1:length(predictors)){
  predictor_to_test = data_sample_4_long[,predictors[i]]
  print(
    ggplot(data.frame(x = predictor_to_test,pearson=residuals(mod_random_int_quad,type="pearson")),
           aes(x=x,y=pearson)) +
      geom_point() +
      geom_smooth(method = 'loess') +
      theme_bw())}

#predictors=c("time")
#for(i in 1:length(predictors)){
 # predictor_to_test = data_sample_4_long[,predictors[i]]
  #print(
   # ggplot(data.frame(x = predictor_to_test,pearson=residuals(mod_random_int_quad,type="pearson")),
    #       aes(x=x,y=pearson)) +
     # geom_point() +
      #geom_smooth(method = 'loess') +
      #theme_bw() )}

predictors=c("STAI_trait")
for(i in 1:length(predictors)){
  predictor_to_test = data_sample_4_long[,predictors[i]]
  print(
    ggplot(data.frame(x = predictor_to_test,pearson=residuals(mod_random_int_quad,type="pearson")),
           aes(x=x,y=pearson)) +
      geom_point() +
      geom_smooth(method = 'loess') +
      theme_bw() )}

predictors=c("pain_cat")
for(i in 1:length(predictors)){
  predictor_to_test = data_sample_4_long[,predictors[i]]
  print(
    ggplot(data.frame(x = predictor_to_test,pearson=residuals(mod_random_int_quad,type="pearson")),
           aes(x=x,y=pearson)) +
      geom_point() +
      geom_smooth(method = 'loess') +
      theme_bw() )}

predictors=c("cortisol_serum")
for(i in 1:length(predictors)){
  predictor_to_test = data_sample_4_long[,predictors[i]]
  print(
    ggplot(data.frame(x = predictor_to_test,pearson=residuals(mod_random_int_quad,type="pearson")),
           aes(x=x,y=pearson)) +
      geom_point() +
      geom_smooth(method = 'loess') +
      theme_bw() )}

predictors=c("mindfulness")
for(i in 1:length(predictors)){
  predictor_to_test = data_sample_4_long[,predictors[i]]
  print(
    ggplot(data.frame(x = predictor_to_test,pearson=residuals(mod_random_int_quad,type="pearson")),
           aes(x=x,y=pearson)) +
      geom_point() +
      geom_smooth(method = 'loess') +
      theme_bw() )}

predictors=c("weight")
for(i in 1:length(predictors)){
  predictor_to_test = data_sample_4_long[,predictors[i]]
  print(
    ggplot(data.frame(x = predictor_to_test,pearson=residuals(mod_random_int_quad,type="pearson")),
           aes(x=x,y=pearson)) +
      geom_point() +
      geom_smooth(method = 'loess') +
      theme_bw() )}

par(mfrow=c(1,1))

# Homoscedasticty assumption
plot(mod_random_int_quad) #funnel shape ?

  # Levens model for testing the for heteroscedasticity, from here: http://ademos.people.uic.edu/Chapter18.html
  # look at the overall model F and p, if it is significant, there may be heteroscedasticity
summary(lm(residuals(mod_random_int_quad)^2 ~ data_sample_4_long[,"ID"]))

# Multicollinearity
# there are some functions out there, but for now just look at the correlation matrix
# some example of a function designed to extract vif from lmer: https://raw.githubusercontent.com/aufrank/R-hacks/master/mer-utils.R
pairs.panels(data_sample_4_long, col = "red", lm = T)

#VIF
vif.mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

kappa.mer <- function (fit,
                       scale = TRUE, center = FALSE,
                       add.intercept = TRUE,
                       exact = FALSE) {
  X <- fit@pp$X
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  nrp <- sum(1 * (nam == "(Intercept)"))
  if (nrp > 0) {
    X <- X[, -(1:nrp), drop = FALSE]
    nam <- nam[-(1:nrp)]
  }
  
  if (add.intercept) {
    X <- cbind(rep(1), scale(X, scale = scale, center = center))
    kappa(X, exact = exact)
  } else {
    kappa(scale(X, scale = scale, center = scale), exact = exact)
  }
}

colldiag.mer <- function (fit,
                          scale = TRUE, center = FALSE,
                          add.intercept = TRUE) {
  ## adapted from perturb::colldiag, method in Belsley, Kuh, and
  ## Welsch (1980).  look for a high condition index (> 30) with
  ## more than one high variance propotion.  see ?colldiag for more
  ## tips.
  result <- NULL
  if (center) 
    add.intercept <- FALSE
  if (is.matrix(fit) || is.data.frame(fit)) {
    X <- as.matrix(fit)
    nms <- colnames(fit)
  }
  else if (class(fit) == "mer") {
    nms <- names(fixef(fit))
    X <- fit@X
    if (any(grepl("(Intercept)", nms))) {
      add.intercept <- FALSE
    }
  }
  X <- X[!is.na(apply(X, 1, all)), ]
  
  if (add.intercept) {
    X <- cbind(1, X)
    colnames(X)[1] <- "(Intercept)"
  }
  X <- scale(X, scale = scale, center = center)
  
  svdX <- svd(X)
  svdX$d
  condindx <- max(svdX$d)/svdX$d
  dim(condindx) <- c(length(condindx), 1)
  
  Phi = svdX$v %*% diag(1/svdX$d)
  Phi <- t(Phi^2)
  pi <- prop.table(Phi, 2)
  colnames(condindx) <- "cond.index"
  if (!is.null(nms)) {
    rownames(condindx) <- nms
    colnames(pi) <- nms
    rownames(pi) <- nms
  } else {
    rownames(condindx) <- 1:length(condindx)
    colnames(pi) <- 1:ncol(pi)
    rownames(pi) <- 1:nrow(pi)
  }         
  
  result <- data.frame(cbind(condindx, pi))
  zapsmall(result)
}

maxcorr.mer <- function (fit,
                         exclude.intercept = TRUE) {
  so <- summary(fit)
  corF <- so@vcov@factors$correlation
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0 & exclude.intercept) {
    corF <- corF[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  corF[!lower.tri(corF)] <- 0
  maxCor <- max(corF)
  minCor <- min(corF)
  if (abs(maxCor) > abs(minCor)) {
    zapsmall(maxCor)
  } else {
    zapsmall(minCor)
  }
}
#VIf über link in seinem script

vif.mer(mod_random_int_quad)

cor(data_sample_4_long[,c("pain_rating", "age", "weight", "STAI_trait", "pain_cat", "mindfulness", "cortisol_serum", "time")])
