library(tidyverse)
library("stringr")
library(car)
library(arm)
library(pROC)
library(e1071)
library(caret)
library(knitr)
library(dplyr)
library(ggplot2)
library(pander)
require(gridExtra)
library(kableExtra)
library(gapminder)
library(forcats)

stats = read.csv("/Users/dapoadegbile/Desktop/NBA FINAL DATA/collegeStatsNBAbpm.csv", header= TRUE)
stats = subset(stats, select = -c(Play_Yrs, Games, X, Year, Pick, X.1))
View(stats)

#stats_sub = stats[, c("TS", "AST", "TO", "USG", "BPM")]

stats$CLASS = factor(stats$CLASS)
stats$CONF = factor(stats$CONF)

# -------------------------------- EXPLORATORY DATA ANALYSIS --------------------------------------------------


ggplot(stats, aes(x= BPM, fill= ..count..)) + 
  geom_histogram(binwidth= .75, color="white") + 
  ggtitle("Distribution of Box Plus Minus") +
  scale_fill_gradient("Count", low = "gray", high = "purple")


#remove outliers

stats = stats[stats$BPM >= -12 & stats$BPM <= 10, ] 
# ---------------------------------------------------------------------------------------

#Interaction 1 -- taller players are typically less skilled in new nba 
ggplot(stats, aes(x= HEIGHT_IN, y = BPM)) +
  geom_point(alpha = .75,colour="blue4") +
  geom_smooth(method="lm",col="red3") + 
  labs(title="Height vs BPM")  #+ facet_wrap(~CLASS)

#Interaction 2  ---- NOT THAT RELEVANT-- Players who have the ball more typically have it for a reason = higher BPM
# ALSO usage rate means more box score stats which would lead to a higher box plus minus 

ggplot(stats, aes(x= USG, y = BPM  )) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + 
  labs(title=" Usage Rate vs BPM")   + facet_wrap(~CONF)



#Interaction 1 -- RELVEVANT --  True Shooting percentage measures how good a scorer you are, makes sense -- younger scorers have more room to grow
# No interaction when wrapping by conference -- scoring across all conferences seems to transfer to nba 
ggplot(stats, aes(x= TS, y = BPM)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + 
  labs(title=" True Shooting Percentage vs BPM") + facet_wrap(~CONF)



#INTERACTION -- PLAYERS THAT ARE YOUNGER THAT ARE RESPONSIBLE FOR TEAMS OFFENSE ARE TYPICALLY BETTER PLAYERS 
ggplot(stats, aes(x= AST, y = BPM)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + 
  labs(title=" Assist Percentage vs Box Plus Minus") + facet_wrap(~CLASS)


ggplot(stats, aes(x= STL, y = BPM)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + 
  labs(title=" STEAL RATE vs BPM") + facet_wrap(~CLASS)


ggplot(stats, aes(x= FTR, y = BPM)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + 
  labs(title="Free Throw Rate vs FT Percentage") + facet_wrap(~CONF)

# Relevant Interaction -- The more 3 pointers shot, typically results in higher percentage (because they can shoot)
ggplot(stats, aes(x= X3P_PERC, y = BPM)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + 
  labs(title="3 Point Rate vs BPM") + facet_wrap(~CONF)

stats %>%
  dplyr::mutate(CONF = fct_reorder(CONF, BPM, .fun = 'median')) %>%
  ggplot(aes(x=CONF, y=BPM, fill=CONF)) +
    geom_boxplot() + #coord_flip() +
    labs(title="Conference vs BPM",x="Conference",y="BPM") + scale_y_continuous(breaks = seq(-8, 6, by=.5), limits=c(-8,6)) +
    theme_classic() + theme(legend.position="none")


stats %>%
  dplyr::mutate(CLASS = fct_reorder(CLASS, BPM, .fun = 'median')) %>%
  ggplot(aes(x=CLASS,  y=BPM, fill=CLASS)) +
    geom_boxplot() + #coord_flip() +
    labs(title="Class vs BPM",x="Class",y="BPM") + scale_y_continuous(breaks = seq(-8, 6, by=.5), limits=c(-8,6)) +
    theme_classic() + theme(legend.position="none")

# ------------------------------------------ MODEL --------------------------------------------------------

skill <- lm(BPM ~  ORTG + USG + TS + AST  + DR + OR + STL + FTR + CLASS + CONF +
              BLK + TO + X3PR + HEIGHT_IN + AST*CLASS + TS*CLASS + TS*CONF , data= stats)
summary(skill)

tab_model(skill)
# ------------------------------------------------- CHECKING ASSUMPTIONS ----------------------------------------------

# Checking Independence and Equal Variance 

plot(skill, which=1,col=c("blue4"))

# Checking Normality

plot(skill, which=2,col=c("blue4"))

# Checking Linearity 

# All Variables check out 
ggplot(stats, aes(x=ORTG, y=skill$residual)) +
  geom_point(alpha= .7) + geom_hline(yintercept= 0, col= 'red3')


# multi colinearity is high for class and TS% and class 
vif(skill)

# ------------------------------------------------- MODEL VALIDATION ------------------------------
# relevel conferences 
stats$CONF = factor(stats$CONF, levels = levels(stats$CONF), labels = c("OTHER", "ACC", "OTHER", "B10", "B12",
                                                           "BE", "OTHER", "OTHER", "OTHER", "OTHER",
                                                           "OTHER", "OTHER", "OTHER", "OTHER", "OTHER",
                                                           "P12", "OTHER", "OTHER","OTHER" ,"SEC", "OTHER", "OTHER"))


# skill_hier = BPM ~  ORTG + USG + TS + AST + TO  + DR + OR + STL + FTR + CLASS + 
#                   BLK + TO + X3PR + HEIGHT_IN + AST*CLASS + TS*CLASS + (1 | CONF)
# 
# hier_model = lmer(skill_hier, stats)
# summary(hier_model)
# 
# library(lattice)
# 
# dotplot(ranef(hier_model, condVar=TRUE))$CONF
# 
# dotplot(ranef(model3, condVar=TRUE))$protocol




NullModel = lm(BPM ~ 1, data= stats)
FullModel = lm(BPM ~  ORTG + USG + TS + AST + TO  + DR + OR + STL + FTR + CLASS + CONF +
                 BLK + TO + X3PR + HEIGHT_IN + AST*CLASS + TS*CLASS , data= stats)

model_forward_AIC <- step(NullModel, scope = formula(FullModel), direction= "forward", trace= 0)
model_forward_AIC$call
summary(model_forward_AIC)


model_stepwise_AIC <- step(NullModel, scope = formula(FullModel), direction= "both", trace= 0)
model_stepwise_AIC$call
summary(model_stepwise_AIC)



# Use this model -- more variables significant 
model_backwards_AIC = step(FullModel, direction="backward", trace=0)
model_backwards_AIC$call
summary(model_backwards_AIC)
tab_model(model_backwards_AIC)

#F test

AIC_backwards_model = lm(formula = BPM ~ ORTG + AST + DR + STL + FTR + CLASS + CONF + 
                           BLK + HEIGHT_IN + AST:CLASS, data = stats)

AIC_stepwise_model = lm(formula = BPM ~ ORTG + STL + DR + BLK + HEIGHT_IN, data = stats)

anova(skill, AIC_backwards_model) #Large P value

anova(skill, AIC_stepwise_model) #Smaller p value

# Checking Independence and Equal Variance 

plot(AIC_backwards_model, which=1,col=c("blue4"))

# Checking Normality

plot(AIC_backwards_model, which=2,col=c("blue4"))


# ---------------------------------------------------------------------------------------------------------------------------------
set.seed(12345)
stats <- stats[sample(nrow(stats)),]  #Reshuffled Data
K <- 10
RMSE <- matrix(0,nrow=K,ncol=1)
kth_fold <- cut(seq(1,nrow(stats)),breaks=K,labels=FALSE)
for(k in 1:K){
  test_index <- which(kth_fold==k)
  train <- stats[-test_index,]
  test <- stats[test_index,]
  model_interaction = lm(formula = BPM ~ ORTG + AST + DR + STL + FTR + CLASS + CONF + 
                           BLK + HEIGHT_IN + AST:CLASS, data = stats)
  predictions <- predict(model_interaction, test)
  RMSE[k,] <- sqrt(mean((predictions - test$BPM)^2))
}

mean(RMSE)


