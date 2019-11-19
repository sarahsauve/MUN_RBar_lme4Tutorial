#======================================================================================================================================================
#R-Bar Workshop on lme4 package
#November 25, 2019
#Sarah Sauve
#======================================================================================================================================================

#======================================================================================================================================================
#Install and load lme4 package
#install.packages("lme4")
library("lme4")
library("tidyverse")
library("gganimate")
#These next two are both required by gganimate but aren't automatically installed with it
library("gifsky")
library("png")
#======================================================================================================================================================

#======================================================================================================================================================
#What is linear modelling?

#The linear model is based on the y = mx + b equation where
#y is the outcome (data point)
#m is the slope of the line
#x is the input (condition, trial, etc.)
#b is the intercept of the line

#We're also going to be adding an error value 'e' to account for noise in our data.

#Say you're dealing with reaction times (RT) to rating words as positive or negative and your RT is 750ms for a positive (2) word and 800ms for a
#negative (1) word. With those two x values (1, 2) and two y values (750, 800), what are the m and b values that will create the best line to explain
#the data?

#So, linear modelling is the optimization of a linear function to best explain, or fit, your data.

#======================================================================================================================================================
#We're going to use simulated data for a word RT study with
#a within-subjects design: positive or negative

#Import data from your 'Files' tab & have a look

#First, let's visualize it

#Quick plot
plot(data$RT)

#Organized plot
dataplot <- ggplot(data, aes(x = Item, y = RT)) +
  geom_point(size = 2) +
  facet_grid(~ Subject) +
  theme_bw()

dataplot

#------------- Null Model ------------------------
#Now, we're going to fit the most basic model possible: intercept only.
#To do this, we have to use the lm function because lmer must include random effects - we'll get to those shortly.

intercept <- lm(RT ~ 1, data = data)

#Let's look at this model
summary(intercept)

#Add its predictions to our data frame using the fitted() function
data$intercept.predictions <- fitted(intercept)

#Visualize
dataplot +
  geom_point(aes(y = data$intercept.predictions), shape = "triangle", size = 4)

#------------ Fixed Effect -----------------------
#Next, we'll add a fixed effect, which is typically your manipulation and any co-variates you might have

fixed <- lm(RT ~ 1 + Category, data = data)

#Look at it
summary(fixed)

#Add predictions to data frame
data$fixed.predictions <- fitted(fixed)

#Visualize
dataplot +
  geom_point(aes(y = data$fixed.predictions), shape = "triangle", size = 4)

#----------- Random Effects 1 --------------------
#Random effects can be applied to the slope, or the intercept and reflect the fact that your participants and your items are only a subset of all the
#possible people and words (in this case) that could be possible. Each participant will respond in a unique way and each item may have particular traits
#that elicit a particularly faster or slower response.

#We'll start by modelling random intercepts for each participant

random.intercepts <- lmer(RT ~ 1 + Category + (1|Subject), data = data)

#Look at the model
summary(random.intercepts)

#Add predictions to data frame
data$randintercept.predictions <- fitted(random.intercepts)

#Visualize
dataplot +
  geom_point(aes(y = data$randintercept.predictions), shape = "triangle", size = 4)

#You can look at the value of the intercept for each participant using ranef() (RANdom EFfects)
#These are in relation to the model's intercept (add or subtract from the (Intercept) value returned by summary())
ranef(random.intercepts)

#Same for fixed effects if you're looking to use them elsewhere in particular (easy access)
fixef(random.intercepts)

#VarCorr extracts the random effects table
VarCorr(random.intercepts)

#And now we'll model random intercepts for each item

random.item.intercepts <- lmer(RT ~ 1 + Category + (1|Subject) + (1|Item), data = data)

#Look at the model
summary(random.item.intercepts)

#Add predictions to data frame
data$randitemintercept.predictions <- fitted(random.item.intercepts)

#Visualize
dataplot +
  geom_point(aes(y = data$randitemintercept.predictions), shape = "triangle", size = 4)

#---------- Random Effects 2 -------------------
#Now we'll add random slopes for each participant based on each category - each person will react differently to each type of word

random.slopes <- lmer(RT ~ 1 + Category + (Category|Subject) + (1|Item), data = data)

#Look at the model
summary(random.slopes)

#Add predictions to data frame
data$randslope.predictions <- fitted(random.slopes)

#Visualize
dataplot +
  geom_point(aes(y = data$randslope.predictions), shape = "triangle", size = 4)

#Look at random effect values
ranef(random.slopes)
VarCorr(random.slopes)

#For fun, check out the improvement in predictions with a gganimate plot!
#Import long format data
ggplot(longdata, aes(x = Item, y = Raw)) +
  geom_point(size = 2) +
  facet_grid(~ Subject) +
  theme_bw() +
  geom_point(aes(x = Item, y = Predictions), size = 3, col = "Red") +
  transition_states(ModelType, transition_length = 1, state_length = 3)

#---------- Co-variates -----------------------
#Co-variates would be treated like a fixed effect. Say you want to see the effect of age on RTs.

data$Age <- rnorm(n = 16, mean = 40, sd = 10)

covariate <- lmer(RT ~ 1 + Category + Age + (Category|Subject) + (1|Item), data = data)

#Look at model
summary(covariate)

#add predictions to data frame
data$covariate.predictions <- fitted(covariate)

#Visualize
dataplot +
  geom_point(aes(y = data$covariate.predictions), shape = "triangle", size = 4)

#---------- Other -----------------------------

#Interactions

interaction <- lmer(RT ~ 1 + Category * Age + (Category|Subject) + (1|Item), data = data)

#separate predictors

sep.predictors <- lmer(RT ~ 1 + Category + Age + Category:Age + (Category|Subject) + (1|Item), data = data)

#This is the same:

equal.random.slopes <- lmer(RT ~ 1 + Category + (1+Category|Subject) + (1|Item), data = data)
equal.random.slopes2 <- lmer(RT ~ Category + (Category|Subject) + (1|Item), data = data)
#(the intercept is implied)

#Remove covariance between random effects
#This should only be done if you don't want these values to covary, which they do in real life so it needs to be justified

no.covariance <- lmer(RT ~ 1 + Category + (1|Subject) + (0+Category|Subject) + (1|Item), data = data)
summary(no.covariance)

#=======================================================================================================================================================
#Model evaluation

#How do you know how good your model is?  **Caveat - this is based on convention in psychology and APA formatting style

#I use a number of evaluation methods to report my models, I'll just walk through them in no particular order

#1. Pearson's r correlation to the data

cor.test(fitted(random.slopes), data$RT)

#2. R2 coefficient of determination - gives variance in the data explained by the model

variance.explained <- function(model, data){
  SStot <- sum((data-mean(data))^2)
  SSres <- sum(residuals(model)^2)
  n <- length(data)
  fit <- ((SStot-SSres)/n)/(SStot/n)
  fit
}

variance.explained(random.slopes, data$RT)

#I also calculate the variance explained by each predictor by calculating R2 for a model with intercept, then add each predictor, interaction, etc.

#3. Confidence intervals on the predictors

confint(random.slopes, method = "Wald")
#If these include 0, the predictor is not significant to the model

#4. Likelihood-ratio test between null model and maximally fitted model; is the latter better?
anova(null, random.slopes)
#This also contains the AIC (Akaike Information Criteria), BIC (Bayesian IC) and log likelihood of each model as well as each model's degrees of
#freedom and the result of the likelihood ratio test (chisq)

#---------- Troubleshooting ------------------

#What if my model is singular?
#We saw earlier that sometimes R tells us that a model is singular; this means that at least one of the random effects explains no variance at all;
#it is the same for all levels (subjects and items here). This tends to happen if you don't have enough data for the complexity of your model.

#You can fit a simplified model and compare it to your maximally fitted model using R2 and the likelihood ratio test. Report the best model.

variance.explained(random.slopes, data$RT)
variance.explained(random.intercepts, data$RT)

anova(random.intercepts, random.slopes) #The maximally fitted model is much better and explains a lot more variance so report that one

#What if my model doesn't converge?
#This also typically happens when you don't have enough data for the complexity of your model. This simply means that your parameter estimates
#aren't as reliable as if the model did converge. Again, compare it to the next simplest model using the same steps as above.

#======================================================================================================================================================
#Generalized Linear Mixed Effects Models (GLMs)

#GLMs are for data that are not normally distributed. They work via what's called a link function so that a linear model's predictions fit a different
#kind of distribution, like a binomial distribution or a Poisson distribution. Logistic regression (where the variable you're predicting is binary),
#is modelled using a GLM.

#I don't have much experience with GLMs so I won't go into too much detail. The syntax is the same as the linear model we covered above, the only big
#different is that you have to specify the 'family' your data matches: binomial, gaussian, Gamma, inverse.gaussian, poisson, quasi, quasibinomial and
#quasipoisson (use '?family' to find this list).

#For example:
binomial.model <- glmer(RT ~ 1 + Category + (Item|Subject), family = "binomial", data = data)

#This doesn't work because it's not the right type of data, but that's the syntax. As long as you're comparing the same types of models (same family
#and both mixed or fixed effects alone), then you can use the same types of evaluation as above.

#======================================================================================================================================================
#Nonlinear Mixed Effects Models (NLMs)

#I have no experience with nonlinear models at all but they are a part of what the lme4 package can do. This is when the outcome cannot be explained
#by a simple addition of two parameters as we have been doing so far. Examples of this are exponential functions, logarithmic functions and trig
#functions to name just a few. It is sometimes possible to apply a transformation to the data to render it linear but not always.


#This function has three parts: the variable you're predicting, the nonlinear formula, and your model formula

View(Orange)
ggplot(Orange, aes(x = age, y = circumference, color = Tree))+
  geom_point() +
  theme_bw()

sample.nlm <- nlmer(circumference ~ SSlogis(age, Asym, xmid, scal) ~ Asym|Tree, data = Orange,
                    start = c(Asym = 200, xmid = 770, scal = 120))
summary(sample.nlm)
