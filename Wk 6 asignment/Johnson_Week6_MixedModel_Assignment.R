setwd("C:/GitHub/Johnson.1/Wk 6 asignment")
# Read in the "Toscano_Griffen_Data.csv" data from GitHub and load the three packages we used in the tutorial this week.
# The paper these data came from is uploaded to Canvas as "Toscano&Griffen_2014_JAE..."

library(MASS)
library(MuMIn)
library(mgcv)

#Make sure all packages are installed and running!

data <- read.csv("Toscano_Griffen_Data.csv")

# First create models with the same (y) and method (GLMM) as the published paper, using the GLMM function from the tutorial. 
  #Create two different models using the same 3 predictor (x) variables from the dataset. (4 points each) 
    # In one model only include additive effects.
    # In the other model include one interactive effect.
# So let's compare models. In the paper they found strong interaction effects between habitat and object.
# All we need to do is change the '+'symbol to a '*' because interaction models also test the predictors individually.
# Use a binomial distribution and block as a random effect in both models to match the paper's analyses. Remember ?family to find distribution names.

#use glmm model!

#first do summary of data and look at paper to determine Y axis
summary(data)

#Use this line as setup or to go off of! Y is first, than x!!!!
glmm.mod <- glmmPQL(Flight.initiation.distance..FID.~Object, family = gaussian, random = ~ 1 | ID, data = df)
#Actualy run this code!
glmm.mod <- glmmPQL(activity.level~eaten, family = binomial, random = ~ 1 | block, data = data)

#Model number 1 with only additive affects:

#Example to go off of 
glmm.mod <- glmmPQL(Flight.initiation.distance..FID.~Object, family = gaussian, random = ~ 1 | ID, data = df)
#Actuality run this one!
glmm.mod1 <- glmmPQL(activity.level~toadfish.cue.treatment+claw.width+carapace.width, family = binomial, random = ~ 1 | block, data = data)


#Model 2 with only one interactive effect:
glmm.mod2 <- glmmPQL(activity.level~toadfish.cue.treatment+claw.width*carapace.width, family = binomial, random = ~ 1 | block, data = data)

# The authors used proportional consumption of prey as the (y) in their model, but did not include this in the dataset.
  # So we are going to create it - run the following line, assuming df= your data frame (feel free to change that):
data$prop.cons <- data$eaten/data$prey 

# (Q1) - The code in line 8 is performing two operations at once. What are they? (2 pts)
#(ITS ACTUALY LINE 13)!!!

#Not only is the code in line 13 extracting a column with a certain name from the data set, but the value data$prop.cons is being assigned to the variables data$eaten/data$prey. 

# (Q2) - Did the interactive effect change which variables predict proportional consumption? How, specifically, did the results change? (5 pts)

#Yes, the interactive effect did change which variables predict proportional consumption. By adding an interactive effect, the model with the interactive effect(glmm.mod2) had a higher R squared compared to my model with only additive effects (glmm.mod1, which had a lower R squared. 
summary(glmm.mod1)
summary(glmm.mod2)
#If you look at the summary output, adding the interactive effect actually didn't help. The same variables are significant and the interaction is not...so the very tiny increase in R squared is a result of over-fitting.

# (Q3) - Plot the residuals of both models. Do you think either model is a good fit? Why or why not? (3 pts)
plot(glmm.mod1)
plot(glmm.mod2)

r.squaredGLMM(glmm.mod1)
r.squaredGLMM(glmm.mod2)

#Based on the residuals of both models, I believe that my model (glmm.mod2) is a better fit because it has a higher R squared value than my (glmm.mod2) model. 
#This does not answer the question - residuals are not related to R squared this way.

# Re-run both models as generalized additive models instead (using gam). Then compare the AIC of both models. (4 points each)
gam.mod1 <- gam(activity.level~toadfish.cue.treatment+claw.width+carapace.width, family = binomial, random = list(ID=~ block), data = data)
gam.mod2 <- gam(activity.level~toadfish.cue.treatment+claw.width*carapace.width, family = binomial, random = list(ID=~ block), data = data)

AIC(gam.mod1, gam.mod2)

# (Q4) - Which model is a better fit? (2 pt)

#My model (gam.mod1) is a better fit because it has a lower AIC score. 


# (Q5) - Based on the residuals of your generalized additive models, how confident are you in these results? (2 pts)
plot(gam.mod1$residuals)
plot(gam.mod2$residuals)

#I am confident in my results, because the residual data for both of my generalized additive models is quite similar and there is not a clear trend, which is a goof thing in this case. 




