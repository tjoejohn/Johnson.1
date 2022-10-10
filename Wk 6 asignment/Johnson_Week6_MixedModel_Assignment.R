setwd("C:/GitHub/Johnson.1/Wk 6 asignment")
# Read in the "Toscano_Griffen_Data.csv" data from GitHub and load the three packages we used in the tutorial this week.
# The paper these data came from is uploaded to Canvas as "Toscano&Griffen_2014_JAE..."

library(MASS)
library(MuMIn)
library(mgcv)

#Make sure all packages are installed and running!

data <- read.csv("Toscano_Griffen_Data.csv")

# First create models with the same (y) and method (GLMM) as the published paper, using the GLMM function from the tutorial. 

#first do summary of data and look at paper to determine Y axis
summary(data)

#Use this line as setup or to go off of! Y is first, than x!!!!
glmm.mod <- glmmPQL(Flight.initiation.distance..FID.~Object, family = gaussian, random = ~ 1 | ID, data = df)
#Actualy run this code!
glmm.mod <- glmmPQL(activity.level~eaten, family = binomial, random = ~ 1 | block, data = data)

  #Create two different models using the same 3 predictor (x) variables from the dataset. (4 points each) 
    # In one model only include additive effects. (Use plus sign. Line 53 in turotial!!!)
    # In the other model include one interactive effect.
# So let's compare models. In the paper they found strong interaction effects between habitat and object.
# All we need to do is change the '+'symbol to a '*' because interaction models also test the predictors individually.
# Use a binomial distribution and block as a random effect in both models to match the paper's analyses. Remember ?family to find distribution names.

#use glmm model!
#Prey, temperature, and eaten??? are x values from paper. Or toadfish

#Model number 1 with only additive affects:

#Example to go off of 
gam.mod1 <- gam(Flight.initiation.distance..FID.~Object + Area, family = Gamma, random = list(ID=~ 1), data = df)

#Actulay run this one!
gam.mod1 <- gam(activity.level~toadfish.cue.treatment+claw.width+carapace.width, family = binomial, random = list(ID=~ block), data = data)

plot(gam.mod1$residuals)

#Model 2 with only one interactive effect:

gam.mod2 <- gam(activity.level~toadfish.cue.treatment+claw.width*carapace.width, family = binomial, random = ~ 1 | block, data = data)

plot(glmm.mod2)
plot(gam.mod2$residuals)


# The authors used proportional consumption of prey as the (y) in their model, but did not include this in the dataset.
  # So we are going to create it - run the following line, assuming df= your data frame (feel free to change that):
data$prop.cons <- data$eaten/data$prey 

# (Q1) - The code in line 8 is performing two operations at once. What are they? (2 pts)
#ITS ACTUALY LINE 13!!!

#Not only is the code in line 13 extracting a column with a certain name from the data set, but the value data$prop.cons is being assigned to the variables to data$eaten/data$prey. 

?????# (Q2) - Did the interactive effect change which variables predict proportional consumption? How, specifically, did the results change? (5 pts)

#Yes, the interactive effect did change which variables predict proportional consumption. My model with a interactive effect (gam.mod2) had a higher AIC score and a much lower R squared compared to my model with only additive effects (gam.mod1).
  
# (Q3) - Plot the residuals of both models. Do you think either model is a good fit? Why or why not? (3 pts)
plot(gam.mod1$residuals)
plot(gam.mod2$residuals)


r.squaredGLMM(gam.mod1)
r.squaredGLMM(gam.mod2)

summary(gam.mod1)
summary(gam.mod2)

#My Model (gam.mod1) is a better good fit because it has a lower AIC score and a higher R squared value. 
???# On the other hand, I'm not sure if either of the models are a good fit because there is only a difference of about 2 in the Aic score and .001 in both the R squared values. 


# Re-run both models as generalized additive models instead (using gam). Then compare the AIC of both models. (4 points each)
gam.mod1 <- gam(activity.level~toadfish.cue.treatment+claw.width+carapace.width, family = binomial, random = list(ID=~ block), data = data)
gam.mod2 <- gam(activity.level~toadfish.cue.treatment+claw.width*carapace.width, family = binomial, random = list(ID=~ block), data = data)

AIC(gam.mod1, gam.mod2)

random = ~ 1 | block, data = data)

????# (Q4) - Which model is a better fit? (2 pt)

#Neither because the AIC score is exactly the same for both models.


???# (Q5) - Based on the residuals of your generalized additive models, how confident are you in these results? (2 pts)

#Not that confident, because the residual data is quite scattered and does not show a clear trend in the data.  





