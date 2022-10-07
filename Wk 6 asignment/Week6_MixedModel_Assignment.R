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

#Use this line as setup or to go off of!
glmm.mod <- glmmPQL(Flight.initiation.distance..FID.~Object, family = gaussian, random = ~ 1 | ID, data = df)
#Actualy run this code!
glmm.mod <- glmmPQL(eaten~activity.level, family = gaussian, random = ~ 1 | block, data = data)

  #Create two different models using the same 3 predictor (x) variables from the dataset. (4 points each) 
    # In one model only include additive effects. (Use plus sign. Line 53 in turotial!!!)
    # In the other model include one interactive effect.
# So let's compare models. In the paper they found strong interaction effects between habitat and object.
# All we need to do is change the '+'symbol to a '*' because interaction models also test the predictors individually.

gam.mod2 <- gam(Flight.initiation.distance..FID.~ Object*Area, family = Gamma, random = list(ID=~ 1), data = df)
    # Use a binomial distribution and block as a random effect in both models to match the paper's analyses. Remember ?family to find distribution names.
#use glmm model!
#Activity level, temperature, and toadfish??? are x values from paper. 

#Model number 1 with only additive affects  

glmm.mod <- glmmPQL(prey~activity.level, family = gaussian, random = ~ 1 | block, data = data)
summary(data)

gam.mod1 <- gam(Flight.initiation.distance..FID.~Object + Area, family = Gamma, random = list(ID=~ 1), data = df)
gam.modd1 <- gam(eaten~activity.level, family = gaussian, random = list(ID=block), data = data)

vis.gam(gam.mod1, view=c("Object","Area"), theta = 45, color = "heat")
vis.gam(gam.mod1, view=c("activity.level","tem"), theta = 45, color = "heat")



# The authors used proportional consumption of prey as the (y) in their model, but did not include this in the dataset.
  # So we are going to create it - run the following line, assuming df= your data frame (feel free to change that):
data$prop.cons <- data$eaten/data$prey 

# (Q1) - The code in line 8 is performing two operations at once. What are they? (2 pts)
#ITS ACTUALY LINE 13!!!

#Not only is the code in line 13 creating the 

# (Q2) - Did the interactive effect change which variables predict proportional consumption? How, specifically, did the results change? (5 pts)


# (Q3) - Plot the residuals of both models. Do you think either model is a good fit? Why or why not? (3 pts)


# Re-run both models as generalized additive models instead (using gam). Then compare the AIC of both models. (4 points each)


# (Q4) - Which model is a better fit? (2 pt)


# (Q5) - Based on the residuals of your generalized additive models, how confident are you in these results? (2 pts)








