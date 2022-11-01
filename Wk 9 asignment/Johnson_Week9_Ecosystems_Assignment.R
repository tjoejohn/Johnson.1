# For this week it's time to start exploring your own ideas and questions in R.
  # There are at least five options in the dataset to create the following comparisons.

setwd("C:/GitHub/Johnson.1/Wk 9 asignment")
getwd()

# (Q1 - 12 pts) Use the dataset from the tutorial to complete one redundancy analysis (RDA) with variance partitioning on a different community (NOT the nematodes).
  # Explain the ecological importance of your significant predictor variables, or the importance if none are significant for your community.

#Must install/read in read_excell package fist!

library(readxl)


#Next read in excel file and untransfrom data from tibble format using as.data.frame
abiotic.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Abiotic factors")
abiotic <- as.data.frame(abiotic.tibble)

invert.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Invertebrate_community")
invert <- as.data.frame(invert.tibble)

head(abiotic)
head(invert)

#There is a different amount of samples per invertabrete_community compared to abiotic factors. Run the following code to take an average/mean and make them even!

abiotic.names <- paste(abiotic$Parcel, abiotic$Land_Use)
abiotic$names <- abiotic.names

head(invert)

#Do the same for the invertebrate(invert) community! 

invert.names <- paste(invert$Parcel, invert$Landuse)
invert$names <- invert.names

#Agregate means by names to columns 

abiotic.means <- aggregate(x = abiotic, by = list(abiotic$names), FUN = "mean")

invert.means <- aggregate(x = invert, by = list(invert$names), FUN = "mean")

#Since there is not the same number of rows, must merge both abiotic and invert!
fix.issue <- merge(abiotic.means, invert.means, by = "Group.1", all = FALSE)
#Now that there merged, have to unmerge.

abiotic.unmerged <- fix.issue[,7:13]
invert.unmerged <- fix.issue[,19:87]


head(abiotic.unmerged)
head(invert.unmerged)

#Already removed NA collumns when unmerged, so now we just have to make the abitoic factors as numeric and in the right format.

abiotic.means1 <- abiotic.unmerged # Plot and NA columns
abiotic.means2 <- sapply(abiotic.means1, as.numeric ) # Make sure everything is numeric.
abiotic.means2 <- as.data.frame(abiotic.means2) # Make sure it's in the right format.

head(abiotic.means2)
#Do same thing again, except for invertebrate!

invert.means1 <- invert.unmerged # Plot and NA columns
invert.means2 <- sapply(invert.means1, as.numeric ) # Make sure everything is numeric.
invert.means2 <- as.data.frame(invert.means2) # Make sure it's in the right format.

head(invert.means2)


#Install/read in this package and compare abiotic factors (abiotic) against invertebrate communities (biotic) 
library(vegan)

#This one worked!
colnames(abiotic.means2)
ord <- rda(invert.means2 ~ pH + totalN + Perc_ash + Kalium + Magnesium + Ca + Al, abiotic.means2)
ord

anova(ord)

#P value is not significant, or not even given in this case, so need to take steps further to remove predictors that are not important one at a time!

plot(ord, ylim = c(-2,2), xlim = c(-5,5))

ord <- rda(invert.means2 ~., abiotic.means2) # shorthand for the model that includes everything.
ord.int <- rda(invert.means2 ~1, abiotic.means2)

step.mod <- ordistep(ord.int, scope = formula(ord), selection = "both")
step.mod$anova

step.R2mod <- ordiR2step(ord.int, scope = formula(ord), selection = "forward")

#Aluminum is mainly predicting the community, so re run it only looking at Aluminum

ord2 <- rda(invert.means2 ~ Al, abiotic.means2)
ord2
anova(ord2)
plot(ord2)

#Question 1 answer: Even after removing a couple of non significant predictors from my abiotic factors data frame, none of the predictors p values were statistically significant. Even though none of my predictor variables from the abiotic factors were significant, each predictor variable plays a specific role in the invertebrate community. From the anova, stepR2 mod, and rda, I know that Aluminum was the most important predictor variable followed by calcium and Kalium and they influenced the invertibrate community the most ecologically. The two predictor variables I removed, totalP and OlsenP, affected the invertebrate community the least ecological. Ecologically, it's important to know how each predictor variable (abitoic facor) influences the community (invertebrate community). Antibiotic factors often limit biotic factors 

#(Q2 - 12 pts) Then use the dataset from the tutorial to create a linear model related to your RDA. Try multiple predictors to find the best fit model.
  # Explain the ecological importance of the significant predictors, or lack of significant predictors.


#Since had to merge and unmeaning abiotic.means2, run this code from the tutorial so abiotic.means2 will work correctly in this section.

abiotic.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Abiotic factors")

abiotic <- as.data.frame(abiotic.tibble)

abiotic.names <- paste(abiotic$Site, abiotic$Land_Use, abiotic$Plot)
# Then add back in as a column in the data frame:
abiotic$names <- abiotic.names

abiotic.means <- aggregate(x = abiotic, by = list(abiotic$names), FUN = "mean")
# This created warnings, so we should see what the data frame looks like:
head(abiotic.means)

abiotic.means1 <- abiotic.means[,-16] # NA column
abiotic.means2 <- abiotic.means1[,-1:-6] # Plot and NA columns
abiotic.means2 <- sapply(abiotic.means2, as.numeric ) # Make sure everything is numeric.
abiotic.means2 <- as.data.frame(abiotic.means2) 

#Now read in new sheet from datatset "Data_experiment_urtica"
data_experiment_urtica.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Data_experiment_urtica")
data.experiment <- as.data.frame(data_experiment_urtica.tibble)

#Then create a column called "Parcel" that allows us to merge with the plants data frame based on Parcel numbers.
abiotic.means2$Parcel <- unique(abiotic$Parcel)

# So instead of megring both by group.1 like before, now we merge by "Parcel" and accept all other defaults.
data.experiment <- merge(abiotic.means2, data.experiment, by = "Parcel")


# Let's take a quick look at our data.
# This data frame is getting large enough that View() might be more helpful than head() for an initial look:
View(data.experiment)

#Install these packages to do a distribution of our y (Length_main_stem)
library(fitdistrplus)
library(logspline)


#Plot distribution against ideals
##fit all possible/likely distribution models and check AIC/BIC
fit.weibull <- fitdist(data.experiment$Length_main_stem, distr = "weibull")
fit.norm <- fitdist(data.experiment$Length_main_stem, distr = "norm")
fit.gamma <- fitdist(data.experiment$Length_main_stem, distr = "gamma")
fit.lnorm <- fitdist(data.experiment$Length_main_stem, distr = "lnorm")
fit.nbinom <- fitdist(data.experiment$Length_main_stem, distr = "nbinom")
fit.logis <- fitdist(data.experiment$Length_main_stem, distr = "logis")
fit.geom <- fitdist(data.experiment$Length_main_stem, distr = "geom")

#For some reason fit.nbinom and fit.geom wont run?????:
Error in fitdist(abiotic.invert$totalN, distr = "geom") : 
  the function mle failed to estimate the parameters, 
                with the error code 100

#Use AIC comparisons of distributions to see which is best:
#call from:
gofstat(list(fit.weibull, fit.norm, fit.gamma, 
             fit.lnorm, fit.logis))
#Weibell is the best fit?????
colnames(data.experiment)

mod1 <- lm(Length_main_stem ~ pH + totalN + Kalium + Magnesium + Ca + Al + Flowering + Land_use + Parcel,data.experiment)
summary(mod1)
anova(mod1)
AIC(mod1)

summary(mod1)$adj.r.squared

#Have way to many NA's. especially for parcel. Now we have to remove variables that are unimportant. 
#Remove Parcel, Land_Use, and Flowering. 

mod2 <- lm(Length_main_stem ~ pH + totalN + Kalium + Magnesium + Ca + Al,data.experiment)
summary(mod2)
anova(mod2)
AIC(mod2)

summary(mod2)$adj.r.squared

#R squared got a little bit better. Now try removing calcium and aluminum

mod3 <- lm(Length_main_stem ~ pH + totalN + Kalium + Magnesium,data.experiment)
summary(mod3)
anova(mod3)
AIC(mod3)

summary(mod3)$adj.r.squared

#Huh.. That made our AIC be within 2 of mod 2, but the R squared changed a bit to none. Lets try one more thing. Maybe their is a relationship between pH, total Nitrogen (totalN), and Kalium. 

mod4 <- lm(Length_main_stem ~ pH*totalN*Kalium*Magnesium + Ca + Al,data.experiment)
summary(mod4)
anova(mod4)
AIC(mod4)

summary(mod4)$adj.r.squared

#There is not a relationship between pH, total Nitrogen (totalN), and Kalium. AiC score increased a decent amount, and R suared increased a lot. 
#I think its safe to say that mod3 is the best fit. It has an AIC score within 2 of mod2, and the R squared in mod3 is the best out of all the mod's i tried. 

#The AIC increased and the R squared increased, so there is likely not a significant relationship between calcium (Ca) and Aluminum (Al)

#Question 2 answer: Similar to question 1, there were no predictors that were statistically significant per say. However, in mod 3, I found that pH, total nitrogen (totalN), Kalium, and Magnesium, were the predictors that resulted in the best AIC score (within 2 of mod 2) and the lowest R squared. Ecologically, it makes sense that PH, total nitrogen (totalN), Kalium, and Magnesium in response to Length_main_stem becase the sheet from the data set (Data_experiment_urtica) that includes Length_main_plant, is about the urtica genus of plant. And the predictor variables in my model of best fit (mod3) all influence plants in one way or another ecologically. Even though this makes sense ecologically, the predictors are not statistically significant. 

# (Q3 - 6 pts) Provide a 3-4 sentence synthesis of how these results relate to one another and the value of considering both together for interpreting biotic-abiotic interactions.

#When we look at my results, I used abiotic factors, as well as invertebrate_community for the rda and Data_experiment_urtica for the linear model in this assignment. Even though these rebuts may differ within there community, the biotic factors in this case are all one big community that can be limited by abiotic factors. When we consider both the invertribate_community and Data_experiment_urtica, we obtain valuable ecological information and see that both communities are influenced by certain things (predictors), including abiotic factors and some of the predictors may even have direct or indirect effects/relationships. Overall, when looking at the three factors (abiotic and biotic) I chose for this assignment, they show how abitoic and bitoic interactions occur as one big community, and influence each other as an individual community or as a genus/species. 


