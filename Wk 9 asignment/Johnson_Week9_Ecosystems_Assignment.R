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
#I would have run with Al and Ca together to see if the result was any different based on this output - what you did is fine though.

ord2 <- rda(invert.means2 ~ Al, abiotic.means2)
ord2
anova(ord2)
plot(ord2)

#Question 1 answer: Even after removing a couple of non significant predictors from my abiotic factors data frame, none of the predictors p values were statistically significant. This tells us that the predictors I used don't exlplain eachother and the community well.  

#(Q2 - 12 pts) Then use the dataset from the tutorial to create a linear model related to your RDA. Try multiple predictors to find the best fit model.
  # Explain the ecological importance of the significant predictors, or lack of significant predictors.

#Now read in new sheet from datatset "Data_experiment_urtica"
data_experiment_urtica.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Data_experiment_urtica")
data.experiment <- as.data.frame(data_experiment_urtica.tibble)

abiotic.means <- aggregate(x = abiotic, by = list(abiotic$Parcel), FUN = "mean")

data.experiment <- aggregate(x = data.experiment, by = list(data.experiment$Parcel), FUN = "mean")

# So instead of megring both by group.1 like before, now we merge by "Parcel" and accept all other defaults.
data.experiment.merged <- merge(abiotic.means, data.experiment, by = "Parcel")


# Let's take a quick look at our data.
# This data frame is getting large enough that View() might be more helpful than head() for an initial look:
View(data.experiment.merged)

#Install these packages to do a distribution of our y (Length_main_stem)
library(fitdistrplus)
library(logspline)


#Plot distribution against ideals
##fit all possible/likely distribution models and check AIC/BIC
fit.weibull <- fitdist(data.experiment.merged$Length_main_stem, distr = "weibull")
fit.norm <- fitdist(data.experiment.merged$Length_main_stem, distr = "norm")
fit.gamma <- fitdist(data.experiment.merged$Length_main_stem, distr = "gamma")
fit.lnorm <- fitdist(data.experiment.merged$Length_main_stem, distr = "lnorm")
fit.nbinom <- fitdist(data.experiment.merged$Length_main_stem, distr = "nbinom")
fit.logis <- fitdist(data.experiment.merged$Length_main_stem, distr = "logis")
fit.geom <- fitdist(data.experiment.mergedt$Length_main_stem, distr = "geom")

#For some reason fit.nbinom and fit.geom wont run?????:
Error in fitdist(abiotic.invert$totalN, distr = "geom") : 
  the function mle failed to estimate the parameters, 
                with the error code 100

#Use AIC comparisons of distributions to see which is best:
#call from:
gofstat(list(fit.weibull, fit.norm, fit.gamma, 
             fit.lnorm, fit.logis))
#Weibell is the best fit
colnames(data.experiment)

mod1 <- lm(Length_main_stem ~ pH + totalN + Kalium + Magnesium + Ca + Al + Flowering + Land_use + Parcel,data.experiment.merged)
summary(mod1)
anova(mod1)
AIC(mod1)

summary(mod1)$adj.r.squared

#Have way to many NA's  for parcel and Land_use.Thats why this won't run. Now we have to remove variables that are unimportant. 
#Remove Parcel, Land_Use, and Flowering. 

mod2 <- lm(Length_main_stem ~ pH + totalN + Kalium + Magnesium,data.experiment.merged)
summary(mod2)
anova(mod2)
AIC(mod2)

summary(mod2)$adj.r.squared

#R squared is small, in fact its negative. The model is over fitted, so try adding variables. 

#Add Biomass

mod3 <- lm(Length_main_stem ~ pH + totalN + Kalium + Magnesium + Biomass,data.experiment.merged)
summary(mod3)
anova(mod3)
AIC(mod3)

summary(mod3)$adj.r.squared

#Wow, that made the R squared a lot better. Lets try adding one more thing and see what that does. 

mod4 <- lm(Length_main_stem ~ pH + totalN + Kalium + Magnesium + Biomass + Al,data.experiment.merged)
summary(mod4)
anova(mod4)
AIC(mod4)

summary(mod4)$adj.r.squared

#Huh.. That did nothing to our R squared. Lets test to see if there is a relationship between biomass and aluminum. 

mod5 <- lm(Length_main_stem ~ pH + totalN + Kalium + Magnesium + Biomass*Al,data.experiment.merged)
summary(mod5)
anova(mod5)
AIC(mod5)

summary(mod4)$adj.r.squared

#Our AIC score decreased. This means there is probably a relationship between biomass and Aluminum(Al), and Kalium. R squared stayed the same.


#I think its safe to say that mod5 is the best fit. It has the lowest AIC score, and the R squared in mod3 is the best and closest to 1 out of all the mod's I tried. 
#Where is mod5? The last model you have here (mod4) still has too many predictors in it. Only biomass is important so all the others should be removed.

#Question 2 answer: Unlike question 1, there was one predictor value that was very statistically significant which was Biomass. This means that Biomass is very ecologically important. This makes sense as well. The more biomass you have the longer the length of ther stem is! 

# (Q3 - 6 pts) Provide a 3-4 sentence synthesis of how these results relate to one another and the value of considering both together for interpreting biotic-abiotic interactions.

#When we look at my results, I used abiotic factors, as well as invertebrate_community for the rda and abitoic factors as well as Data_experiment_urtica for the linear model in this assignment. Even though these rebuts may differ within there community, the biotic factors in this case are all one big community that can be limited by abiotic factors. When we consider both the invertribate_community and Data_experiment_urtica, we obtain valuable ecological information and see that both communities are influenced by some (predictors) more than others, including abiotic and biotic factors (aka the predictors) that may even have relationships between one another. Overall, when looking at the data (abiotic and biotic) I chose for this assignment, they show how abitoic and bitoic interactions occur as one big community, and influence each other as an individual community or as a genus/species. 
#But you showed the abiotic factors had no effect at all! The only effect was biotic on biotic.

