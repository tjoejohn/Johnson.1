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

colnames(abiotic.means2)
ord <- rda(invert.means2 ~ Al, abiotic.means2)
ord

#This one worked!
colnames(abiotic.means2)
ord <- rda(invert.means2 ~ pH + totalN + Perc_ash + Kalium + Magnesium + Ca + Al, abiotic.means2)
ord

colnames(abiotic.means2)
ord <- rda(invert.means2 ~ pH + totalN + Perc_ash, abiotic.means2)
ord

colnames(abiotic.means2)
ord <- rda(invert.means2 ~ pH + totalN + Perc_ash + Kalium, abiotic.means2)
ord

anova(ord)

#P value is not significant, or not even given in this case, so need to take steps further to remove predictors that are not important one at a time!

plot(ord, ylim = c(-2,2), xlim = c(-5,5))

ord <- rda(invert.means2 ~., abiotic.means2) # shorthand for the model that includes everything.
ord.int <- rda(invert.means2 ~1, abiotic.means2)

step.mod <- ordistep(ord.int, scope = formula(ord), selection = "both")
step.mod$anova

step.R2mod <- ordiR2step(ord.int, scope = formula(ord), selection = "forward")


#(Q2 - 12 pts) Then use the dataset from the tutorial to create a linear model related to your RDA. Try multiple predictors to find the best fit model.
  # Explain the ecological importance of the significant predictors, or lack of significant predictors.


#******* (FIX ONECE Q1 ISSUE IS FIXED) It seems that Nitrogen has the best the best influencer of the invertebrate community. 
#The data (abitoic.means2) is already merged with the data (invert.means2) from question 1. 

#ZZZZZZZZZZZZZZZZZ ************* DELETE?
data_experiment_urtica.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Data_experiment_urtica")
data.experiment <- as.data.frame(data_experiment_urtica.tibble)

#Then create a column called "Parcel" that allows us to merge with the plants data frame based on Parcel numbers.
abiotic.means$Parcel <- unique(abiotic$Parcel)

# So instead of megrring both by group.1 like before, now we merge by "Parcel" and accept all other defaults.
data.experiment <- merge(abiotic.means, data.experiment, by = "Parcel")

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

*******#For some reason fit.nbinom and fit.geom wont run?????:
#Error in fitdist(abiotic.invert$totalN, distr = "geom") : 
  #the function mle failed to estimate the parameters, 
                #with the error code 100

#Use AIC comparisons of distributions to see which is best:
#call from:
gofstat(list(fit.weibull, fit.norm, fit.gamma, 
             fit.lnorm, fit.logis))
#Weibell is the best fit?????
colnames(data.experiment)

mod1 <- lm(Length_main_stem ~ pH + totalN + Kalium + Magnesium + Ca + Al + TotalP + Land_use + Biomass, data.experiment)
summary(mod1)
anova(mod1)
AIC(mod1)


# (Q3 - 6 pts) Provide a 3-4 sentence synthesis of how these results relate to one another and the value of considering both together for interpreting biotic-abiotic interactions.


