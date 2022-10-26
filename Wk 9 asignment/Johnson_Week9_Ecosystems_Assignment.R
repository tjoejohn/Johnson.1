# For this week it's time to start exploring your own ideas and questions in R.
  # There are at least five options in the dataset to create the following comparisons.

setwd("C:/GitHub/Johnson.1/Wk 9 asignment")
getwd()

# (Q1 - 12 pts) Use the dataset from the tutorial to complete one redundancy analysis (RDA) with variance partitioning on a different community (NOT the nematodes).
  # Explain the ecological importance of your significant predictor variables, or the importance if none are significant for your community.

#Must install/read in read_excell package fist!

library(readxl)

#Next read in excel file and untransfrom data from tibble format using as.data.frame

vegitation.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Vegetation_transects")
vegitation <- as.data.frame(vegitation.tibble)

invert.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Invertebrate_community")
invert <- as.data.frame(invert.tibble)

head(vegitation)
head(invert)

#There is a different amount of samples per invertabrete_community compared to Vegitation_transects. Run the following code to take an average/mean and make them even!

vegitation.names <- paste(vegitation$Parcel, vegitation$Landuse)
vegitation$names <- vegitation.names

head(invert)

#Do the same for the invertebrate(invert) community! 

invert.names <- paste(invert$Parcel, invert$Landuse)
invert$names <- invert.names

#Agregate means by names to columns 

vegitation.means <- aggregate(x = vegitation, by = list(vegitation$names), FUN = "mean")

invert.means <- aggregate(x = invert, by = list(invert$names), FUN = "mean")

#Remove NA columns 
invert.means1 <- invert.means[,-73] 
head(invert.means1)
invert.means2 <- invert.means1[,-1:-3]
head(invert.means2)
# Make sure everything is numeric.
invert.means2 <- sapply(invert.means2, as.numeric ) 
#Make sure everything is in the right format!
invert.means2 <- as.data.frame(invert.means2) 

#Do same thing again, except for vegetation!

# (Q2 - 12 pts) Then use the dataset from the tutorial to create a linear model related to your RDA. Try multiple predictors to find the best fit model.
  # Explain the ecological importance of the significant predictors, or lack of significant predictors.

# (Q3 - 6 pts) Provide a 3-4 sentence synthesis of how these results relate to one another and the value of considering both together for interpreting biotic-abiotic interactions.


