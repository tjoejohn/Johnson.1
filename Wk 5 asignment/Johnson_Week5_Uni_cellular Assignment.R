# First, recreate Figure 4 from Herron et al. (2019). De novo origins of multicellularity in response to predation. Scientific Reports.
  # Search datadryad.org by the paper title and download the dataset. It will include .csv files and R scripts, organized by figure.
  # Save the script and change the working directory on lines 8 and 115 to match your GitHub repository. (6 points)
  # Export and save the plot you've created. (2 points)
  # Zoom into your plot to look at the distribution for different strains.

# Do all of the strains in the plot have the same distributions (yes/no)? (2 pt)

#No.Not all strains in the plot have the same distribution. 

# Based on these observations of your strain distributions, why did the authors use a Kruskal-Wallis test rather than ANOVA to compare the strains? (2 pts)

#Although anova test can be used to compare more than two sample means, one of the KEY assumptions of the anova test is that all the data have to be normally distributed. This data is NOT normally/evenly distributed, so the authors used the Kruskal-Wallis test instead. This test can compare more than two sample means that ARE NOT nomrmally/evenly distributed   
#It compares the medians or ranks, not the truly the means.


#first, have to make sure the following packages are installed and running/box is checked!
library(fitdistrplus)
library(logspline)

# Use the fitdist() and gofstat() functions to compare the poisson, negative binomial, and logistic distributions for:
  # (1) - The number of cells of progeny (data$Num.Cells.Progeny)
  # (2) - The replication time (data$RepTime.sec)
      # 3 points each
    #HINT- "Num.Cells.Progeny" has defined breaks. To display results, use the formula with the "chisqbreaks" argument as follows:
      #gofstat(list(fit.1, fit.2, fit.3, etc), chisqbreaks=c(1,2,4,8,16,32,64))

setwd("C:/GitHub/Johnson.1/Wk 5 asignment")
data <- read.csv("Figure4Data.csv", header = TRUE)


Compare <- lm(data$Num.Cells.Progeny ~ data$RepTime.sec)

plot(data$Num.Cells.Progeny~ data$RepTime.sec, xlab ="Time", ylab ="Number Cells Progeny", main = "Compare" )
abline(Compare, col = "grey", lwd= 3)

#1 The number of cells of progeny (data$Num.Cells.Progeny)

one.col <- Compare

hist(one.col, main = "Compare")

fit.norm <- fitdist(c(na.exclude (data$Num.Cells.Progeny)), distr = "norm")

fit.nbinom1 <- fitdist(c(na.exclude (data$Num.Cells.Progeny)), distr = "nbinom")
fit.logis1 <- fitdist(c(na.exclude (data$Num.Cells.Progeny)), distr = "logis")
fit.pois1 <- fitdist(c(na.exclude (data$Num.Cells.Progeny)), distr = "pois")

gofstat(list(fit.logis1, fit.pois1, fit.nbinom1), chisqbreaks=c(1,2,4,8,16,32,64))

#2 The replication time (data$RepTime.sec)

fit.nbinom2 <- fitdist(c(na.exclude (data$RepTime.sec)), distr = "nbinom")
fit.logis2 <- fitdist(c(na.exclude (data$RepTime.sec)), distr = "logis")
fit.pois2 <- fitdist(c(na.exclude (data$RepTime.sec)), distr = "pois")


gofstat(list(fit.logis2, fit.pois2, fit.nbinom2))


# Based on the AIC scores, which distribution is the best fit for: (4 pts)
  # (1) - The number of cells of progeny (data$Num.Cells.Progeny)?
  # (2) - The replication time (data$RepTime.sec)?

#For the number of cells of progeny, the 3-mile-nbinom is the best fit since it has the lowest AIC score out of all the distributions. 
#For the replication time, 3-mile-nbinom is also the best fit since it again has the lowest AIC score out of all the distributions. 
#3 is the column number and mle (not mile) refers to maximum likelihood. The distribution is negative binomial.

# Plot a generic histogram for the replication time (data$RepTime.sec) (2 pt)

hist(data$RepTime.sec, xlab = "Repetition Time", main= "Cell Replication")


# Based on the patterns of this histograms and Figure 4:
  #Give one hypothesis for an evolutionary process represented by the two tallest bars in your histogram. (6 pts)
  # Don't cheat by looking at the paper! 
    # This hypothesis does not need to be correct - it only needs to be ecologically rational based these two figures.

#My hypothesis is that the first tallest bar at around 50,000 repetition time was the first group to have genetics that allowed them to replicate at a high frequency. Although, the environment changed after causing the bars after 50,000 to decrease in frequency. After the environment stabilized around 150,000, the 200,000 group (second tallest bar) found success again and were also able to replicate at a high frequency. 
#you have two different definitions of the time in your hypothesis that do not make sense together.





