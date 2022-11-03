# Load the packages from this week's tutorial.
#In the tutorial we looked at the community as a whole and the swimmers which ultimately matched a prediction we had for their distribution.

setwd("C:/GitHub/Johnson.1/Wk 10 asignment")

library(spdep)
library(adespatial)
library(vegan)

#Part 1: Look at two other subsets of the community and determine the relative influence of space and habitat on each following the methods in the tutorial. (10 points)
#The options include groupings by taxonomy, where Diptera (true flies) have the strongest flight ability, Trichoptera the 2nd strongest, 
    #Ephememeroptera are 3rd, and non insects are 4th...because they don't have wings.
#Groupings by habits include the swimmers (off limits for the assignment) as most mobile, sprawlers as 2nd (they move in search of food, but not quickly),
    #and the clingers come in last (they might move up and down on individual rocks).

#He used bugs by patch and swimmers in the tutorial, so can't use those two excels. Must read in lat lon and HabitatbyPatch csv by well. 

PatchLatLon.csv <- read.csv("PatchLatLon.csv", header=T)
Trichoptera.csv <- read.csv("Trichoptera.csv", header=T)
HabitatbyPatch.csv <- read.csv("HabitatbyPatch.csv", header=T)
Sprawlers.csv <- read.csv("Sprawlers.csv", header=T)

#Format data by metrics 

PatchLatLon.mat <- as.matrix(PatchLatLon.csv[,-1])
Trichoptera.mat <- as.matrix(Trichoptera.csv)
HabitatbyPatch.mat <- as.matrix(HabitatbyPatch.csv)
Sprawlers.mat <- as.matrix(Sprawlers.csv)

#Now build networks 

nb<-cell2nb(3,30,"queen") #three columns, 30 rows long, 90 nodes.
nb1 <- droplinks(nb, 19, sym=TRUE) #these drop specific values from the network based on missing data points.
nb2 <- droplinks(nb1, 22, sym=TRUE)
nb3 <- droplinks(nb2, 25, sym=TRUE)
nb4 <- droplinks(nb3, 30, sym=TRUE)

#Map the empty network onto real lat/lon data, aka sampling locations.
bin.mat <- aem.build.binary(nb4, PatchLatLon.mat, unit.angle = "degrees", rot.angle = 90, rm.same.y = TRUE, plot.connexions = TRUE)
#How does this network compare to the real lat/lon of the points?
plot(PatchLatLon.mat[,2]~PatchLatLon.mat[,3], xlim = rev(c(76.75,77)))

#Now weight the relationships between these real locations on a prescribed network:
aem.ev <- aem(aem.build.binary=bin.mat)
#Remove the rows where links were dropped and focus on vector data frame
aem.df <- aem.ev$vectors[c(-19,-22,-25,-30),]
#this creates a LOT of variables. How do we choose which to use?
aem.df


#We will use forward selection in this case because of the sheer number of variables.
Space.rda <- rda(Trichoptera.mat, as.data.frame(aem.df))
Space.r2a <- RsquareAdj(Space.rda)$adj.r.squared

aem.fwd <- forward.sel(Trichoptera.mat,aem.df, adjR2thresh=Space.r2a)

#To identify which variables are important, you can identify them in order:
aem.fwd$order
#Those variables can then be selected from within the original AEM data frame.
#Notice the last part of this new use of rda() - there is a third matrix for habitat data!
#To determine how much variance is explained by spatial relationships relative to local habitat we need to compare two rda results.
#One rda for space controlling for habitat, and one for habitat controlling for space.
SpaceNoHab.rda <- rda(Trichoptera.mat, as.data.frame(aem.df[,aem.fwd$order]), HabitatbyPatch.mat)
SpaceNoHab.rda 
anova(SpaceNoHab.rda, perm.max = 10000)
RsquareAdj(SpaceNoHab.rda)

#And the habitat controlling for space:
HabNoSpace.rda <- rda(Trichoptera.mat, HabitatbyPatch.mat, as.data.frame(aem.df[,aem.fwd$order]))
HabNoSpace.rda 
anova(HabNoSpace.rda, perm.max = 10000)
RsquareAdj(HabNoSpace.rda)

#Bewllow is from turorial to help you answer question 1!
#Now look at the variance explained by each:
#Unconstrained variance is the same in each, but the constrained and conditional values change!
#SpaceNoHab - 46% constrained and 26% conditional
#HabNoSpace - 4.9% constrained and 67% conditional
#How is this possible? Some variance can only be explained by the synergistic relationships of habitat that varies predictably with space.

#Part 2: What is your interpretation of the pattern for each group individually, and the two in comparison, based on their mobility? (5 points)

#Format data metrics 



#Part 3: For each of your chosen groups of bugs, perform variable selection for the habitat data rather than the AEM data. Which habitat variables are significant for each? (10 points)
  # Definitions for the habitat column names:
    #Inorg = total suspended inorganic solids in the water column
    #Organ = total suspended organic solids in the water column
    #Chla = Chlorophyll a concentration from benthic rocks collected in-stream
    #BOM = total benthic organic matter in the sample
    #Depth = water depth
    #Flow	= water velocity where samples were collected
    #Fines = Percent of the substrate as "fines" i.e. small particles too small to measure
    #AveAr = The average size of rocks where each sample was collected


#Part 4: How do you expect selecting both the spatial and the habitat variables would change the results of the RDAs from Part 1 above? (5 points)
  #(You do not need to redo the RDAs, unless you *want* to.)