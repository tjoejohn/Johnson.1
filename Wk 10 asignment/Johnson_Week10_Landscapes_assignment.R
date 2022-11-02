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

#Part 2: What is your interpretation of the pattern for each group individually, and the two in comparison, based on their mobility? (5 points)

#Format data metrics 

PatchLatLon.mat <- as.matrix(PatchLatLon.csv[,-1])
Trichoptera.mat <- as.matrix(Trichoptera.csv)
HabitatbyPatch.mat <- as.matrix(HabitatbyPatch.csv)
Sprawlers.mat <- as.matrix(Sprawlers.csv)

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