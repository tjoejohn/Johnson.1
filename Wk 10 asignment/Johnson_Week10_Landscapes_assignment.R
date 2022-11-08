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
Diptera.csv <- read.csv("Diptera.csv", header=T)
HabitatbyPatch.csv <- read.csv("HabitatbyPatch.csv", header=T)
Clingers.csv <- read.csv("Clingers.csv", header=T)

#Format data by metrics 

PatchLatLon.mat <- as.matrix(PatchLatLon.csv[,-1])
Diptera.mat <- as.matrix(Diptera.csv)
HabitatbyPatch.mat <- as.matrix(HabitatbyPatch.csv)
Clingers.mat <- as.matrix(Clingers.csv)

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
Space.rda <- rda(Diptera.mat, as.data.frame(aem.df))
Space.r2a <- RsquareAdj(Space.rda)$adj.r.squared

aem.fwd <- forward.sel(Diptera.mat,aem.df, adjR2thresh=Space.r2a)

#To identify which variables are important, you can identify them in order:
aem.fwd$order
#Those variables can then be selected from within the original AEM data frame.
#Notice the last part of this new use of rda() - there is a third matrix for habitat data!
#To determine how much variance is explained by spatial relationships relative to local habitat we need to compare two rda results.
#One rda for space controlling for habitat, and one for habitat controlling for space.
SpaceNoHab.rda <- rda(Diptera.mat, as.data.frame(aem.df[,aem.fwd$order]), HabitatbyPatch.mat)
SpaceNoHab.rda 
anova(SpaceNoHab.rda, perm.max = 10000)
RsquareAdj(SpaceNoHab.rda)

#And the habitat controlling for space:
HabNoSpace.rda <- rda(Diptera.mat, HabitatbyPatch.mat, as.data.frame(aem.df[,aem.fwd$order]))
HabNoSpace.rda 
anova(HabNoSpace.rda, perm.max = 10000)
RsquareAdj(HabNoSpace.rda)

#SpaceNoHab - 43% Constrained and 40% Conditional. Significant. 
#HabNoSpace - Converting from scientific notation? 0.04% Constrained and 78% Unconstrained. Not Significant.
#Unconstrained is the same in each, but conditional changes.

#Functional Groups:
#We can subset the whole community by particular traits to see if they have different relationships to space or the environment.

#We will look at clingers. 
#First need to redo the variable selection so that to match this subset of the community.
ClingersSpace.rda <- rda(Clingers.mat, as.data.frame(aem.df))
ClingersSpace.r2a <- RsquareAdj(ClingersSpace.rda)$adj.r.squared

Clingeraem.fwd <- forward.sel(Clingers.mat,as.data.frame(aem.df), adjR2thresh=Space.r2a)

#The rest of this should look the same, just substituting the right AEM vectors and the swimmer data.
ClingersSpaceNoHab.rda <- rda(Clingers.mat, as.data.frame(aem.df[,Clingeraem.fwd$order]), HabitatbyPatch.mat)
ClingersSpaceNoHab.rda 
anova(ClingersSpaceNoHab.rda, perm.max = 10000)
RsquareAdj(ClingersSpaceNoHab.rda)

ClingersHabNoSpace.rda <- rda(Clingers.mat, HabitatbyPatch.mat, as.data.frame(aem.df[,Clingeraem.fwd$order]))
ClingersHabNoSpace.rda 
anova(ClingersHabNoSpace.rda, perm.max = 10000)
RsquareAdj(ClingersHabNoSpace.rda)

#ClingersSpaceHabNoHab -- 49% constrained and 25% conditional, Signifigant
#ClingersHabNoSpace -- 0.4% constrained and 70% conditional, Not significant
#Unconstrained is the same, but condiotnal values change. 

#Question 1 answer: When looking at the Diptera community, we can see that space with no habitat is significant and habitat with no space is not significant. For the Clingers community, Space with no habitat was significant and habitat with no space was not significant. Between the anova and the rda tests, we know that space had a grater influence on the Diptera and Clinger Community while habitat did not have a strong influence on these two communities.   



#Part 2: What is your interpretation of the pattern for each group individually, and the two in comparison, based on their mobility? (5 points)

#Looking at the Diptera and Clingers community individually and in comparison, the pattern I see is that Space is much better explained as well as significant compared to habitat which is not explained well and not significant. In terms of mobility, it's known that Diptera (true flies) are much more mobile than Clingers. So both of these communities likely move some within there space locally, but not a lot within there entire habitat regionally. 



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


#Lets do this for Diptera

DipteraSpace1.rda <- rda(Diptera.mat, as.data.frame(HabitatbyPatch.csv))
DipteraSpace1.r2a <- RsquareAdj(DipteraSpace1.rda)$adj.r.squared

DipteraSpace1.fwd <- forward.sel(Diptera.mat,as.data.frame(HabitatbyPatch.csv), adjR2thresh=Space.r2a)


#Now lets do this for clingers 

ClingersSpace1.rda <- rda(Clingers.mat, as.matrix(HabitatbyPatch.mat))
ClingersSpace1.r2a <- RsquareAdj(ClingersSpace1.rda )$adj.r.squared

ClingerSpace1.fwd <- forward.sel(Clingers.mat, as.matrix(HabitatbyPatch.mat), adjR2thresh=Space.r2a)

#For Diptera, Chla, depth, Flow, AveAr, and Inorg are significant. 

#For clingers, Depth and Chla are significant. 

#Part 4: How do you expect selecting both the spatial and the habitat variables would change the results of the RDAs from Part 1 above? (5 points)
  #(You do not need to redo the RDAs, unless you *want* to.)

#If I were to select both the spatial and habitat variables, I would expect this would increase the constrained(aka explained) percentage. This is because we now have another variable helping us explain our chosen bug community, and not only how they move within there space, but also within there habitat. 
