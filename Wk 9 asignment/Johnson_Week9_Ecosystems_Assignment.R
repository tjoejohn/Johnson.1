# For this week it's time to start exploring your own ideas and questions in R.
  # There are at least five options in the dataset to create the following comparisons.

setwd("C:/GitHub/Johnson.1/Wk 9 asignment")
getwd()

# (Q1 - 12 pts) Use the dataset from the tutorial to complete one redundancy analysis (RDA) with variance partitioning on a different community (NOT the nematodes).
  # Explain the ecological importance of your significant predictor variables, or the importance if none are significant for your community.

#Must install/read in read_excell package fist!

library(readxl)
colnames(vegitation.means2)
ord <- rda(invert.means2 ~ Urtica_dioica + Hedera_helix +	Corylus_avellana + Glechoma_hederacea	+ Aegopodium_podagraria	+ Angelica_sylvestris	+ Polygonatum_multiflorum	+ Paris_quadrifolia	+ Ranunculus_ficaria	+ Primula_elatior	+ Carex_sylvatica	+ Brachypodium_sylvaticum	+ Athyrium_filixfemina + Dryopteris_filixmas + Dryopteris_carthusiana + Deschampsia_cespitosa + Acer_campestre + Geum_urbanum	+ Rubus_fruticosus + Geranium_robertianum	+ Ilex_aquifolium	+ Circaea_lutetiana +	Filipendula_ulmaria	+ Festuca_gigantea +	Arum_maculatum + Anthriscus_sylvestris + Salix_sp	Lamium purpureum + Lamium_album	Lamium_galeobdolon + Heracleum_sphondylium + Quercus_robur + Fraxinus_excelsior	+ Acer_pseudoplatanus	+ Ribes_rubrum + Ranunculus_acris	+ Ribes_uva-crispa, vegitation.means2)


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
head(vegitation.means)

vegitation.means1 <- vegitation.means[,-41] 
head(vegitation.means1)
vegitation.means2 <- vegitation.means1[,-1:-3] 
head(vegitation.means2)
# Make sure everything is numeric.
vegitation.means2 <- sapply(vegitation.means2, as.numeric ) 
#Make sure everything is in the right format!
vegitation.means2 <- as.data.frame(vegitation.means2) 

#Install/read in this package and compare against vegitation transects (abiotic) against invertebrate communities (biotic) 
library(vegan)



# (Q2 - 12 pts) Then use the dataset from the tutorial to create a linear model related to your RDA. Try multiple predictors to find the best fit model.
  # Explain the ecological importance of the significant predictors, or lack of significant predictors.

# (Q3 - 6 pts) Provide a 3-4 sentence synthesis of how these results relate to one another and the value of considering both together for interpreting biotic-abiotic interactions.


