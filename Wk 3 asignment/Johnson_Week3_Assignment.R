# (1) Approximately how many hours ahead of Sunbury was the peak flow in Lewisburg during the 2011 flood? (2 pt)
  
#During the 2011 flood, the peak flow of the flood in Lewisburg was 9 hours and 45 minutes ahead to the peak flow time in sunbury

# (2) Give one reason why information on the time between peak flow events up- and downstream could be valuable? (4 pts)

#Information on the time between peak flow events up and downstream can be extremely valuable. Lets say the Susquehanna river is rising rapidly due to lots of rain and runoff. If the site or station upstream in Lewisburg revives data that the flow rate is exceeding flood limits, they warn the citizans who live downstream that a flood is likely to occur in Sunbury in however many hours they predict. This not only allows people who live close to or on the rive to quickly get valuables out of there house, but it also can save lives of the people that did not know the flood was coming.   


# Package scavenger hunt! (12 pts each)

## (3) Using Google and ONLY packages from GitHub or CRAN:
    # Find a package that contains at least one function specifically designed to measure genetic drift.

#Install package by looking up packagage on google and doing one of the flowing:
#1. In Control pannel on middle to bottom right, click packages. Pres install on left with down arrow in grey box. Type in name of package. Press Install.package. Let it install (might take a bit). Next, in pacgages tab, check packakge(s) you want to run. Than this should show up. 
library(learnPopGen)

#2: Do Code
install.packages(username/packagename)
#user name is github useres name. Package name is 

    # Copy-paste into your script - and run - an example from the reference manual for a function within this package related to a measure of genetic drift. 
#Package called learnPopGen

#Description This function simulates genetic drift at a biallelic genetic locus with no selection and no mutation in a sexually reproducing diploid population or set of populations. It is essentially redundant with drift.selection, but in which there is no difference in relative fitness among genotypes; however, it also allows the user to visualize heterozygosity or genetic variation through time- options that are not yet implemented in drift.selection.
#Arguments p0 Ne nrep time show pause ... Starting frequency for the A allele. Effective population size. Number of replicate simulations. Total time, in number of generations, for the simulation. Various options for plotting. "p" shows the frequency of the A allele through time; "genotypes" creates an animated histogram with the frequencies of each of the three genotypes through time; "fixed" shows the fraction of populations that have fixed for each allele, a or A; "heterozygosity" plots the mean heterozygosity and the expected heterozygosity through time. The default is show="p". Pause betweengenerations. pause=0.01 (for instance) might smooth animation. optional arguments. In genetic.drift the optional arguments are presently: colors (a vector giving the colors to be used to graph the various simulations); and lwd. The plot method of the object class adds the optional argument type (e.g., "l" or "s".)
#Value:
#The function creates one of several possible plots, depending on the value of show. The function also invisibly returns an object of class "genetic.drift" that can be printed or replotted by the user using corresponding print and plot methods. (See examples.)
#Author(s): Liam Revell <liam.revell@umb.edu>

#Example

## Not run: 
genetic.drift() 
object<-genetic.drift(p0=0.7,show="heterozygosity") 
plot(object,show="genotypes") 
## End(Not run)


        # Depending on the function, either upload a plot of the result or use print() and copy/paste the console output into your script.

print(genetic.drift()) 
object<-genetic.drift(p0=0.7,show="heterozygosity") 
plot(object,show="genotypes")


    # After running the function example, manipulate a parameter within the function to create a new result. 
    
object<-genetic.drift(p0=0.475,show="fixed") 
 
        # Common options might be allele frequency, population size, fitness level, etc. 
        # Add the results of this manipulation to your script (if in the console) or upload the new plot.
print(object<-genetic.drift(p0=0.475,show="fixed"))
       
          # By manipulating these parameters you can see how it impacts the results.
          # This type of manipulation is one example of how theoretical ecology and modelling are used to predict patterns in nature.



## (4) Using Google and ONLY packages from GitHub or CRAN:
    # Find a package that will generate standard diversity metrics for community ecology, specifically Simpson's Diversity Index.

#package called vegan . Follow same steps from (3) to install package. 

    # Copy-paste into your script - and run - an example from the reference manual for a function to calculate Simpson's diversity. 

diversityresult(x, y = NULL, factor = NULL, level = NULL, 
                index=c("Simpson"), 
                method=c("pooled", "each site", "mean", "sd", "max", "jackknife"), 
                sortit = FALSE, digits = 8)

data("BCI")
data(BCI, BCI.env)
H <- diversity(BCI)
simp <- diversity(BCI, "simpson")
plot(simp, pch = 17, col = "green")

#Note**** color name must be in quotes!!!!!

        # Depending on the example usage of the function, either upload a plot of the result or use print() and copy/paste the console output into your script.

#Remember to Export plot as PDF, 7X4, Landscape

    # After running the function example, modify your script to generate another diversity metric that is NOT part of the example. 
        # If there are two diversity metrics in the example script, neither of these will count as the modified script.
        # Hint: If the function can "only" caluclate Simpson's diversity, the inverse of Simpson's diversity is another common metric. 
#This is the inverse simpsons diversity index!

data(BCI, BCI.env)
H <- diversity(BCI)
invsimp <- diversity(BCI, "inv")
plot(invsimp, pch = 7, col = "purple"
        # Add the results of this manipulation to your script (if in the console) or upload the new plot.
     
     #Remember to Export plot as PDF, 7X4, Landscape!
     
          # Diversity metrics are frequently used in community ecology for reasons ranging from a quick comparison between sites to understanding community stability.
          # Their calculation can be very tedious by hand - and very fast with a package designed for the operation.



