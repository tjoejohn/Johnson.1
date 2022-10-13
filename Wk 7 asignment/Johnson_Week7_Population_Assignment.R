setwd("C:/GitHub/Johnson.1/Wk 7 asignment")
# Load the "anytime" and "ggplot2" packages to complete this week's assignment.
#Make sure to install both packages and make sure there running/checked off uner packages section. 

# Read the "Plankton_move_average" CSV in from GitHub. 
# These are data from the Great Lakes Environmental Research Laboratory plankton sampling.

data <- read.csv("Plankton_move_average.csv")

#Used the following lines to format the date and remove NAs from the dataset:
#(plot populations of species??)
data$Date <- as.Date(data$Date, origin = "0001-01-01") # Setting values to "day zero".
data <- na.omit(data)

#Plot these population data over time with the following code:
ggplot(data)  +
  xlab("Numeric Date") + ylab("Density Individuals")+
  geom_line(data=data, aes(Date, D.mendotae), color="black", alpha = 0.7, size=1)+
  geom_line(data=data, aes(Date, LimncalanusF+LimncalanusM), color="orange",  alpha = 0.7, size=1)+ # adding males and females together, hint: this is actually spelled Limnocalanus
  geom_line(data=data, aes(Date, Bythotrephes), color="sky blue",  alpha = 0.7, size=1)+
  geom_line(data=data, aes(Date, Bythotrephes), color="sky blue",  alpha = 0.7, size=1)+
  theme_bw() 

# Export this plot to have on hand for reference in the next section of the assignment (and upload with your script).

# (1) - Which species is most likely to be r-selected prey and which its primary predator? (2 pts)

#The species that is most likely to be r-selected as prey is the D.mendotae(aka Daphnia) and it's primary predator is the Bythotrephes. 

# What is one relationship the third species MIGHT have to the first two? (2 pts)
#This might make more sense after Thursdays (10/13/22) class. 
***#The Third species (Limnocalanus seems to have some type of influence on the first two species. Although the peaks in the plot are not the same density, all the peaks of each species are at a very similar time. This likely means some condition (come up with (soemthing spacial or an envriromental conditOion) is occurring that is causing them all to trhve in there envrionment.

#Now copy/paste in the Lotka-Volterra function, plotting script, and load the "deSolve" package from the tutorial:

# (2) - What do alpha, beta, gamma, and delta represent in this function? (4 pts)

# (3) - By only changing values for alpha, beta, gamma, and/or delta
# change the default parameters of the L-V model to best approximate the relationship between Limncalanus and D.mendotae, assuming both plots are on the same time scale.
# What are the changes you've made to alpha, beta, gamma, and delta from the default values; and what do they say in a relative sense about the plankton data? (4 pts)
# Are there other paramenter changes that could have created the same end result? (2 pts)
# Export your final L-V plot with a legend that includes the appropriate genus and/or species name as if the model results were the real plankton data, 
# and upload with your script. (hint - remember which one is the predator and which is the prey)




