setwd("C:/GitHub/Johnson.1/Wk 7 asignment")
# Load the "anytime" and "ggplot2" packages to complete this week's assignment.
#Make sure to install both packages and make sure there running/checked off uner packages section. 

# Read the "Plankton_move_average" CSV in from GitHub. 
# These are data from the Great Lakes Environmental Research Laboratory plankton sampling.

data <- read.csv("Plankton_move_average.csv")

#Used the following lines to format the date and remove NAs from the dataset:
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

#The Third species (Limnocalanus) seems to have some type of influence on the first two species. Although the peaks in the plot are not the same density, all the peaks of each species occur at a very similar time. This likely means some condition is occurring that is causing them all to thrive in there environment. This condition could be an increase in food (phytolankton) within the three species environment (all three of these species are Zooplankton which feed on phytoplankton). More food allows the three species to repdocue quicker, therefore increasing population density. 

#Now copy/paste in the Lotka-Volterra function, plotting script, and load the "deSolve" package from the tutorial:

#Code from: https://www.r-bloggers.com/lotka-volterra-model%C2%A0%C2%A0intro/
library(deSolve)

# To create these plots and view in the full window, we need to remove the par() settings for multipanel plots.
# This is done with the dev.off() function
dev.off()

#create the Lotka-Volterra function we will be using with this operation.

LotVmod <- function (Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dx = x*(alpha - beta*y)
    dy = -y*(gamma - delta*x)
    return(list(c(dx, dy)))
  })
}

#Because this is not part of a package, we should spend a little more time exploring the variables to understand what each represents.

Pars <- c(alpha = 2, beta = 0.5, gamma = .2, delta = .6) #This is the line we will change
State <- c(x = 10, y = 10)#For now keep this the same.
Time <- seq(0, 100, by = 1)#For now keep this the same.
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time)) #This is the operation that creates the Lotka-Volterra model based on our specified parameters.

#The next two lines plot the model with the predator and prey against each other.
matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Rabid foxes", "Cute bunnies"), lty = c(1,2), col = c(1,2), box.lwd = 0)

# (2) - What do alpha, beta, gamma, and delta represent in this function? (4 pts)

#As we went over in class, in the Lotka-Volterra function;
#Alpha represents the rate at which prey population grows. 
#Beta represents rate of predation.
#Gamma shows that the rate of prey consumption = the population stability. 
#Delta shows that the rate of prey consumption = predator die off. 

# (3) - By only changing values for alpha, beta, gamma, and/or delta
# change the default parameters of the L-V model to best approximate the relationship between Limncalanus and D.mendotae, assuming both plots are on the same time scale.

LotVmod <- function (Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dx = x*(alpha - beta*y)
    dy = -y*(gamma - delta*x)
    return(list(c(dx, dy)))
  })
}


Pars <- c(alpha = 3, beta = 0.8, gamma = .2, delta = .8)
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time)) #This is the operation that creates the Lotka-Volterra model based on our specified parameters.

matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Limncalanus", "D.mendotae"), lty = c(1,2), col = c(1,2), box.lwd = 0)

# What are the changes you've made to alpha, beta, gamma, and delta from the default values; and what do they say in a relative sense about the plankton data? (4 pts)

#I changed alpha from 2 to 3. Alpha is the rate of population growth. 
#I changed beta from 0.5 to 0.8. Beta is a rate of prey (Daphnia mendotae) predation
#Gamma I left the same (0.2). Gamma is the rate of prey consumption = population stability. 
#I changed delta from .6 to .8 Delta is the rate of prey consumption = predator die off.
  
#Theses changes in the 4 variables say a lot about the plankton data. The rate of population growth has increased, so some condition is causing the population to grow at a faster rate. The rate of predation has decreased, so the preadtor became more effective. Rate of prey consumption equaling population stability has stayed the same. Rate of prey consumption equaling predator die off has increase, so both the coefficients have increased, but they give different effects. 


# Are there other paramenter changes that could have created the same end result? (2 pts)
  
#Yes, there are other parameter changes that could have gotten you the same, if not a similar end result. For example, if you increase gamma slightly and decrease delta slightly, and leave alpha and beta the same as the default numbers gived/used in tutorial, this will give you the same, if not a similar result to what the first parameter changes I made in the beginning of question 3.

Pars <- c(alpha = 2, beta = 0.5, gamma = .3, delta = .5)
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time)) #This is the operation that creates the Lotka-Volterra model based on our specified parameters.

matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Limncalanus", "D.mendotae"), lty = c(1,2), col = c(1,2), box.lwd = 0)


# Export your final L-V plot with a legend that includes the appropriate genus and/or species name as if the model results were the real plankton data, 
# and upload with your script. (hint - remember which one is the predator and which is the prey)




