# Look at the plot and model results for our Dryad data in the tutorial. Part 1: Without knowing which points represent which groups, 
  # give one explanation for why these data might be difficult to draw spatial inferences about genes.(3 points)

#It's hard to draw spatial inferences about genes without knowing which points represent which groups because the net1 plot is a flowchart per say, and you can't determine the spacial inferences. 

  # Part 2: Despite the drawbacks, give the result or interpretation that you feel most confident in (3 points), and EXPLAIN WHY (4 points).

#Despite the drawbacks, I can compare data points on a relative scale. This is because I can infer that Seq24 and Seq16 are 10 points/dots away from each other as well as Seq5 and Seq16. All the other Seq points/dots are less than 10 points apart from each other.

# For your scripting assignment we will use the "ge_data" data frame found in the "stability" package.
  # Install the "stability" package, load it into your R environment, and use the data() function to load the "ge_data". (2 points)

#To install the ge_data package, click the packages tab in the bottom right window. 
#Nextr, hit the install button which is the gray box with a down facing arrow, and Search for "stability". Once you find it, select the pakage and press isnstall.
#Lastly, in the packages tav, serch "stability in the serch bar and click the box next to the package name so it has a check box. 
#You must do this so you can run the package. It worked if > library(stability) comes up in the code window. 
#Now to load the specific data frame you want to work with use this code:
data(ge_data)


# Create two linear models for Yield Response: one related to the Environment and one to the Genotype. (2 points each)
  # 'Yield Response' in this dataset is a measure of phenotype expression.
  # Hint: Look at the help file for this dataset.
?ge_data
?lm

Kyle <- lm(ge_data$Yield ~ ge_data$Env)
Busch <- lm(ge_data$Yield ~ ge_data$Gen)
# Test the significance of both models and look at the model summary. (3 points each)
anova(Kyle)
summary(Kyle)

anova(Busch)
summary(Busch)

  # Which model is a better fit to explain the yield response, and WHY? (6 points)
  # Hint: Does one model seem more likely to be over-fitted?

#I belive the data that yields Environment (the linear model I named "Kyle") is a better fit to explain the yield response. This is because the R squared value (a measure of best fit) is larger (which means a stronger relationship) than the linear model that yields genotype (the linear model named Busch). 

# Which environment would be your very WORST choice for generating a strong yield response? (2 points)

#The environment that would be the worst for generating a strong yield response would be the ge_data$EnvSargodht environment. This is because the Pr(<|t|) value (which is basically a probability or p value) is much higher compared to the other environments, and therefore it would not generate a strong yield response. 

#Always rember to set working directory!!!!!!!!
setwd("C:/GitHub/Johnson.1/Wk 4 asignment")
getwd()