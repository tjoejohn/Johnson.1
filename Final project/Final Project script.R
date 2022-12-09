#Set Working directory

setwd("C:/GitHub/Johnson.1/Final project")

#First we will need to download the readxl package since one of my data sets is a .xlsx

install.packages("readxl")
library(readxl)
#So now I will read in the .xlsx file "Data.xlsx"

Data1.xlsx.tibble <- read_excel("Data.xlsx")

#Beware that using the read_excel function turns the dara into a "tibble format". 
#In order to fix this, I need to do the as.data.frame function

Data1.xlsx <- as.data.frame(Data1.xlsx.tibble)

#Now that our Data.xlsx file is read in, I now need to read in the csv file "Daily_data_cod.csv"
#To do this, I will use the read.csv function 

Data2.Daily_data_cod.csv <- read.csv("Daily_data_cod.csv")

#So now we need a unique identifier to bring the two data sets together.
#However, the column names are not exactly the same or from the exact same dates...
#Thankfully we are in luck. Both of the studies my data came from were done in the same location of the South and South Eastern coast of Norway, specifically Skagerrak Norway. A specific part/area of the North sea. 
#Although location is a unique identifier, it is not already a column within  my datasets. 
#So In order to bring the two data sets together, I need to R bind. 

#In order to use the rbind() function, I need to first to the following steps:
#I will need to remove the columns I don't plan to use from the data frame 

#For my Dat1.xlsx frame:
Data.set.1 <- Data1.xlsx [,c(-3:-7, -10:-12, -14:-16)]

#For my Data2.Daily_data_cod frame:
Data.set.2 <- Data2.Daily_data_cod.csv [,c(-1:-2, -4, -6, -9:-11, -13:-17)]

#Great so now the columns I don't need are removed. 
#However, the problem is the column names of both the data sets ("Data.set.1" and "Data.set.2" are not exaclty the same")
#This means R will not currently let me rbind() the two data sets I listed above without R giving an error message.
#The solution to this is changing the column names to be the same in both data sets, without altering any of the actual data from both studies. 
# I can do this by using the following code.

#For Data.set.1
colnames(Data.set.1) <- c("Date", "Fish_ID", "Average depth_day", "Average depth_night", "Water Temperature at 1m")

#For Data.set.2 

colnames(Data.set.2) <- c("Fish_ID", "Date", "Average depth_day", "Average depth_night", "Water Temperature at 1m")


#So now we can finally bring both data sets together by using the rbind() function . 
?rbind

Master_Cod_data <- rbind(Data.set.1, Data.set.2)

head(Master_Cod_data)

#Yay! Now I have my two data sets, with only the columns I want to use, brought together in one data set called "Master_cod_data". 
#The only issue now is that their are some NA's in the Average depth_day and Average depth_night columns. 
#However, the NA's still have data in the Date and Water tempurtaure at 1m columns that is stil usefull for what im trying to do. 
#So, I will make all the NA's a value of 0 in the Average depth_day and Average depth_night columns with the following code.

Master_Cod_data <- subset(Master_Cod_data, Master_Cod_data$`Average depth_night` != "NA")
Master_Cod_data <- subset(Master_Cod_data, Master_Cod_data$`Average depth_day` != "NA")



Master_Cod_data$`Average depth_day` <- ifelse(is.na(Master_Cod_data$`Average depth_day`), Master_Cod_data$`Average depth_night`, Master_Cod_data$`Average depth_day`)
Master_Cod_data$`Average depth_night` <- ifelse(is.na(Master_Cod_data$`Average depth_night`), Master_Cod_data$`Average depth_day`, Master_Cod_data$`Average depth_night`)

head(Master_Cod_data)

#Within my Not_Master_Cod_data , I have 2 columns I want to make into one column.I want to make Average depth_day and Average dpeth_night into one collumn called Average_depth. 
#I want to do this in order to smooth out tghe process of making my figures and running certain tests later on. 
#To do this, I need to run the following code:


Master_Cod_data$`Average depth_day`<-as.numeric(as.character(Master_Cod_data$`Average depth_day`))
Master_Cod_data$`Average depth_night`<-as.numeric(as.character(Master_Cod_data$`Average depth_night`))

Master_Cod_data$new <- (Master_Cod_data$`Average depth_day` + Master_Cod_data$`Average depth_night`)/2


#This created the depth data as a column titled new in the Master_Cod_data frame. So now I don't need the Average depth_day and Average depth_night columns. So lets get rid of those two columns in a new data frame. 

Master_Cod_data2 <- Master_Cod_data [,c(-3:-4)]

#In addition, I need to rename the column titled "new" "Depth". 

colnames(Master_Cod_data2) <- c("Date", "Fish_ID", "Water_Temperature_at_1m", "Depth")


#Now I can move on to making the figures. 

#Firgure 1:
#The first test/figure i would like to make is a nonlinear mdoel (GAM) or a generalized liner mixed model (GLMM) depending on how the scatterplots and or resiudals look and if their linear or not. 
#In order to do this, the first step is to install the following packages.  

install.packages("mgcv")
library(mgcv)

install.packages("MASS")
library(MASS)

install.packages("MuMIn")
library(MuMIn)

#NOTE*Make random effect Fish_id!

#*Note: Y first, than X. 

#Generalized Linear Mixed Model
#Use gaussian family. This is the only family that worked when I tried doing all the family's for this glmm.  

glmm.mod <- glmmPQL(Depth~Water_Temperature_at_1m, family = gaussian, random = ~ 1 | Fish_ID, data = Master_Cod_data2)

r.squaredGLMM(glmm.mod)

summary(glmm.mod)

plot(glmm.mod)


hist(Master_Cod_data2$Water_Temperature_at_1m)

#Generalized addative mixed model 

gam.mod1 <- gam(Depth~Water_Temperature_at_1m, family = gaussian, random = list(Fish_ID=~ 1), data = Master_Cod_data2)

summary(gam.mod1)

plot(gam.mod1$residuals)

r.squaredGLMM(gam.mod1)


#Change family!!!
?family

#Figure 1 messing around/experimenting:

r.squaredGLMM(glmm.mod)


# Notice the error? This is the problem with trying to fit a non-normal distribution into a linear model. It doesn't always work
# Between this error and our pattern in the residuals, it's time to try a non-linear option.

# This red message isn't an error - it's just notifying us the function has been updated from it's original version.
#The R squared gives us two results - the R2m is "marginal"
# This is the R-squared we really care about. It represents the variance explained by "fixed" effects.
# These are the ones we have "fixed" in the model - the usual (x) values.

# The R2c value is the "conditional" variance explained by the random effect of individual
# So this is saying the response is much more strongly predicted by boldness of the individual squirrel 
# rather than the presence of a human or other predator. 

# We can compare our AIC scores side-by-side now:
AIC(gam.mod1, gam.mod2)

# The interactive model is a better fit for the data with both a higher R-squared and lower AIC, so that would be the best approximation of these data.


#Figure 2:
#The second thing I would like to look at is how the depth of the cod is changed over time. 
#To do this I will again either use a a nonlinear mdoel (GAM) or a generalized liner mixed model (GLMM) depending on how the scatterplots and or resiudals look and if their linear or not. 
#The necessary packages should be installed and running before. 
#However, with my data frame "Master_Cod_data2", under the column date, the year, month, and day are listed in each row.
#For this figure, I only would like to use year. I already have lots of data points, so using year on the x axis, as opposed to month or day, will hopefully create less confusion for the reader in my results and discussions part of this project
#That means I have to extract year from date and month already in the "Date" column of the "Master_Cod_data2" data frame. 
#So to do this, I have to run the followng code:

print ("Master_Cod_data2")
Master_Cod_data2

Master_Cod_data2$Date <- as.Date(Master_Cod_data2$Date)
print("Extract year")

Master_Cod_data2$Date <- as.numeric(format(Master_Cod_data2$Date, "%Y"))
Master_Cod_data2

#Now we have Year extracted and alone, but it's under the date column. I should rename it to Year instead. 

colnames(Master_Cod_data2) <- c("Year", "Fish_ID", "Water_Temperature_at_1m", "Depth")
#Now I can create my model. 


glmm.mod2 <- glmmPQL(Year~Depth, family = gaussian, random = ~ 1 | Fish_ID, data = Master_Cod_data2)


r.squaredGLMM(glmm.mod2)

summary(glmm.mod2)

plot(glmm.mod2)


gam.mod2 <- gam(Year~Depth, family = gaussian, random = list(Fish_ID=~ 1), data = Master_Cod_data2)

summary(gam.mod2)

# Notice in the interactive model the r-squared went from 0.27 to 0.38, which is a good sign. 
# But we've also added quite a few interactive predictors.

plot(gam.mod2$residuals)

AIC(gam.mod2)

#Left off here. 
#Seems like for figure 1, glmm.mod is better. (I think)
#For figure 2, I;m not sure weather glmm.mod2 is better of gam.mod2 is better. Leaning towards gam.mod2.
#gam.mod2 has higher R squared. so might be better.
#DELETE THE 4 LINES ABOVES WHEN DECIDE!!!

#Figures should be part of your paper.

