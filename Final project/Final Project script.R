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
#Thankfully we are in luck. Both of the studies my data came from were done in the same location off of the South and South Eastern coast of Norway, specifically Skagerrak Norway. A specific part/area of the North sea. 
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

#Lasly before i start making the figures, it's important to note what units were used in this study, especially ll be analyzing numbers with units in my figures. 
#In this study, temperature was measured in ℃, and Depth (that the fish were found at within the water column) was m(meters).
#Now I can move on to making the figures. 

#Anlysis 1:
#The first test/anyliss I would like to make is a generalized liner mixed model (GLMM). 
#In order to do this, the first step is to install the following packages.  

install.packages("mgcv")
library(mgcv)

install.packages("MASS")
library(MASS)

install.packages("MuMIn")
library(MuMIn)


#*Note: Y first, than X. 

#Generalized additive mixed model 

#Use Gaussian family. This is the only family that worked when I tried doing all the family's for this glmm.  
#NOTE*, that for the gam, the random effect is Fish_id!

#Lastly, I can plot the residuals of this gam. The axis names and the color/visability of the points look good, but I should still add a figure title, chnage the point size, and point shape. 


gam.mod1 <- gam(Depth~Water_Temperature_at_1m, family = gaussian, random = list(Fish_ID=~ 1), data = Master_Cod_data2)

summary(gam.mod1)

r.squaredGLMM(gam.mod1)

#         R2m         R2c
#[1,] 0.001331264 0.001331264

#Figure 1: A residuals plot showing the residuals from my analysis 1, which is a generalized additvie mixed model.
#Make sure to add trend line, choose colors that are easy for reader to see, and change point size and trend line size. 

plot(gam.mod1$residuals, main = "Figure_1", xlab = "Predicted Value" ,ylab = "Residuals", pch = 21, cex = 1.2) 
abline(0,0, lwd = 2.5, col = "red")

#Analysis 2 :
#The second thing I would like to look at is how the depth of the cod is changed over time. 
#To do this I will again either use a glmm,or a generalized liner mixed model (GLMM).
#However, with my data frame "Master_Cod_data2", under the column date, the year, month, and day are listed in each row.
#For this analysis, and for my second figure later on, I only would like to use year. I already have lots of data points, so using year on the x axis, as opposed to month or day, will hopefully create less confusion for the reader in my results and discussions part of this project.
#That means I have to extract year from date and month already in the "Date" column of the "Master_Cod_data2" data frame. 
#So to do this, I have to run the following code:

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
           #R2m       R2c
#[1,] 0.0003541281 0.9851525

summary(glmm.mod2)


#Figure 2: 
#For my second figure, I would like to make a histogram to show the frequency of the Atlantic Cod at Certain water temperatures. 
#To makae this histogram, I have to intsall the following package. 

install.packages("ggplot2")
library(ggplot2)

#To make this histogram, run the following code. 

ggplot(Master_Cod_data2, aes(x = Water_Temperature_at_1m)) +
  geom_histogram(color = "dark grey", fill = "dodgerblue3", bins = 20) +
  labs(x = "Water Temperature at 1m (Degree Celsius)", y = "Count", title = "Figure_2") +
  geom_vline(aes(xintercept = mean(Master_Cod_data2$Water_Temperature_at_1m, na.rm = TRUE), color = "mean"), show.legend = TRUE, size = 2) +
  scale_color_manual(name = "Legend", values = c(mean = "red"))

#Figure 3: 
#For my third figure, I will be doing a scatter plot. I want to do this to show the relation and the significance of how the depth cod are found at in the ocean has changed over time, rather than just the residuals which I showed in figure 2. 
#In order to do this, I will need to make a new data frame that specifies which columns I would like to use from within the Master_Cod_data2 data frame.  

Figure_3 <- lm(Master_Cod_data2$Depth ~ Master_Cod_data2$Year)

#Now I can make this scatter plot. Make sure to name the main figure, give each axis a clear name, and change the point size as well a shape. 
#I will also add a trend line using the abline function to make the trend clear to the reader in my relsuts and discussion section of my paper. 
#Also for the tend line, I will change it's color and size to make it easier to see for the reader. 

plot(Master_Cod_data2$Depth ~ Master_Cod_data2$Year, xlab ="Year", ylab ="Depth (m)", main = "Figure_3", pch = 21, cex = 1.6)
abline(Figure_3, col = "cyan4", lwd= 3)

#Note, I tried doing this with a ggplot as well, but it pretty much looks the same as the base model plot. So I will stick with with base model plot. 

#Analysis 3: 
#For my third analysis, I will use a linear model to look at Water temperature at 1m and Year. 

Mod.lm <- lm(Master_Cod_data2$Water_Temperature_at_1m ~ Master_Cod_data2$Year)

anova(Mod.lm)

#This is significant!!!
#The anova shows that that year is significant. 

#Figures should be part of your paper

