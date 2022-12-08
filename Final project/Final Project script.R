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

#The first test/figure i would like to make is a nonlinear mdoel (GAM). 
#In order to do this, the first step is to install the mgcv package. 

install.packages("mgcv")
library(mgcv)

#Make random effect fish id!

#Note: Y first, than X. 

gam.mod1 <- gam()

glmm.mod <- glmmPQL(Flight.initiation.distance..FID.~Object, family = gaussian, random = ~ 1 | ID, data = df)


gam.mod1 <- gam(Flight.initiation.distance..FID.~Object + Area, family = Gamma, random = list(ID=~ 1), data = df)

gam.mod1 <- gam(Water_Temperature_at_1m~Depth, family = gaussian, random = list(Fish_ID=~ 1), data = Master_Cod_data2)

#Change family!!!
?family

