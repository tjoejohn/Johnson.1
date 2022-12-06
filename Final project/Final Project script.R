#Set Workin g directory

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
#Although, location is not already a column within  my datasets. 
#To do this, I need to R bind. 
#See week 1 stuff to do this!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#First I will need to remove the columns I don't plan to use from the excel. 


df1 <- Data1.xlsx [,c(-2:-4, -6:-7, -10:-12, -14:-16)]

?rbind



#Now I need to merge the data. Lets start with our depth data. 
?merge
