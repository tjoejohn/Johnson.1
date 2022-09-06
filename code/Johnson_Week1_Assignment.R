# Now it is time to create your own data frame using the tools we have learned this week.
# First, resave this script as: your last name_Week1_Assignment()
  # e.g. mine would be Wilson_Week1_Assignemnt


# Create 3 numeric vectors and 2 character vectors that are each 15 values in length with the following structures:
  # One character vector with all unique values
  # One character vector with exactly 3 unique values
  # One numeric vector with all unique values
  # One numeric vector with some repeated values (number of your choosing)
  # One numeric vector with some decimal values (of your choosing)
#to do this, create vectors with the following code


a <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "o", "p")
b <- c("q", "q", "q", "q", "q", "r", "q", "q", "s", "q", "q", "w", "q", "q", "q")
c <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
d <- c(15,16,17,18,19,20,18,21,22,23,18,24,25,18,26)
e <- c(0.56,27,28,29,0.99,0.21,0.67,30,31,32,33,0.88,0.23,45,50)



# Bind the vectors into a single data frame, rename the columns, and make the character vector with unique values the row names.
#To bind, 
data <- cbind(a,b,c,d,e)
data
#To get rid of quotation marks: 
df <- as.data.frame(data)
df

#To rename columns: 
colnames(df) <- c("lastinitial", "firstinitial", "passes", "touchdowns", "interceptions")
df

#Chaacter vectors with unique values are already row names???, but supposed to use this code 
row.names(df) <- df$playernumber
df


# Remove the character vector with unique values from the data frame.

#To do this: 
df$lastinitial <- NULL
df

#Except lastinitial would be name of whaever column you want to remove. 

# or this which I could not get to move the correct column
row.names(df1) <- df1$passes
df <- df[,-1]
df

# Add 1 row with unique numeric values to the data frame

#To do this use the following code 
add.row <- data.frame(200,99,100.10,179)
# next Bind rows:
df1 <- rbind(df, add.row)
tail(df)
#obviosly "add.row" would be whatever the name of the data is you want to add.


#rebind everything. (reframe is needed)
data <- cbind(a,b,c,d,e,f)
df

# Export the data frame as a .csv file 

# Generate summary statistics of your data frame and copy them as text into your script under a new section heading.

# Push your script and your .csv file to GitHub in a new "Week1" folder.

setwd("c:/GitHub/Johnson.1/data")
getwd()

