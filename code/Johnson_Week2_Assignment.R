# With the data frame you created last week you will:

#first, update data frame from last week to this. Same numbers as last week in Johnson_Week_1_Assignmnet, but now have names infront instead of letters 

unique.char <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "o", "p")
group.char <- c("q", "q", "q", "q", "q", "q", "r", "r", "r", "r", "s", "s", "s", "s", "s")
uniqu.num <- c(1,2,3,4,5,6,7,8,69,80,118,115,110,99,88)
rep.num <- c(15,16,18,26,19,20,18,21,22,23,18,24,25,18,26)
dec.num <- c(0.56,9,10,11,0.99,0.21,0.60,30,12,14,13,0.88,0.23,27,50)

#next follow these steps. 
# Combine vectors into a data frame:
df <- as.data.frame(cbind(unique.char,group.char,uniqu.num,rep.num,dec.num))
#Make sure the numeric columns stay numeric:
df$uniqu.num <- as.numeric(as.character(df$uniqu.num))
df$rep.num <- as.numeric(as.character(df$rep.num))
df$dec.num <- as.numeric(as.character(df$dec.num))

# Create a row to add to the data frame:
add.row <- data.frame("X", "q", 99,100,18.18)

# Match the names of the columns:
colnames(add.row) <- colnames(df)    

# Bind rows:
df <- rbind(df, add.row)

# Now to move the row names and delete the column. 
df$unique.char <- NULL
df


# Create a barplot for one numeric column, grouped by the character vector with 3 unique values
#Now to create barplot for one numeric column, grouped by the character vector with 3 unique values
# Now let's generate means for each group based on the factor value for the $rep.num column. 
# We will also specify the function to be applied is generating mean() values with the "FUN" argument:
df.mean <- aggregate(df$rep.num ~df$group.char, FUN = "mean")
df.mean
# $symbols can get messy fast, so ranme without them:
colnames(df.mean) <- c("Factor", "Mean")
df.mean

#Now we can plot the mean values by factor:
barplot(df.mean$Mean)

#Missing X axis, so do this:
barplot(df.mean$Mean, names.arg = df.mean$Factor)

# Add error bars with mean and standard deviation to the plot

 # First we need to use the 'FUN' argument to call the standard deviation, or sd(), function:
df.sd <- aggregate(df$rep.num ~df$group.char, FUN = "sd")
# And add column names:
colnames(df.sd) <- c("Factor", "StanDev")
df.sd

barplot(df.mean$Mean)
#Add error bars to exisiting plot by using the arrows () function
# First we need to create a plot object in R.
#We also need to Change the x and y labels and add a title, and expand the Y limits!
b.plot <- barplot(df.mean$Mean, names.arg = df.mean$Factor, xlab = "Football Player last name initial", ylab = "#of touchdowns", main = "Susquehanna U Football", ylim = c(-2,70))

# To create the flat top and bottom of the error bars we use the argument "angle = 90" to specify they are perpendicular to the y-axis. 
# The "code = 3" argument is used to draw arrows on both ends (not just above or below the mean): 

arrows(b.plot, df.mean$Mean-df.sd$StanDev,
       b.plot, df.mean$Mean+df.sd$StanDev,angle=90,code=3)

#Set working directory!
setwd("C:/GitHub/Johnson.1")
getwd()


  # Export the plot as a PDF that is 4 inches wide and 7 inches tall.

# Create a scatter plot between two of your numeric columns.
# Typically, the x-axis is used to plot the explanatory variable and the y-axis is used to plot the response variable.
#To create scatter plot:
plot(df$dec.num ~ df$rep.num)
# Change the point shape and color to something NOT used in the example.
?pch
#This gives you a list if colors and shapes
pch=19
# Change the x and y labels and add a title
plot(df$dec.num ~ df$uniqu.num, xlab = "#of apple trees planted", ylab = "Apples grown", main = "Johson Apple Farm ", 
     cex.axis=0.8, cex.main = 0.5, cex.lab = 1.00, pch=15, col= "red2")
#Set working directory!!!
# Export the plot as a JPEG by using the "Export" button in the plotting pane.

# Upload both plots with the script used to create them to GitHub.
  # Follow the same file naming format as last week for the script.
  # Name plots as Lastname_barplot or Lastname_scatterplot. Save them to your "plots" folder.
