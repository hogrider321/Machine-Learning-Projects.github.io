# -----------------------------------------------
# Exploration and Visualizaton of mpg dataset
# ------------------------------------------------
library(ggplot2)

#Add one method to view the mpg dataset. 
mpg=mpg
View(mpg)

#Show a summary of the mpg dataset
summary(mpg)
#Show the structure of the dataset?
str(mpg)

#Observe the categorical and continuous variables. 
#you will be asked to send list of categorical and continuous variables in a seperate file
continuous = c("displ", "year", "cyl", "cty", "hwy")
continuous
categorical = c("manufacturer", "model", "trans", "drv", "fl", "class")
categorical

########### Single Variable Plot
#Draw distributions for manufacturers, year of manufacturing, displacement, transmission type
#Pick the right plot for each variable. 

#distribution of manufacturer - bar graph
ggplot(mpg,aes(manufacturer))+geom_bar() 

#distribution of  year of manufacturing - histogram
ggplot(mpg, aes(year)) + geom_histogram()

#distribution of displacement - density?
ggplot(mpg, aes(displ)) + geom_density()

#distribution of transmission type - bar
ggplot(mpg,aes(trans))+geom_bar() 

#Color code the manufacturer by drive type. ### Hint: Add color dimension. 
ggplot(mpg,aes(manufacturer, fill=drv))+geom_bar() 

#Color mapping for the manufacturer distribution by class
ggplot(mpg,aes(manufacturer, fill=class))+geom_bar() 

################### 2 variables plot
# Create a  plot where engine displacement (displ) and highway miles per gallon (hwy) are mapped to the x and y aesthetics
ggplot(mpg, aes(displ,hwy))+geom_point()

#Create a color dimension for 'class'
ggplot(mpg, aes(displ,hwy, color=class))+geom_point()
#or
ggplot(mpg, aes(displ,hwy, size=class))+geom_point()

# Let's decorate the plot. 
#Add Labels with labs()
#x label "Engine Displacement in Liters",
#y label "Highway miles per gallon",
#title = "MPG data",
#IMPORTANT subtitle = 
            #give a title based on what you see on the plot? #For eg:Cars with higher miles have fewer cylinders.. 
            #What correlation do you observe here? based on that give a subtitle
#caption = "Source: mpg data in ggplot2")
plot = ggplot(mpg, aes(displ,hwy, color=class))+geom_point() + labs(
title = "MPG Data",
x = "Engine Displacement in Liters",
y = "Highway Miles per Gallon",
subtitle = "Cars with higher displacement have lower highway miles",
caption = "Source: mpg data in ggplot2"
)
plot

# Add a smooth line of blue color using geom_smooth. you may notice a warning which you can ignore.. 
plot = plot + geom_smooth(color="blue")
plot 

# Facet by 'class'. Create facets by class on the same plot. 
ggplot(mpg, aes(displ,hwy, color=class))+geom_point() + facet_grid(class~.)

# Create a facet which is side by side using facet_wrap(). Choose any variable for the facet to show the behavior
ggplot(mpg, aes(displ,hwy, color=class))+geom_point() + facet_wrap(class~.)

#Create a two variable facet showing relationship between mpg/hp with facets of drv and cyl
ggplot(mpg, aes(displ,hwy, color=class))+geom_point() + facet_grid(drv~cyl)

#Use the mtcars dataset for the following items
# Create a plot to show relationship between between qsec and weight? 
mtcars=mtcars
ggplot(mtcars, aes(qsec,wt))+geom_point()
# Create a plot to show relationship between weight and mpg - some
ggplot(mtcars, aes(wt, mpg))+geom_point()

# Challenge

# In a scatter plot above, map a continuous variable to size. Hint: simply replace color=variableName to size=variableName in the aesthetics mapping
ggplot(mtcars, aes(wt, mpg, size=disp))+geom_point()

# In a scatter plot, with displ and hwy, convert the continuous variable displacement to factor. Observe the changes...instead of shaded hues, is it discrete colors
ggplot(mpg, aes(displ, hwy, color=factor(cyl)))+geom_point()

# What happens if you map an aesthetic to something other than a variable name, like aes(...color = displ < 5)? Change your code above to color=displ<5
ggplot(mpg, aes(displ, hwy, color=displ<5))+geom_point()

#Show the relationship between city and highway miles per gallon for cars, and map the color for only SUVs. (hint:add a condition for class equal to SUV. Check the mpg dataframe to see how SUV appears in the dataframe. You need to pick the exact label from the dataframe for the condition to work.)
ggplot(mpg, aes(cty, hwy, color=class=="suv"))+geom_point()

### Getting ready for the real thing! Machine Learning. Run the following lines
mt=mtcars

#What do you observe when you run cor() function? Cor() measures correlation.. 
#is there a positive or negative correlation between qsec and weight, or is there any at all?
cor(mt)
#negative correlation

#### - Next EDA of MPG/MTCARS Dataset.  