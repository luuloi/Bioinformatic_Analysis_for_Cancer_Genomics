###Input data
# create dummy data
data <- data.frame(
  name=letters[1:5],
  value=sample(seq(4,15),5)
)

#Using edit(data.frame())
dt <- edit(data.frame())
dt
#   var1 var2
# 1  age  ins
# 2   12 13.4
# 3   34 23.0
# 4   56 10.7
# 5   78  4.9
# 6   36  4.7
# 7   90 22.0
# 8   78 11.0
# 9   67 <NA>
colnames(dt)
# [1] "var1" "var2"
#Edit data
dt <- edit(data.frame(dt))
# var1 var2
# 1   35 11.5
# 2   12 13.4
# 3   34 23.0
# 4   56 10.7
# 5   78  4.9
# 6   36  4.7
# 7   90 22.0
# 8   78 11.0
# 9   67 22.9


###Barplot
clinical <-read.csv("/home/acer/Documents/DATA/DATA_VISUALIZATION/data/clinical.csv",header=TRUE)
head(clinical)
#  CaseNo Gender Age Survival.Months.
# 1      1      F  71                1
# 2      2      M  70               13
# 3      3      F  73                8
# 4      4      M  64               14
# 5      7      F  74                4
# 6      8      F  84                0

b <- table(clinical$Gender)
# F M 
# 9 5 
barplot(b,ylim=c(0,10),col="#69b3a2", main="barplot of gender")

barplot(b,xlim=c(0,10),col="#69b3a2", main="barplot of gender", horiz = TRUE)

# create dummy data
data <- data.frame(
  name=letters[1:5],
  value=sample(seq(4,15),5)
)
barplot(height=data$value, names=data$name, labels=data$piepercent)


###Pie chart
b <- table(clinical$Gender)
# F M 
# 9 5 
pie(b,col=c("blue","yellow"))

# Create data for the graph.
x <-  c(21, 62, 10,53)
labels <-  c("London","New York","Singapore","Mumbai")

piepercent<- round(100*x/sum(x), 1)

pie(x, labels = piepercent, main = "City pie chart",col = rainbow(length(x)))
legend("topright", c("London","New York","Singapore","Mumbai"), cex = 0.8,
   fill = rainbow(length(x)))

###Histogram
hist(clinical$Age, col = "#84f0e1",  main= "Histogram of Age", xlab = "Age", border = "blue", probability = T)
lines(density(clinical$Age), col="red",lwd=2.0)

hist(clinical$Age, col = "#84f0e1",  main= "Histogram of Age", xlab = "Age", border = "blue")

###Density
# Calculate density
dx <- density(clinical$Age)
#plot
plot(dx, col="red",lwd=2.0,main="Density of Age")
#Filled Density Plot
polygon(dx, col="yellow",border="red",main="Density of Age")

###Dotchart
mtcars <- mtcars[order(mtcars$mpg), ]
head(mtcars)
# Dot chart of a single numeric vector
dotchart(mtcars$mpg, labels = row.names(mtcars),
         cex = 0.6, xlab = "mpg")

# Plot and color by groups cyl
grps <- as.factor(mtcars$cyl)
my_cols <- c("#999999", "#E69F00", "#56B4E9")
dotchart(mtcars$mpg, labels = row.names(mtcars),
         groups = grps, gcolor = my_cols,
         color = my_cols[grps],
         cex = 0.6,  pch = 19, xlab = "mpg")
##
clinical$class <- c(rep(1,10),rep(2,4))
colors <- numeric(2)
colors[clinical$class == "1"] <- "red"
colors[clinical$class == "2"] <- "blue"

dotchart(clinical$Age,labels= clinical$CaseNo,pch = 19,
         pt.cex = 1.5,groups = rev(clinical$class), color = colors)

###Group chart

# Create data
set.seed(112)
data <- matrix(sample(1:30,15) , nrow=3)
colnames(data) <- c("A","B","C","D","E")
rownames(data) <- c("var1","var2","var3")

#   A  B  C  D  E
# var1 14 13 24 11 25
# var2 10  9  4  1  2
# var3 12  6  5 26  7

# Get the stacked barplot
barplot(data, 
        col=colors()[c(23,89,12)] , 
        border="white", 
        space=0.04, 
        font.axis=2, 
        xlab="group")

###Scatter plot
head(iris)
# Plot the ‘Iris’ data set
plot(iris$Petal.Length, iris$Petal.Width)

# $ syntax
plot(iris$Petal.Length, iris$Petal.Width)

# with() function
with(iris, plot(Petal.Length, Petal.Width))

# attach() function
attach(iris)
plot(Petal.Length, Petal.Width)
detach(iris)

# formula syntax
plot(Petal.Width ~ Petal.Length, data=iris)

# Change the shape of the points and scale them down by 0.6
plot(Petal.Width ~ Petal.Length, data=iris,
     pch=16,
     cex=0.6)

#Adding Titles and Axis Labels
plot(Petal.Width ~ Petal.Length, data=iris,
     pch=16,
     col="dodgerblue1",
     main = "Iris Flower Data Set",
     xlab = "Petal Length (cm)",
     ylab = "Petal Width (cm)")
##uses the pch argument to plot each point with a different plotting character, 
# according to the parallel factor “Species”.
# A scatter plot that shows the points in groups according to their "species"
plot(Petal.Length ~ Petal.Width, data=iris,
     col=c("brown1","dodgerblue1","limegreen"),
     pch=c(1,2,3)[as.integer(Species)])

legend(x="topleft",
       legend=c("setosa","versicolor","virginica"),
       col=c("brown1","dodgerblue1","limegreen"),
       pch=c(1,2,3))
##Coplots (conditioning scatter plots)
coplot(Petal.Length ~ Petal.Width | Species,
       data=iris,
       columns=3,
       bar.bg=c(fac="lightskyblue"),
       col="dodgerblue1")
##Plotting the Regression Line
m <- lm(Petal.Width ~ Petal.Length, data=iris)
summary(m)
# Call:
# lm(formula = Petal.Width ~ Petal.Length, data = iris)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -0.56515 -0.12358 -0.01898  0.13288  0.64272 

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -0.363076   0.039762  -9.131  4.7e-16 ***
# Petal.Length  0.415755   0.009582  43.387  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.2065 on 148 degrees of freedom
# Multiple R-squared:  0.9271,	Adjusted R-squared:  0.9266 
# F-statistic:  1882 on 1 and 148 DF,  p-value: < 2.2e-16

# Petal.Width = 0.42* Petal.Length - 0.36

plot(Petal.Width ~ Petal.Length, data=iris, col="dodgerblue1")
abline(m, col="brown2")



###Line Plots
# Create some variables
x <- 1:10
y1 <- x*x
y2  <- 2*y1

# Create a basic stair steps plot 
plot(x, y1, type = "S")
# Show both points and line
plot(x, y1, type = "b", pch = 19, 
     col = "red", xlab = "x", ylab = "y")

#Plots with multiple lines
# Create a first line
plot(x, y1, type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "x", ylab = "y")
# Add a second line
lines(x, y2, pch = 18, col = "blue", type = "b", lty = 2)
# Add a legend to the plot
legend("topleft", legend=c("Line 1", "Line 2"),
       col=c("red", "blue"), lty = 1:2, cex=0.8)

###BOXPLOT
boxplot(clinical$Age,ylim=c(39,90))
boxplot(clinical$Age ~ clinical$Gender,col=c("green","yellow"),xlab = "Age", ylab = "Gender")

#Exp
head(ToothGrowth, 6)
#  len supp dose
# 1  4.2   VC  0.5
# 2 11.5   VC  0.5
# 3  7.3   VC  0.5
# 4  5.8   VC  0.5
# 5  6.4   VC  0.5
# 6 10.0   VC  0.5

# Box plot of one variable
boxplot(ToothGrowth$len)
# Box plots by groups (dose)
# remove frame
boxplot(len ~ dose, data = ToothGrowth, frame = FALSE)
# Horizontal box plots
boxplot(len ~ dose, data = ToothGrowth, frame = FALSE,
        horizontal = TRUE)
# Notched box plots
boxplot(len ~ dose, data = ToothGrowth, frame = FALSE,
        notch = TRUE)

#Change group names
boxplot(len ~ dose, data = ToothGrowth, frame = FALSE,
        names = c("D0.5", "D1", "D2"))

#Change colors
# Change the color of border using one single color
boxplot(len ~ dose, data = ToothGrowth, frame = FALSE,
        border = "steelblue")
# Change the color of border.
#  Use different colors for each group
boxplot(len ~ dose, data = ToothGrowth, frame = FALSE,
        border = c("#999999", "#E69F00", "#56B4E9"))
# Change fill color : single color
boxplot(len ~ dose, data = ToothGrowth, frame = FALSE,
        col = "steelblue")
# Change fill color: multiple colors
boxplot(len ~ dose, data = ToothGrowth, frame = FALSE,
        col = c("#999999", "#E69F00", "#56B4E9"))

##Box plot with multiple groups
boxplot(len ~ supp*dose, data = ToothGrowth,
        col = c("white", "steelblue"), frame = FALSE)



##CLASSWORK
#
dt <- read.csv("/home/acer/Documents/DATA/VnP2022/R/R2/R2/data/data/methyl_1.csv",sep="\t")
data <- dt[1:5,]
library(reshape2)
a <- melt(data)
boxplot(value~variable,data=a)
#
library("readxl")
mydt <- read_excel("/home/acer/Documents/DATA/VnP2022/R/R3/meth_02.xlsx")
e <-melt(mydt)
boxplot(value~variable,data=e)
