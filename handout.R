
############################################################
###################Author Info##############################
############################################################
# Author: Tianyang (Terry) Zhang
# Date: 09/09/2016
# Email Address: tz2261@tc.columbia.edu
# Edited by: xxx
#            xxx
#            NAME HERE
#            xxx

############################################################
###################R Installation###########################
############################################################
# Where to download R?
# http://www.r-project.org/

# Where to download R-Studio?
# www.rstudio.com/

# More R online resource
# https://www.edx.org/course/introduction-r-data-science-microsoft-dat204x
# https://www.coursera.org/learn/r-programming

############################################################
###################Arithmetics##############################
############################################################

1 + 1

3 - 4

2 * 3

10/2

2^3

sqrt(9)

9^(1/2)

# What is the result of this operation?
(125 ^ (1/3)) + 5 
### Remember, the order of calculation is prioritied in such way: 
### whatever in parentheses -> exponents (^) -> multiplications (* and /) -> addition (+ and -)

### For example, what do you think this equals to?
(1+1)*3^2
### it is not equals to (2*3)^2 = 64, because 3^2 is calculated before 2*x
### please try it yourself, do your math on paper and check to see if it is the same with R output.

############################################################
###################Calling Build-in Functions###############
############################################################
### There is a lot of functions avaiable out there for your to explore, here are some examples
### first we need some data, install this package that contains data we need
install.packages("datasets")
library("datasets") ## After the installation, you need to put it in your library so you could use it.
print(sleep) ### This is a students' sleep data
names(sleep) 
dim(sleep) ### dimension of this data called sleep. There are 20 responses, and 3 variables
hist(sleep$extra)
plot(sleep$extra,main="Plot variable called  'extra'")
plot(sleep$group,sleep$extra,main='Boxplot for extra within each group')
mean(sleep$extra) 
sum(sleep$extra)/length(sleep$extra) ## notice this is equal to the mean of extra, do you know why??

var(sleep$extra) ## variance of extra
sd(sleep$extra) ## standard deviation of extra
min(sleep$extra) ## minimum
max(sleep$extra)

fit <- lm(extra~group,data=sleep)  # linear regression of extra on group
fit
summary(fit) 
anova(fit)  ## run ANOVA on this linear regression result

############################################################
###################Read in Data#############################
############################################################
###You can read in from web
data <- read.table("http://socserv.socsci.mcmaster.ca/jfox/Books/Companion/data/Duncan.txt",
                   header = TRUE)
data
### From local file in txt, csv, etc.
data <- read.table(file.choose(), header = TRUE)
### Or simply click Tools -> Import Dataset.....


# More examples of build-in function for today:
names(data)
data$education
summary(data)
attach(data)
type
detach(data)

library("car")
hist(data$education,
     col = "light blue", # Use 'col' to change the color of your graph.
     main = "Education variable histogram", # Use 'main' to change the title of your graph
     xlab = "Education values", # Use 'xlab' to change the title of the x-axis
     ylab = "Frequency in the data set")

plot(data$income, data$education, col = "purple", main ="Scatterplot of Income and Education")
abline(lm(data$education ~ data$income), #lm() for the regression inside the abline() function
       lwd = 2, # 'lwd' specifies the thickness of the regression line.
       col = "green") # 'col' gives a color to the line
# Include of lowess regression line
lines(lowess(data$education, data$income), col="blue")

scatter3d(data$income ~ data$education + data$prestige, # Indicates the three variables to plot
          data = data)

############################################################
#######################Begin Coding#########################
############################################################

#Assignment
a<-10
b<-5
c <- 0
a
b
c

#Goal is to switch the value of a and b
c<-a
a<-b
b<-c
a
b

#Example 1: Find the max value of an array
b <- round(rnorm(n=10,mean=5,sd=5),0)
b
max(b)
min(b)

#ASIDE:
?rnorm()
rnorm(10)
dnorm(0)

# Or you can write your own code
result <- b[1]
for(i in 1:length(b)){
  if (result<b[i]){
    result <- b[i]
  }
}
result

#Or even define a function by yourself:
myextrema <-function(imput){
  mymin <- imput[1]
  mymax <- imput[1]
  for ( i in 1:length(imput)){
    if (mymin>imput[i]){mymin <- imput[i]}
    if (mymax<imput[i]){mymax <- imput[i]}
  }
  return (c(mymin,mymax))
}
myextrema(a)

#Example 2: Sort an array
a<- round(rnorm(10,0,1),3)
a
sort(a)

#or use order function
order(a)
a[order(a)]
a[2]

#or write your own code
result <- a
temp <- NA
for (i in 1:length(a)){
  for(j in 1:(length(a)-1)){
    if(result[j]>result[j+1]){
      temp <- result[j+1]
      result[j+1] <- result[j]
      result[j] <- temp 
      temp <- 0
    }
  }  
}
result


# Question to think: how to find the second largest number in an array?

############################################################
############################Loop############################
############################################################
### Loops (or iterations) is a way to make computer do something over and over.
### Typically in practice we will use "for loop" and "while loop"

### for loop will run a section of code whenever the condition is ture. For example: sum 1 for 100 times
temp1<- 0
for (i in 1:100){
  temp1 <- temp1 + 1
}
temp1

###sum up 1 to 100
temp <-0
for (i in 1:100){
  temp <- temp + i 
  ## notice that i, as a local variable, is now in the loop. Therefore,
  ## for each time for loop is running, i has different values: in the first run i is 1, the second time i is 2, etc.
}
temp

#to illustrate this, think about this example:
for ( i in 1:10){
  print(i)
}

# To test yourself, see if you know the result of this for loop:
temp1 <- 0
for (i in 1:10){
  temp1 <- 2*i-1
}
temp1 ## Is your answer equal to 19? Great! 

### while() loop will execute a block of commands until the condition is no longer satisfied.
x <- 1
while(x < 5) {
  x <- x+1
  print(x)}

while(TRUE) {
  x <- x+1
  print(x)}
#"next" can skip one step of the loop.
#"break" will end the loop abruptly.
#Let's break the loop when x=3:
x <- 1
while(x < 5) {x <- x+1; if (x == 3) break; print(x); }

##Let's skip one step when x=3:
x <- 1
while(x < 5) {x <- x+1; if (x == 3) next; print(x);}

### in some cases you will need a loop within a loop, such structure is called nested loop. For example:
temp4<-0
for (i in  1:5){
  for (j in 1:3){
    temp4 = temp4+1
    print(i)
    #print(j)
  }
}
temp4

############################################################
###################Write Your Own Functions##################
############################################################
### Before you write your own function, you would like check to see if there is an appropriate function
### already out there. type "?"+"FUNCTION NAME" for function that you know the name, and "??"+"KEY WORDS" for
### for all related functions. 

### For example, you would like to order a sequence of numbers from smallest to the largest. 
?order  ### This function seems about right, let's try it out!
temp <- c(4,9,7,1,3)  ## create a column vector for experiment
order(temp) ## it works, that means if we want to output elements of temp in ascending order, simply:
temp[order(temp)]
### comment: it is generally a good idea to use build-in functions, as in most cases it is reliable.
### what if you don't even know what you show be looking for?
??order  ## it take some time to find a right function, you should also try using Google.
### Here I would like to try stri_order{stringi}, even though it is for characters, not numbers.
install.packages("stringi")  ## for any packages, you should only install it once!
library("stringi")
stri_order(temp) ## Yeah! Looks like it also works! Check this:
temp[stri_order(temp)]

### Exercise: Try if you could find a function to rank a vector in ascending order, say this one below:
your.temp <- rnorm(10, mean= 5, sd = 1)
your.temp
### Hint, key word is "rank"

### Now, if you only trust yourself, fine! Take a look at this before we start:
### http://www.statmethods.net/management/userfunctions.html

### celsius to fahrenheit example:
CtoF <- function(input){ # comment your code wherever is necessary, this is very important!
  f <- (input-32)*5/9
  return(f)
}
### Try convert 10 in celsius to farenheit
CtoF(input=10)  # it works, yeah!
### try write a function that convert Fahrenheit to Celsius, also try inches to m, and vice versa.

### two variable example: BMI
BMI <- function(height, weight){  # height is in m, weight is in kg
  bmi <- weight/height^2 
  return(bmi)
}

### now try input your height and weight, convert to meters and kilograms using user-defined function, and call
### this BMI function. Good luck to your health! 

