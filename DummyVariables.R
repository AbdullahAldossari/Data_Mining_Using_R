

install.packages("dummies")
library(dummies)
students <- read.csv("data-conversion.csv")

students

# We can create dummies for all factors in the data frame

students.new <- dummy.data.frame(students, sep = ".")
names(students.new)

# The students.new data frame now contains all the original variables and the 
# newly added dummy variables. The dummy.data.frame() function has created
# dummy variables for all four levels of the State and two levels of Gender factors. 

# The dummy.data.frame() function creates dummies for all the factors in the data 
# frame supplied. Internally, it uses another dummy() function which creates dummy 
# variables for a single factor. The dummy() function creates one new variable for 
# every level of the factor for which we are creating dummies. It appends the variable 
# name with the factor level name to generate names for the dummy variables. We can use 
# the sep argument to specify the character that separates them-an empty string is the default:

dummy(students$State, sep = ".")

dummy(students$Gender, sep = ".")

students.new1 <- dummy.data.frame(students, names = c("State","Gender") , sep = ".")

students.new1