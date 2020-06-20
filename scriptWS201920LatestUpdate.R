### Let's go!
#Let's create objects!
name <- c("Hans", "Lena", "Franz", "Luise", "Hubert")
age <- c(19, 18, 21, 25, 22)
subject <- c("social sciences", "medicine", "engineering", "medicine", "social sciences")
bafoeg <- c("yes", "no", "yes", "no", "yes")
#Press ctrl+enter to execute. We have now created several objects. 
#When entering words in R, R thinks it is an object or a command/function. That is why this works:
name

#but this does not
name2 <- Hans

#but this does
name2 <- "Hans"

#Example to show why c() is necessary and what the print command () is for:
age2 <- 21
(age2 <- 21)

age2 <- 21, 28
age2 <- c(21, 28)
name2 <- c("Hans", "Franz")

#object names
2age <- c(22,29)
age 2 <- c(22,29)

#Objects: looks at global environment, showing objects
objects()
rm(age2, name2)

#There are different kinds of objects. Vectors are one dimensional objects containing the same data type.
class(age)
class(name)

#Overwritting objects: You made a mistake. Lena is 19. Keep the name of the object the same.
(age <- c(19, 19, 21, 25, 22))

#You had actually not made a mistake. Lena is 18. There is another way to change that value, with the 
#index function.
age
age[2]
age[2] <- 18 #object age, value at index 2 becomes 18
age

#So far, you have a lot of different vectors, all floating around. Let's make a DF:
(survey2 <- data.frame(name, age, subject, bafoeg))
#Describe what has changed. 
class(survey2)
#Tidy data: every column is a variable, every row is an observation!!! 

#When we open a new DF, sometimes we don't know what it actually looks like. There are several ways
#of doing that:
View(survey2)
head(survey2)
tail(survey2, 2)
str(survey2)
dim(survey2)
nrow(survey2)
ncol(survey2)
length(survey2) # What's the point of length? Length can also be applied to vectors!
length(name)
summary(survey2)

#Before, when we selected parts of a vector, we could "select" it by its object name and we could select the info
#it contained using the index function. Now, the object name refers to the dataframe (DF) and it has two dimensions.So things 
#change! Using the index function:
survey2[1] #By default, if only one number is entered in the square brackets of the index function when the object
# is a two-dimensional DF, the corresponding column is selected.  
survey2[1,]
survey2[,1]

#Lena is 19, not 18!
survey2
(survey2[2,2] <- 19) 
survey2[2,]

#The index function can also be used to select several variables:
survey2[2:4]
survey2[-(2:4)] #The minus in this case means "select the DF, except column 2:4". 

########################################################################################################################
#Excercise 1
# 1. Ask five classmates their first names, hobby, favourite colour and approximate body height. Create the corresponding four
#    vectors. Change the colour you like least to the colour you like most!
# 2. Check what type of vectors they are. 
# 3. Make a dataframe out of the vectors. Name it ex1. 
# 4. Remove all the objects from the environment, except the DATAFRAME.
# 5. Select the 3rd person's body height in the DATAFRAME and double it by multiplying.
# 6. "Print" the first row (i.e. tell R that you want to see it in the CONSOLE).
# 7. Select the 2nd person's favourite hobby. Change it to yours! 
# 8. Print the first column.
# 9. Make a copy called ex1.1 of ex1. 
# 10. If you are done already: 
# 10.1. If you found a type of vector other than character and numeric in 2. above, google it! What kind is it?
# 10.2. Check out what kind of object lists are. Make one. Print it. Make it longer. Make it
#       shorter. Write down what it is and the commands you need in your script!
# 10.3. Experiment with the so-called logicals (<, >, =>, <=) and different numbers. Create a vector that contains the result
#  of a logical evaluation. 
#########################################################################################################
########################################################################################################################
#Excercise 1: Solutions
# 1. Ask five classmates their first names, hobby, favourite colour and approximate body height. Create the corresponding four
#    vectors. Change the colour you like least to the colour you like most!
#Answer(A): See the beginning of the script. To change an entry, use the index function. Since you are using it on a vector,
#  you only write a single number (index) to tell R where the value you want to change is located (object[x] <- "newValue"). 
#  Since we are talking about a colour (i.e. a word), put it in "". 
# 2. Check what type of vectors they are. 
class().
# 3. Make a dataframe out of the vectors. Name it ex1. 
DFname <- data.frame(object1, object2). #Make sure the objects are the same length (for e.g. with length()). If they are 
#  not the same length, data.frame will not work. 
# 4. Remove all the objects from the environment, except the DATAFRAME.
# A: Find out which objects there are with objects(). Then remove them with rm(). 
# 5. Select the 3rd person's body height in the DATAFRAME and double it by multiplying.
# A: The 3rd person is found in the 3rd row. If body height is the 4th variable (you can check with names()), then the 
#  solution is: 
object[3,4] <- object[3,4]*2 # If you forget "object[3,4] <- ", R does not know what you want to do with the 
#  value you have just calculated (x*2) and the value just disappears again. You need to also tell R: "Take this value and put
#  it there." Since you want to overwrite a value, the place you want R to put it is the same place from which you took the 
#  value to multiply it by 2. 
# 6. "Print" the first row (i.e. tell R that you want to see it in the CONSOLE).
DF[1,].
# 7. Select the 2nd person's favourite hobby. Change it to yours! 
DF[2,2] <- "beingAsuperhero" # (assuming that hobby is the 2nd column)
# 8. Print the first column.
DF[1] 
DF[,1] #The deafult of the index function for a 2 dimensional object when only one number is put between the 
#  square bracket is to interpret that as referring to a column.
# 9. Make a copy called ex1.1 of ex1. 
ex1.1 <- ex1
# 10. If you are done already: 
# 10.1. If you found a type of vector other than character and numeric in 2. above, google it! What kind is it?
# 10.2. Check out what kind of object lists are. Make one. Print it. Make it longer. Make it
#       shorter. Write down what it is and the commands you need in your script!
# 10.3. Experiment with the so-called logicals (<, >, =>, <=) and different numbers. Create a vector that contains the result
#  of a logical evaluation. 
# A: The first two excercises of 10. are either not important for the course or the topic they are on will de dealt with later.
#  Concerning 10.3, note the following:
(x <- 1:10)
(y <- x>5)
(z <- x[y])
# This logic is applied at times in different functions. The y vector contains the values TRUE or FALSE, depending on whether 
# x>5 for each value in x. If you take such a TRUE/FALSE vector and apply it, via the index function, to the original vector 
# (or any vector of the same length), R creates a new vector containing only those values which correspond to the TRUE values
# in the original vector (or any vector of the same length). 
#########################################################################################################

#There is another way to select variables in a DF by name. This is done using the dollar sign. To see which variables
#one can select with $ in a DF, use names(). 
names(survey2)
survey2$name
# names() does not work with vectors. That is what objects() is for! 
names(name)
#And if you want to select more than 1 variable by name:
survey2[,c("name", "subject")]

#Before we do the first excercise, let's learn two more things: 1. How logical operators (<, >, =, etc.) work and 
#how they can be used with which to select columns; 2. what it means that R is a vector-oriented language.  
#Try out some logical operators and see what happens.
5 == 3
5 > 3

#One can also select particular rows by using logical operators, the index and the which() function:
survey2[which(survey2$age ==19),] # In R, the expression "is equal to" is written "==", not "="!
survey2[which(survey2$age !=19),] # This is the opposite of == (i.e. those not equal to x).
survey2[which(survey2$age >=25),] 
#What this line of code is telling R is the following: Within survey2([]), select (which()) those rows (look at the 
#comma!) which, in the sex or age variable, meet the logical condition (i.e. which are found to be TRUE). 

#Many R commands are vectorized. Let's create a copy of survey2 and figure out what that means!
survey3 <- survey2
survey3$daysPerYear <- 365 
survey3$age*365
survey3$agePlus1and2<- survey3$daysPerYear + c(1,2)
survey3
# What happened? As you can see, R is taking a single value, 365, and creates a variable from it that is as long as
# the DF (nrow()) to which it is attached, by repeating that value. Something similar happens with c(1,2). R takes these two
# values and adds the value 1 to the 1st row, 2 to the 2nd row, 1 to the 3rd row etc. until every row of age2 has
# been recalculated. 
# In other (statistics) programming languages, you would have to say explicitly: "take vector x, take another value 
# and add that single value to every value contained in x (called a loop)." In R, you just say: "take vector x, add a single 
# value" and R does that automatically!

# But where did our varible survey3$age*365 go? It's not in the DF! Remember how one creates objects? If you want 
# to "save" changes you made to an object or save a new one, you need to assign it a name. If you give it the 
# same name, the previous object will be overwritten. If you give it a new name, it will be saved as a new object.
# Pay attention to whether you are working with a variable in a DF (then you need to tell R about it with $)
# or just a vector.Pay attention to whether you want the new object to become a variable in an existing DF or a single 
# vector.
(survey3$ageInDays <- survey3$age*365) # A new variable is being calculated and attached to survey2.

# Sometimes, you want to create a variable which has a particular sequence of numbers. Here are some ways of doing
# that:
(1:10)
(10:1)
(letters)
(LETTERS)
(letters[1:5])
rep(c(1,2,3,4,5), times=2)
rep(letters[1:5], 2)
rep(c(1,2), each=5)
rep(c(1,2), times=c(2,8))
rep(1:3, length.out=10) # Creates a vector with 10 values!
seq(1, 20, 2) #seq(from = , to =, by = )
seq(1, 1000, length.out=10) # Same as above, but R is told to look for 10 numbers within a particular interval! Since
# 1 is included, 1000/9!
# Ok, let's select the 1:10 sequence again! Oh, wait, where the hell is it gone? Ah, right... Do you remember what to do
# to save it?

#So now we have a fantastic dataframe with the info we have been missing. Lets save this object as ab .RData file. 
save(survey2, file="survey2.RData")

# Before doing the following excercise, PLEASE NOTE the following. You may encounter problems because you are trying
# to do something to a vector that you think is numeric or character, but that R thinks is a factor. So if you encounter
# an unexplainable problem, check the class(DF$var) that a particular variable is! If it is a factor (another indicator
# that you are dealing with a factor is seing the word LEVELS), convert it to the kind of vector you need with these
# commands: 
as.character() # converts a factor (or a numeric vector) to a character vector 
as.numeric(as.character()) # converts a factor to a numeric vector. Why this is done like that will be explained later. 
# Lastly, 
as.numeric() # converts a character to a numeric vector
# One can also convert DFs to matrices, matrices (can only contain one data type (i.e. numbers or letters!)) to tables etc. 

#Let's remove all objects: 
remove(list=ls())

#But wait! We want to understand what is happening here! Can you find out? Try "R stats" and add your problem or function in
#google. Or put the function ("remove()") with the words "R documentation" before it. Let's have a look!

#But there is still an easier way! Every function in R has a so-called documentation, which esplains in detail how a function 
#works. It can be accessed directly through R with a question mark or help():
?remove
#Look at the documentation and figure out what "list=" stands for in the remove() function. Then find out what ls() stands for.
#Try ls(). Now you should be able to understand the whole function remove(list=ls())! 

#In any case, remember we saved the object survey2. Let's load it again!
load("survey2.RData")

#So now we want to attach this saved DF to another DF. Lets load "survey" as well (see your E-Mail, download attachment,
#copy and paste into the R project folder!). 
load("survey.RData")

#The object we loaded is called survey. But it is a coincidence that survey.RData and the object survey have the same name. 
#Consider the following example:
dog <- c("dog", "dog", "dog")
save(dog, file = "cat.RData")
rm(dog)
(load("cat.RData"))
#Therefore, it's better to give the object you want to save a descriptive name and give the .RData file the same descriptive
#name.

#Let's look at "survey". 
(survey)
#That is not very pretty. Let's try dim(), describe() and summary()!
dim(survey)
describe(survey)
summary(survey)
names(survey)
# Ah, much better!

#Now, we want to join the two dataframes. Better said, we want to attach "survey2" to the bottom of "survey".
#To be able to do so without problems, we have to make sure the names and order of the variables are the same. 
#Let's compare the order and names of the variables and then put the two DFs together.
names(survey)
names(survey2)
#Oh boy, the variables are in the wrong order, have the wrong names, there is one variable too many in the one DFs and
#one too few in the other. Let's adjust survey2!

#Add a variable, "sex". Look at the first names to deduce their sex and assign men the value 0 and women the value 1:
survey2$sex <- c(0,1,0,1,0)

#Delete the "name" variable:
survey2$name <- NULL
#Another way to do that using the index function:
#(survey2 <- survey2[,-1]) # The script is written in such a way as that you can select it all and execute it and it will
#work. Since we used NULL to delete name, apply the index function would delete another variable from the DF, so I put a #.

#Now that you think about it, you prefer to delete Lena, since you don't know if she really is 18 or 19. 
# How do you do it?
(survey2 <- survey2[-2,])
#Look at the output. The row numbers all the way on the left are now weird, because 2 is missing. Let's fix that (row names
#are usually not important because they are not "in" the dataset, they are not a variable, but you may, for example, look at
#survey 2 and think it has 5 instead of 4 rows etc.). You can "reset" row.names with NULL:
row.names(survey2) <- NULL

#Rename "subject" to "studies":
names(survey2)[2] <- "studies" 
#Here, you are telling R: look at the names (names()) of the DF columns, look at the second name ([2]), overwrite that value. 
#While we are on the subject, the names() command actually returns the colnames, which are what we have been calling the 
#variable names. 
#Alternatively: survey2$subject <- survey2$studies , but then you attach a new variable called studies and the subject variable
#still exists, which is not what we want!
names(survey2)
colnames(survey2)
#Like row.names, they are not a variable that forms part of the actual data in a data.frame. Another way to change variable
#names is thus through accessing them via colnames:
#(colnames(survey2)[colnames(survey2)=="subject"] <- "studies")

#Now, let's bring the variables in the right order:
names(survey)
names(survey2)
survey2 <- survey2[c(1,4,3,2)] # default for DFs with subset function without comma is column

#The variable bafoeg has y/n instead of 1/0. Use ifelse() to recode:
survey2$bafoeg <- ifelse(survey2$bafoeg=="yes",1,0) 
#This funciton tells R: overwrite the survey2$bafoeg values in such a way that the "yes" values for that variable are
#coded 1, all other values are coded 0. 

#Well done! Let's put the two DFs together!
survey <- rbind(survey, survey2) #rbind stands for row bind (i.e. it binds two DFs together top to bottom at their rows).
save(survey, file="surveyCompl.RData")

#########################################################################################################################
# Excercise 2
# 1. Create a new dataframe from the survey DF which contains only those people younger 21!
# 2. Create a vector called "random" for a new dataframe with 10 values consisting of the alternating values 1 and 2.
# 3. Create a vector of that same length called "random2" with the alternating values of 2 and 1.
# 4. Create a new DF from random and random2. 
# 5. Create a new variable for that dataframe which adds the values of its variables random and random2 together.
# 6. Rename the variable random to "notAtAllRandom".
# 7. Rename the variable random2 to "completelyRandom". 
# 8. Create a vector and attach as a row to the DF. 
# 9. Create a vector called "oneToSix" with the following values using rep() and attach it to the new DF: 1,1,2,2,2,2,2,3,4,4,6.
# 10.Try out the following math functions on oneToSix: prod, sum, cumsum, mean, sd, var, min, max, which.max, median, range, 
#    quantile. 
#    What do they do? If you can't figure it out, make smaller vectors and try on them!
# 11.Create two vectors identical with the variables notAtAllRandom and completelyRandom, called xRandom and yRandom.
# 12.Make a new DF out of them. Attach that DF to the previously created DF with cbind(). What does cbind() stand for?
#    cbind() stands for column bind.
##########################################################################################################################
#########################################################################################################################
# Excercise 2: Solutions
# 1. Create a new dataframe from the survey DF which contains only those people younger 21!
survey21 <- survey2[which(survey2$age < 21),] 
# 2. Create a vector called "random" for a new dataframe with 10 values consisting of the alternating values 1 and 2.
random <- rep(c(1,2), length.out=10)
# 3. Create a vector of that same length called "random2" with the alternating values of 2 and 1.
random2 <- rep(c(2,1), length.out=10)
# 4. Create a new DF from random and random2. 
randomDF <- data.frame(random, random2)
# 5. Create a new variable for that dataframe which adds the values of its variables random and random2 together.
randomDF$random3 <- random + random2
# 6. Rename the variable random to "notAtAllRandom".
names(randomDF)
names(randomDF)[1] <- "notAtAllRandom"
# 7. Rename the variable random2 to "completelyRandom". 
names(randomDF)[2] <- "completelyRandom"
# 8. Create a vector and attach as a row to the DF. 
ncol(randomDF) #To know how many values you need to attach the ROW to the BOTTOM of the DF.
x <-  c(1,2,3)
randomDF<- rbind(randomDF, x)
nrow(randomDF) #To know that it worked. Now you have 11 instead of 10 rows. 
# 9. Create a vector called "oneToSix" with the following values using rep() and attach it to the new DF: 1,1,2,2,2,2,2,3,4,4,6.
(randomDF$oneToSix <- c(rep(1,2), rep(2,5), 3, rep(4,2), 6))
# 10.Try out the following math functions on oneToSix: prod, sum, cumsum, mean, sd, var, min, max, which.max, median, range, 
#    quantile. 
#    What do they do? If you can't figure it out, make smaller vectors and try on them!
# A. This will be dealt with in the next class! If you want to know more, execute ?function from the script.
# 11.Create two vectors identical with the variables notAtAllRandom and completelyRandom, called xRandom and yRandom.
xRandom <- randomDF$notAtAllRandom
yRandom <- randomDF$completelyRandom
# 12.Make a new DF out of them. Attach that DF to the previously created DF with cbind(). What does cbind() stand for?
#    cbind() stands for column bind.
(randomDF2 <- data.frame(xRandom, yRandom))
(randomDF <- cbind(randomDF, randomDF2))
##########################################################################################################################
###### Factors (levels and labels)
# Factors are not actually vectors, they are "built on" numeric and character vectors, giving them certain added features. But that
# is not important. What's important is understanding factor levels, labels and how they work internally. 

# Let's make a dataframe!
(person= 1:5)
sex = c("a", "b", "a", "b", "b")
typeof(sex)
(facExp=data.frame(person, sex))

# Sex should be a factor!
(facExp$factor1 <-  factor(sex))
typeof(facExp$factor1)
class(facExp$factor1)
levels(facExp$factor1) # levels() shows the different values a factor can take on (e.g. female, male). 

# For the sex variable, "a" and "b" stand for female and male. That is confusing. Let's change it to "f" and "m"! 
facExp$factor2  <- factor(sex, labels = c("m", "f"))
levels(facExp$factor2)

#We said that "a" stands for female, "b" for male. So the levels of sex are wrong! Look at the DF!
facExp
facExp$factor3  <-  factor(sex, levels = c("b", "a"), labels = c("m", "f"))
levels(facExp$factor3)
facExp

# Why do we need levels and labels?
facExp$factor4 = factor(sex, levels = c(1,2))
(facExp)
#What is R thinking? factor(sex) automatically converts the values "a" and "b" in factor levels, the values that a factor can 
#take. Here, R is told to take sex, containing "a" and "b" values and is then told that the levels are "1" and "2". That makes no
#sense to R, because if you want to change the levels, you use the labels command. 
#Therefore, levels are automatically identified by R if you convert a vector to a factor. If you want to change the levels, you use
#labels. The number of levels and labels should be the same and the first level will receive the first label etc. So make sure you
#know the order of the levels with levels(). Sometimes, it helps to make the levels explicit with levels=c(), in which case you
#can order them how you want (i.e. you don't have to mention them in the order in which they appear with levels(). 

#We made a mistake! "a" and "b" did not stand for male or female, but whether a person's favourite fruit is an apple or a banana. 
#The factor already exists, but now we want to change the levels. 
facExp$factor4  <-  facExp$sex
levels(facExp$factor4)
levels(facExp$factor4) <-  c("apple", "banana")
(facExp$factor4)

# Let's drop the people liking bananas!
facApple <- facExp[facExp$factor4 != "banana", ]
levels(facApple$factor4)
#What's going on?! Why does the factor still have the level banana although no person in the DF actually likes banana? Factors
#don't just drop a factor level if all the observations which take on that value disappear. That has to be done explicitly with
#droplevels()
facApple$factor4 <- droplevels(facApple$factor4)
levels(facApple$factor4)

# Lastly, I have already shown how to convert numeric or character vectors to factors and factors to character vectors (above).
# Converting factors to numeric vectors, however, can cause problems. Although you do not see it, factors assign the factor levels,
# whether words or numbers, their own internal numbering for calculation, but they only show you the factor levels. Thus, if you
# use as.numeric() on a factor, it will reach into the factor and show you the numbers internally assigned to the levels, not the 
# actual values which R mistakenly took for factor levels. To avoid this problem, first convert the factor with the factor level 
# numbers to a character vector and then to a numeric vector with the following function.
# For example:
xFactor <- factor(x=c(rep(c(1,2), 5), rep(c(5,6), 2)))
levels(xFactor)
# We have created a factor. Let's pretend this is downloaded data and R interpreted this vector to be a factor without us wanting 
# it that way. Let's see what happens when we use as.numeric(). 
xNumeric <- as.numeric(xFactor)
(z <- data.frame(xFactor, xNumeric))
# Do you spot the difference? Look at the last 4 rows!
# Let's see what happens now!
z$xCharacterNumeric <- as.numeric(as.character(z$xFactor))
z
#That worked!
#Lastly, factors can be ordered, especially useful when we work with ordinal data and for controlling in which order the different
# factor levels are displayed when generating graphics.
x <- c("a", "b", "b", "a")
factor(x, levels=c("b", "a"), ordered=TRUE)
#Alternatively, you can use the ordered() function

############################################### Excercise 3###########################################################
# 1. Load the dataframe saved as surveyCompl.RData.
# 2.1. Which variables are supposed to be factors? Convert them into factors, give them proper labels. For sex, label the 
# 0s as male ("m") and the 1s as female ("f"). For Bafoeg, label 0 "n" for no and 1 "y" for yes. 
# 2.2. Save the dataframe with the recoded factors (under the same name, surveyCompl)!
# 3. Create your own factor called "interest" with three factor levels: boring, normal, fascinating. Attach it to survey.
# 4. Rename the factor level boring with "interesting", normal with "superinteresting".
# 5. Create a new DF called "superinteresting" with all observations that have the factor level "superinteresting".
# 6. Save the DF superinteresting!
##################################################################################################################
############################################### Excercise 3: Solutions
View(survey)
# 1. Load the dataframe saved as surveyCompl.RData.
load("surveyCompl.RData")
# 2.1. Which variables are supposed to be factors? Convert them into factors, give them proper labels. For sex, label the 
# 0s as male ("m") and the 1s as female ("f"). For Bafoeg, label 0 "n" for no and 1 "y" for yes. 
names(survey)
#Sex, bafoeg and studies are all supposed to be factors, because they are all nominal variables.
#Sex
survey$sex <- factor(survey$sex, levels=c(0,1), labels=c("m", "f"))
levels(survey$sex)
#Bafoeg
survey$bafoeg <- factor(survey$bafoeg, levels=c(0,1), labels=c("n", "y"))
levels(survey$bafoeg)
#2.2.
save(survey, file="surveyComplFac.RData")
# 3. Create your own factor called "interest" with three factor levels: boring, normal, fascinating. Attach it to survey.
# Read the code above carefully if you don't know how to do it.
nrow(survey)
nReps <- 1056/3
survey$interest <- factor(x=rep(1:3, nReps), levels=(1:3), labels= c("boring", "normal", "fascinating")) 
# 4. Rename the factor level boring with "interesting", normal with "superinteresting".
levels(survey$interest)
levels(survey$interest)[1] <- "interesting"
levels(survey$interest)[2] <- "superinteresting"
# 5. Create a new DF called "superinteresting" with all observations that have the factor level "superinteresting".
superinteresting <- survey[survey$interest == "superinteresting", ]
nrow(superinteresting)
# 6. Save the DF superinteresting!
save(superinteresting, file="superinteresting.RData")
##################################################################################################################
################# NAs (Not Availables)
# NAs are unkown values. If you have made a survey, asked somebody to tell you their income and that person has not
# told you, for example, you would enter the letters NA in column (variable) called "income" for that person.
# Since missing data is a common problem and there are many different ways of indicating that data is missing (e.g.
# leaving a blank space, writing n.a. etc.), it is very important that when you import data to R, missing values are 
# called NA and nothing else. R does also not understand blank cells in dataframes.
# Besides data import, another important point is that many functions in R don't work if a value included in the given
# vector is NA. 
NA/5
# The reason is that R, in this example, does not know what value NA is, so it cannot divide by 5. The same applies to the 
# following example:
(x <- c(rep(1:3, 3), NA))
mean(x)
# In order to make a great many functions in R work, it is necessary to set the option na.rm (NA remove, or remove NAs) 
# to the value TRUE, because the default is FALSE.
mean(x, na.rm=TRUE)
# How can one know how many NAs there are in a vector/DF? What does is.na() do?
(y <- is.na(x))
# That's right, it creates a logical TRUE/FALSE vector. When a value in x is NA, the output shows a TRUE for that value,
# otherwise it shows FALSE.
# As you know, it is possible to select particular rows of a dataframe by using a logical TRUE/FALSE vector and the 
# index function (the squared brackets []). The same applies to the TRUE/FALSE vector we just created, with which we 
# could now select all rows that contain NAs in a DF. In the following example, y is applied only to a vector, so the 
# result is not very exciting. 
x[y]
# When R shows you a logical vector, what you see are the words TRUE and FALSE. But for R's internal use
# FALSE=0 and TRUE=1. That means that we can do the following: 
sum(is.na(x))
# What does that show us? By that same logic, what does the following function show us?
mean(is.na(x))

# Let's look at na.omit():
df <- data.frame(x,y)
df2 <- na.omit(df)
nrow(df2)
# All rows, in this case one, containing an NA value were removed. Let's see what happens next:
(df$a <- c(1:4, NA, 6:10))
(df2 <- na.omit(df))
nrow(df2)
# As you can see, two rows were removed, although each row had the NA value in a different column.
# If we want to remove only those rows from a DF which have NAs in a particular column, apply the following:
df3 <- df[!is.na(df$a),]
nrow(df3)
df3

#######################################################################################################################
########################## Reading and writing data, using internal datasets
# Another, more comfortable way to enter data!
df <- edit(as.data.frame(NULL)) #If you want to open it again, use fix(df)
fix(df)
# Built-in datasets in R
data() # Shows which datasets are available. Some packages come with specific datasets, so load the package to be able
#to use it. To find out more about these, use the ?
?starwars
starwars
#diamonds, flights and mtcars are dfs that are commonly used to illustrate how R code works. Use these datasets if you 
#want to practice doing stuff!

# Let's practice to import and export data in .txt files and to use them in combination with Excel! 
dfStarWars<- starwars
nrow(dfStarWars)
dfStarWars <- apply(starwars,2,as.character) #Do not worry about this step! We are doing this because the variables of
#the DF are of the type list, which cannot be written to file.
write.table(dfStarWars, "dfStarWars.txt", sep="\t", row.names = FALSE) 
#Go to your project folder, open the .txt file. Look at the seperator. Close it. Open Excel. Open the file in Excel. 
#Delete all variables except name, homeworld and species. Save it as .txt with the same name.Now import it!
dfStarWars2<- read.table("dfStarWars.txt", header=TRUE, sep="\t")

##################################################################################################################
#################Tables
# By now, we have prepared the DF for analysis! Where do we start with out analysis? 
# The best thing to do is to generate summaries! There are (among others) two ways of doing so, by making tables and by 
# generating graphics! Let's discover the magic of tables!
load("surveyComplFac.RData")
table(survey)
table(survey$studies)
View(survey)
#Let's make a UNIVARIATE table with all sorts of sums and percentages!
N=table(survey$studies)
(relativeN=prop.table(N))
(percentage <- 100*relativeN)
(cumulatedPerc <- cumsum(percentage))
(subjectsFreqTab <- cbind(N, relativeN, percentage, cumulatedPerc))
(subjectsFreqTab <- round(subjectsFreqTab, 2))

#Let's make a BIVARIATE table!
table(survey$sex, survey$studies)

#Compare this line of code and the result with the previous one. What is the difference?
(tab1 <- with(survey, table(sex, studies)))

#What information is this table showing us? Is that very useful?
prop.table(tab1)
prop.table(tab1)*100
addmargins(prop.table(tab1)*100)
addmargins((prop.table(tab1)*100), margin=1)
round(addmargins(prop.table(tab1)*100), 2)

#Let's save the table!
write.table(subjectsFreqTab, "subjectsFreqTab.txt", sep="\t", row.names=FALSE) 
#The above command tells R to save the table we have produced in a particular file type (.txt) and format ("sep" means
#seperator, the kind of "sign" used to tell a program reading the file what seperates the different values). In this
#case, the format is tab (the same as the tab button on your keyboard). sep=" " (space) or sep="," (comma) are other 
#possibilities. Saving the table as a .txt file in the tab.delim (\t) format makes it easy to open the table 
#in Excel. This is the first time(!) that we are exporting data in order to use it with another program.   
#row.names=FALSE is necessary to prevent R from creating a column with a number for each row. We will come back to
#the issue of importing and exporting data on the last day!

#Now, let's make a cross table showing the relationship between the sex (independet variable) and studies 
#(dependet variable).
(crossTab <- with(survey, table(studies, sex)))
(crossTabFin <- round((prop.table(crossTab, margin=2)*100),2))
class(crossTabFin)
crossTabFin[2]
crossTabFin[2,]
crossTabFin[,2]

# Prozentsatzdifferenz!
prozentsatzdiff <- crossTabFin[,1]-crossTabFin[,2]

# Lastly, let's control for a variable with tables! The idea is as follow: if a relationship between two variables is the 
# same independently of a third variable, that third variable has no influence on that relationship. To find out if that
# is true, we need to control for it. We do this by creating separate tables, each for one value that the controlling 
# variable can take on. In this case, we will control for sex, so we will look at the relationship between the two variables
# in a table only with women and only with men. If the relationship is the same or similar in each table, we can say
# that the third variable (sex) has no influence on the third variable!
arrayOfTables <- with(survey, table(bafoeg, studies, sex)) # Note that this returns an ARRAY, which you do not need to know
#anything abot, except that it is for multidimensional (i.e. more than two dimensional) data objects and that the index
#function gets expanded to array[row, column, table]
round(prop.table(arrayOfTables[,,1], margin=2)*100, 2)
round(prop.table(arrayOfTables[,,2], margin=2)*100, 2)

#######################################Excercise 4#############################################################
# 1. Make a table showing the percentage of males and females which have participated in the study, avoiding
# decimals.
# 2. Make a table showing the percentage of males and females which do (not) receive Bafoeg, showing the 1st decimal,
# and add margin totals.
# 3. Make 2 tables with the ONE table command, with one table showing the percentage of females studying different 
# subjects and which do (not) receive Bafoeg and another table showing the percentage of males studying different 
# subjects and which do (not) receive Bafoeg. Use ?table if in doubt.
# 4. Create a single table showing how only the females are distributed over the different subjects. Try using a logical 
# to create a TRUE/FALSE vector and placing that in the index function to filter out only the women! Round to 2 decimals. 
# 5. Create a cross tablulation of sex and bafoeg to check if any discrimination takes place!
#################################################################################################################
#######################################Excercise 4#############################################################
# 1. Make a table showing the percentage of males and females which have participated in the study, avoiding
# decimals.
sexTab <- round(prop.table(table(survey$sex))*100)
# 2. Make a table showing the percentage of males and females which do (not) receive Bafoeg, showing the 1st decimal,
# and add margin totals.
sexBagfoegTab <- round(addmargins(prop.table(table(survey$sex, survey$bafoeg))*100), 1)
# 3. Make 2 tables with the ONE table command, with one table showing the percentage of females studying different 
# subjects and which do (not) receive Bafoeg and another table showing the percentage of males studying different 
# subjects and which do (not) receive Bafoeg. Use ?table if in doubt.
(prop.table(table(survey$sex, survey$studies, survey$bafoeg))*100) # MAKE SURE THE ARGUMENTS ARE IN THE RIGHT ORDER!
# 4. Create a single table showing how only the females are distributed over the different subjects. Try using a logical 
# to create a TRUE/FALSE vector and placing that in the index function to filter out only the women! Round to 2 decimals. 
f <- survey[survey$sex == f,]
f<- droplevels(f)
round(prop.table(table(f$sex, f$studies)*100),2)

#################################################################################################################
#######ggplot2
# Tables are one useful way of summarizing data, but especially larger ones still requires a lot of attention from 
# those reading them. Another way of summarizing data is visually. There are easier ways of generating graphs,
# but we will learn ggplot2, which is far superior to other ways of generating graphics.
library(ggplot2) #Alternatively, you can load the tidyverse (of which ggplot2 forms a part)
diamonds
# Do you notice any difference between this DF and the other ones? This type of DF is called a tibble.
?diamonds
#There is a set way to build a plot. 
ggplot(data=diamonds) + geom_bar(mapping=aes(cut))
#A geom is a geometrical shape used to represent data. Its the kind of object you display in your data. 
ggplot(data=diamonds) + geom_histogram(mapping=aes(price))
# This is a histogram. Histograms are similar to bar charts, except for metric data. Histograms have "bins" which 
# contain a range of values. Experiment with the binwidth option shown below:
ggplot(data=diamonds) + geom_histogram(mapping=aes(price), binwidth=1000)
# There is also the geom freqpoly, which is like geom_histogram, except that it uses a line instead of bars.
ggplot(data=diamonds) + geom_freqpoly(mapping=aes(price))
# "Real" line graphs are not the same as freqpoly geoms, however.
(z <- data.frame(y=c(rep(1:3, 4), rep(5,8)), x=(1:20), year=1990:2009))
ggplot(data=z) + geom_freqpoly(mapping=aes(y), binwidth=1)
ggplot(data=z) + geom_freqpoly(mapping=aes(y), binwidth=2)
# How do you explain the difference in the two visualizations? What is being shown here?
# Consider these two graphs:
ggplot(data=z) + geom_freqpoly(mapping=aes(y), binwidth=1)
ggplot(data=z) + geom_line(mapping=aes(x=year, y=y))
# How do you explain the difference? What is being shown here? Look at the term stat:
?geom_freqpoly 
?geom_line
# So what are stats? Stats (statistical transformations), which form a part of ggplot functions, perform automatic 
# calculations on the data
# they are given. In the case of freqpoly, stat = "bin", which means that the occurences of a given value are counted
# and put into classes or bins. 
# In geom_line, stat="identity". The option "identity" means that the values are taken from the chosen variable as they
# are. So geom_line does not count the occurences of a value. It plots the values as they are. In order to do so,
# however, geom_line requires an explicit x variable (which, in freqpoly, is created by bin). If you want to know more
# about a stat, enter:
?stat_count
ggplot(data=diamonds) + geom_bar(mapping=aes(cut))
ggplot(data=diamonds) + stat_count(mapping=aes(cut))

#So now you know what geom_bar and stats are. But what is a mapping?
?aes
#So when we write aes(x=year, y=y), we are assigning an aesthetic property, in this case "location", to a geom. But 
#we can set other kinds of aesthetics:
ggplot(diamonds) + geom_bar(mapping=aes(cut, colour=cut))
ggplot(diamonds) + geom_bar(mapping=aes(cut, fill=cut))
#Look closely. Do you notice anything in the first graph?
#The second graph may be prettier, but the colours have no real purpose (and as such it is bad practice to portray data
#like that). Consider the following, however:
ggplot(diamonds) + geom_bar(mapping=aes(cut, fill=clarity))
#How many variables are we showing now?
#There are many other kinds of aesthetics with which we will be experimenting below. These are, for example, size, alpha
#(transparency), shape, linetype. Obviously, only certain kinds of aesthetics make sense for certain kinds of objects.

#The last bar chart we made looks nice and the colours are not just pretty, but also have a purpose. But it is difficult
#to compare the percentage of types of clarity between each categories. Is the percentage of very clear diamonds, for 
#example, the same in every cut category? To make it possible to see that, we need to know about position adjustments.
ggplot(diamonds) + geom_bar(mapping=aes(cut, fill=clarity), position = "fill")
#Note that the position is not an aesthetic, so it goes outside the aesthetic option. 
#With position adjustment, we could also place the absolute numbers all side by side. 
ggplot(diamonds) + geom_bar(mapping=aes(cut, fill=clarity), position = "dodge")                                  
#What kind of default position does geom_bar use? Read the documentation, look at the last time we used geom_bar above!                                       
?geom_bar

#Let's have a look at the coolest kind of graphic, scatterplots. For that purpose, we will take a random sample
#from the dataframe, since it is very large.
diamonds100 <- diamonds[sample(1:nrow(diamonds), 100, replace=FALSE),]
#Save the df
save(diamonds100, file="diamonds100.RData")

#Let's continue:
ggplot(diamonds100) + geom_point(aes(carat, price))
ggplot(diamonds100) + geom_smooth(aes(carat, price)) # The grey shows the standard error (SE), which is the
#standard deviation of sample means.Dont't worry about the SE right now.
?geom_smooth # Let's look at methods!
ggplot(diamonds100) + geom_smooth(aes(carat, price), se=FALSE)
# What if we want the line to only show the linear relationship? 
ggplot(diamonds100) + geom_smooth(aes(carat, price), se=FALSE, method=lm)

#One can add one geom over another. For example:
ggplot(diamonds100) + geom_point(aes(carat, price)) + geom_smooth(aes(carat, price), se=FALSE)
ggplot(diamonds100) + geom_point(aes(carat, price, colour=cut)) + geom_smooth(aes(carat, price), se=FALSE, colour="black")

#So far, we have specified aesthetics for each geom. Technically, we could also set aesthetics in ggplot(). These
#characteristics then filter down to all other levels.In this case, for example, we are look at the same two variables
#with the point and smooth geoms. So:
ggplot(diamonds100, aes(carat, price)) + geom_point(aes(colour=cut)) + geom_smooth(se=FALSE, colour="black") + geom_smooth(aes(linetype=cut), se=FALSE)
#We are getting carried away! But the point is this:
#1. You can add as many geom layers as you want to a plot.
#2. If you want some aesthetic to be valid for all layers, put the aes() in ggplot(). These are also automatically over-
# written by aes() at the geom level.
#3. Lastly, if you want to set aesthetic qualities manually, you do so outside of the aes() command.So for example:
levels(diamonds$cut)
ggplot(diamonds, aes(cut, fill = cut)) + geom_bar()
# Of course, one can also set colour manually! 
ggplot(diamonds) + geom_bar(mapping=aes(cut, fill=cut)) + scale_fill_manual(values=c("red", "orange", "yellow", "lightgreen", "darkgreen"))
?scale_fill_manual
# Google "R stats ggplot cheatsheet"
??aes
?colour
#The last important graphic that we need to cover is the boxplot! We have already covered quartiles. Let's look at
#boxplots:
ggplot(diamonds) + geom_boxplot(aes(y=price, x=""), outlier.alpha=.02, width=0.3)
ggplot(diamonds) + geom_boxplot(aes(y=price, x=cut), outlier.alpha=.02)
?geom_boxplot

# Add labels and headings: + labs(y = "var1", x = "var2", title = "Blabla")
# Exporting/saving a graph: ggsave("name.ex", width = 2, height =2 )
# You need to replace ".ex", the file extension, with .pdf, or .jpg etc. 
# ggsave() saves that graph which you have last generated. Look at the bottom right window of R-Studio. Click on the tab 
# "Plots". The graph which appears there is saved with ggsave(). It is saved into your project folder.
# RESEARCH AND CHECK THEMES, MORE ON SETTING AESTHETICS YOURSELF ETC.

# Nicely done! Now you can basically make any kind of plot that you like to analyses data!
##############################################Excercise 5 ########################################################
# 1.  Create a graph showing how often a particular diamond colour appears in the sample. 
# Which colour is "G"? Which colour is "J"?
# 2. Is there a relationship between color and cut? Show in terms of absolute and relative quantities.
# 3. Check if there is a relationship between depth and price
# 4. Draw a straight, red (regression) line through the cloud of dots to see the relationship. Avoid showing the grey area!
# 5. As you can see, although it is possible to calculate a linear relationship, it is in fact not really linear.
# Draw a squiggly line through the cloud of dots, which minimizes the standard deviation.
# 6. On the last graph you created, differentiate between the different levels of cut.
# 7. Add a black linear relationship line to the plot as well. Show the grey SE (standard error)!
# 8.1. Instead of just having two linear regression lines for all dots, add linear regression lines for each type of cut
# (i.e. coloured dots). Avoid the grey standard error area for these! The lines should have the same colour as the dots!
# (Tip: think of filerting down!)
# 8.2. Create a boxplot showing the distribution of the x variable. Check what it stands for and interpret the results.
# 8.3. create several boxplots to compare the distribution of the x variable across the different levels of cut quality.
# 8.4. That looks a bit boring! Let's add colour!

# Nicely done! Now you can basically make any kind of plot that you like to analyses data!
####################################################################
##############################################Excercise 5 ########################################################
# 1.  Create a graph showing how often a particular diamond colour appears in the sample. 
ggplot(diamonds100, aes(x=color)) + geom_bar()
# Which colour is "G"? Which colour is "J"?
# 2. Is there a relationship between color and cut? Show in terms of absolute and relative quantities.
ggplot(diamonds100, aes(x=color, fill=cut)) + geom_bar()
ggplot(diamonds100, aes(x=color, fill=cut)) + geom_bar(position="fill")
# 3. Check if there is a relationship between depth and price
ggplot(diamonds100, aes(x=depth, y=price)) + geom_point()
# 4. Draw a straight, red (regression) line through the cloud of dots to see the relationship. Avoid showing the grey area!
ggplot(diamonds100, aes(x=depth, y=price)) + geom_point() + geom_smooth(aes(colour="red"), method=lm, se=FALSE)
# 5. As you can see, although it is possible to calculate a linear relationship, it is in fact not really linear.
# Draw a squiggly line through the cloud of dots, which minimizes the standard deviation.
ggplot(diamonds100, aes(x=depth, y=price)) + geom_point() + geom_smooth(colour="red", se=FALSE)
# 6. On the last graph you created, differentiate between the different levels of cut.
ggplot(diamonds100, aes(x=depth, y=price)) + geom_point(aes(colour=cut)) + geom_smooth(colour="red", se=FALSE)
# 7. Add a black linear relationship line to the plot as well. Show the grey SE (standard error)!
ggplot(diamonds100, aes(x=depth, y=price)) + geom_point(aes(colour=cut)) + geom_smooth(colour="red", se=FALSE) + geom_smooth(method=lm, colour="black")
# 8.1. Instead of just having two linear regression lines for all dots, add linear regression lines for each type of cut
# (i.e. coloured dots). Avoid the grey standard error area for these! The lines should have the same colour as the dots!
# (Tip: think of filerting down!)
ggplot(diamonds100, aes(x=depth, y=price, colour=cut)) + geom_point() + geom_smooth(se=FALSE) + geom_smooth(colour="red", se=FALSE) + geom_smooth(method=lm, colour="black")
# Obviously, one would probably never want to create a plot. But now you should understand the uses of aes() and 
# adding different layers to a plot. Let's make a new kind of plot.
# 8.2. Create a boxplot showing the distribution of the x variable. Check what it stands for and interpret the results.
ggplot(diamonds) + geom_boxplot(aes(y=x, x=""), outlier.alpha=.02, width=0.3)
# 8.3. create several boxplots to compare the distribution of the x variable across the different levels of cut quality.
ggplot(diamonds) + geom_boxplot(aes(y=x, x=cut), outlier.alpha=.02)
# 8.4. That looks a bit boring! Let's add colour!
ggplot(diamonds) + geom_boxplot(aes(y=x, x=cut, fill=cut), outlier.alpha=.02)
# Nicely done! Now you can basically make any kind of plot that you like to analyses data!
####################################################################
####################################################################
library(tidyverse)
load("survey.RData")

#Comparing tibbles and dataframes
survey2 <- survey
survey <- as.tibble(survey)

#####################dplyr
#select()
smallerDF <- select(survey, age, sex, bafoeg)
names(smallerDF)
smallerDF <- select(survey, age:bafoeg)
names(smallerDF)

#rename(): new names go left, old names go right 
smallerDFrenamedVars <- rename(smallerDF, age2=age, bafoeg2 = bafoeg)
names(smallerDFrenamedVars)

#arrange() for rows
(surveyStudies <- arrange(survey, studies))
(surveyStudiesSex <- arrange(survey, desc(studies), sex))
View(surveyStudiesSex)

#filter() for rows 
surveySex1 <- filter(survey, sex == 1)
nrow(survey)
nrow(surveySex1)
surveySex1 <- filter(survey, sex == 1, studies != "medicine")
nrow(surveySex1)
# To filter out several factor levels, use filter(studies %in% c("medicine", "social Science"))

#mutate() is used to calculate new variables, which are automatically attached to the DF!
mutate(survey, varAge=mean(age)-age)
names(survey)
# Variable is automatically attached, but the "new" DF is not saved automatically!
survey <- mutate(survey, varAge=mean(age)-age)
names(survey)
# What happens if we give the new variable no name?
survey <- mutate(survey, mean(age)-age)
names(survey)

#summarise() creates a new df with the output of the applied function
summarise(survey, meanAge=mean(age))

#Please note: summarise() and mutate() cannot just take any Base R function and work with it. Some
#functions that work in Base R, for example, will just not work with these dplyr package functions.
#For example, if you want to create a variable containing the number of cases, mutate(df, nrow(var))
#will not work! Instead, you need to specify mutate(df, var, n()). As always, when a function does not
#work, look it up with ?. The descriptions of summarise() and mutate() that will open up list the
#functions that work within summarise() and mutuate().

#The pipe (%>%) and groub_by()
# As mentioned previously, R does not automatically remember objects, unless they are given a name. For some things
# one may want to do, that means that one would have to create objects whose only purpose is to be used for further
# calculations. The so-called pipe was invented to avoid exactly that. Don't forget: the pipe was designed for usage 
# with other tidyverse packages such as dplyr. Trying to apply it without using dplyr functions is likely no to work 
# at times! Learning by doing is the way to go! 
# Let's pretend we do not know the pipe yet to illustrate what I mean.
names(survey)
# Let's say we only want to assess a particular subset of the data, those 21 years of age or younger.
dfEqualSmaller21 <- filter(survey, age <= 21)  
nrow(dfEqualSmaller21)
#Now we want to know how how many of those receive Bafoeg.
round(mean(dfEqualSmaller21$bafoeg)*100, 2)
# So now we know how many students of age 21 or younger receive bafoeg. As you can see, dfEqualSmaller21 was only created
# to then do calculations with that subset. That is messy programming. Also, how could we go about finding out the percentage
# of students receiving Bafeog in each category of the studies variable? That would require a lot of code. We could, for 
# example, create a dataframe with only those studying social sciences and then calculate the percentage of those receiving
# bafoeg from that df, then do the same for the other two categories. That would be messy code. Both problems can be solved
# by using the pipe and the group_by() function. # group_by() "breaks up" a DF into subsets of data according to a given 
# factor's levels. Operations applied to such a DF # are then applied seperately to all the subsets of each level.  
# ungroup() undoes the subsetting. Always ungroup to avoid problems later!!!

# Let's try doing what we have done in the code above already using the pipe, which one could
# read as meaning "then" and use group_by() to calcualate the percentage of those receiving bafoeg in each category of 
# studies.
survey3 <- survey %>% filter(age <=21) %>% group_by(studies) %>% summarise(meanBafoeg=mean(bafoeg)) %>% ungroup()
# Ungroup makes more sense when the df that was grouped is used afterwards, which is not the case when producing a new 
# df with summarise(), but it can't harm.

# Let's look at the difference between summarise() above and mutate() again:
survey3 <- survey %>% filter(age <=21) %>% group_by(studies) %>% mutate(meanBafoeg=mean(bafoeg)) %>% ungroup()

# Another example, calcualting mean ages for each studies level:
avAgeAccStudies<- survey %>%
  group_by(studies) %>%
  summarise(meanAge=mean(age))%>%
  ungroup()

# Often, it is necessary to change the format of the data. The rule for tidy data was: each variable one column, each 
# observation one row. This may not apply when we generate particular graphics with ggplot2. The long format contains only
# two columns, one containing the names of all the various variables and the second column containing the actual data.
# In the wide format, for example, the factor levels of an original factor may be converted to column names (e.g. names
# of political parties are actually a factor variable, but one may want to reorganize a df so that each party's name becomes
# a column name)
# To convert a tidy df into the long format, we can use the function gather(), to put data into the wide format, we can use
# spread().
# gather(select(varToBeChanged), key="namesOfVarForNewCol", value="nameOfOriginalColumn")
# spread(key=varWithVarNames, value=ColWithValuesFromMultipleVars)
surveyLongFormat <- survey %>% gather(age:studies, key="allVarNames", value="values")
# How can we find out if it worked as we had intended? Let's make a table quick!
table(surveyLongFormat$allVarNames)

surveyWideFormat <- surveyLongFormat %>% spread(key=allVarNames, value=values)

#Why does this not work? If you look at surveyWideFormat, how should R know which values from the different variables, 
#all found in one column, belong to a single person? We need an id variable.
survey$id <- 1:nrow(survey)
#Now you can rerun the code! Check what happens with the id variable!
surveyLongFormat <- survey %>% gather(age:studies, key="allVarNames", value="values")
table(surveyLongFormat$id)

surveyWideFormat <- surveyLongFormat %>% spread(key=allVarNames, value=values)

# Using dplyr with ggplot: One can use the pipe and feed the output directly into a graph. ggplot() should, in this case,
# not make the df explicit, since it is fed into the function with the pipe. For instance:
survey %>%
  mutate(agePlus2=age+2) %>%
  ggplot() + geom_bar(aes(agePlus2))

####################################Excercise 6 ######################################
# 1. Let's practice with a new DF. Let's work with mtcars. First, get an overview of whether all variables that 
#should be factors are factors.
# 2. Convert the variables that should be factors into factors. Use summary() and table() if you are not sure what kind of
# variable you are delaing with!
# Don't worry about labels and levels. 
# 3. Without creating new subset dataframes, calculate the mean, sd and provide the n for MPG for each number of cylinders.  
# 4. Once you have managed to create the expected 3x3 table (without col and row names), add a column with the percentage
# of cases. 
# 5. Create a boxplot showing the mpg for each level of cyl
# 6. Calculate the same summary values, mean and sd, for only those cars which have 8 cylinders and 3 gears.  
# 7. Create a scatterplot for mpg and weight, colour according to cylinders.
# 8. Create a DF with only that subset of cylinders which scores highest on the wt and lowest on the mpg variables, since you
# want to display these in your paper in the form of a table. Sort the obserations from highest to lowest first for the 
# wt and than the mpg variables, which should be shown in that order first.
# Include only the following variables in the DF: mpg, disp, wt, qsec, gear.
# 9. Finally, calculate the ratio of mpg to weight (1:x) and call the variable wpmpg (weight per mpg). Attach to dfmtcars:

############################################################################################
####################################Excercise 6 ######################################
# 1. Let's practice with a new DF. Let's work with mtcars. First, get an overview of whether all variables that 
#should be factors are factors.
(dfmtcars<- as.tibble(mtcars))
# 2. Convert the variables that should be factors into factors. Use summary() and table() if you are not sure!
# Don't worry about labels and levels. 
summary(dfmtcars)
?summary
dfmtcars$cyl <- factor(dfmtcars$cyl) 
dfmtcars$vs <- factor(dfmtcars$vs)
dfmtcars$am <- factor(dfmtcars$am)
dfmtcars$gear <- factor(dfmtcars$gear)
dfmtcars$carb <- factor(dfmtcars$carb)
#The variable hp may or may not be a factor. The first ten rows show lots of 0s and 5s and 110 is repeated. There are
#also no decimals.  Let's check with table.
table(dfmtcars$hp)
#The table suggests that we are dealing with a factor with a lot of categories. But since the N is small and most 
#categories only contain one case, it does not make much sense to convert it to a factor.
# 3. Without creating new subset dataframes, calculate the mean, sd and provide the n for MPG for each number of cylinders.  
table(dfmtcars$cyl)
(sumData <- dfmtcars %>% group_by(cyl) %>% summarise(meanMPG=mean(mpg), sdMPG=sd(mpg), n=n())) 
# 4. Once you have managed to create the expected 3x3 table (without col and row names), add a column with the percentage
# of cases. 
(sumData2 <- sumData %>% mutate(nPerc=n/sum(n)*100))
# 5. Create a boxplot showing the mpg for each level of cyl
ggplot(dfmtcars, aes(cyl, mpg)) + geom_boxplot()
# 6. Calculate the same summary values, mean and sd, for only those cars which have 8 cylinders and 3 gears.  
(sumData <- dfmtcars %>% filter(cyl==8, gear==3) %>% summarise(meanMPG=mean(mpg), sdMPG=sd(mpg), n=n())) 
# 7. Create a scatterplot for mpg and weight, colour according to cylinders.
ggplot(dfmtcars, aes(mpg, wt, colour=cyl)) + geom_point()
# 8. Create a DF with only that subset of cylinders which scores highest on the wt and lowest on the mpg variables, since you
# want to display these in your paper in the form of a table. Sort the obserations from highest to lowest first for the 
# wt and than the mpg variables, which should be shown in that order first.
# Include only the following variables in the DF: mpg, disp, wt, qsec, gear.
data4cylTab <- dfmtcars %>% filter(cyl==4) %>% select(mpg, wt, disp, qsec, gear) %>% arrange(desc(mpg, wt))
# 9. Finally, calculate the ratio of mpg to weight (1:x) and call the variable wpmpg (weigt per mpg). Attach to dfmtcars:
(dfmtcars <- dfmtcars %>% mutate(wpmpg=wt/mpg))

############################################################################################
########### Three ways to write the same code
# Given what we have learnt so far, there are three ways that we can write the same kind of code. In the examples below, 
# we want to calculate the standard deviation. Base R has the function sd() for that, but the formula contains (n-1) in the 
# divisor. Since we want to calculate the sd as a descriptive stats measure, we could do the following:
library(tidyverse)

#Creating many "intermediary" objects
x <- 1:10
meanX <- mean(x)
df <- data.frame(x, meanX)
df$diff <- df$x-df$meanX
df$diffSq <- df$diff^2
df$sumDiffSq <- sum(df$diffSq)
df$var <- df$sumDiffSq/nrow(df)
df$sd <- sqrt(df$var)

#Using brackets: 
df$sd2 <- sqrt((sum((df$x-df$meanX)^2))/nrow(df))

# Using the pipe. We first create the DF that we want to work with separately, since that is what the pipe was designed for: 
# to work with DFs, not to create them. 
x <- 1:10
meanX <- mean(x)
df <- data.frame(x, meanX)

sd <- df %>%
  mutate(diff = x-meanX,
         diffSq = diff^2,
         sumDiffSq = sum(diffSq),
         var = sumDiffSq/n(),
         sd = sqrt(var))

########################### The apply family ##############################################
# As has already been explained previously, R is a language that works with vectors. Different
# kinds of functions can be applied to different kinds of objects. Some functions can, for example,
# only be applied to numeric vectors and not to dataframes. At times, however, we may want to 
# apply one function to all the variables in a dataframe or another larger object. That can be
# done by using a function from the apply family:

#apply() takes two-dimensional objects
applyDF <- data.frame(x=rep(c(1,2), times=c(3,7)), y=rep(c(3,4), times=c(3,7)), z=c(1,5,4,7,8,2,9,3,10,6))
applyDF <- as.tibble(applyDF)

applyDF$rowMean <- apply(applyDF, 1, mean) #apply(X, MARGIN, FUN, ...)
(colMean <- apply(applyDF, 2, mean))

(applyDF2 <- apply(applyDF, 2, sort)) # sort(x, decreasing = FALSE, ...)
(applyDF2 <- apply(applyDF, 2, sort, decreasing=TRUE)) 

(applyDF3 <- tapply(applyDF$y, applyDF$x, sum)) # tapply(observations, group/"factor level", function)
?tapply

#tapply is like table, but does not just count
table(applyDF$y, applyDF$x)

############ Writing your own functions
# In R, it is possible to write your own functions. You will want to write functions in order to avoid having to repeat code 
# unnecessarily. Let's say we want to calculate the standard deviation for various variables. With the methods we learned,
# one would just copy and paste the code, insert the new variable (perhaps called y) and run the code again. This kind of 
# writing code is redundant, difficult to read and prone to error. In such cases, it makes sense to write a function. 
sdDesc <- function(x){
  df <- data.frame(x, meanX)
  df$diff <- df$x-df$meanX
  df$diffSq <- df$diff^2
  df$sumDiffSq <- sum(df$diffSq)
  df$var <- df$sumDiffSq/nrow(df)
  df$sd <- sqrt(df$var)
}

# Functions can be given names, just like we would name objects. To create the function with ctrl+enter, click into the word
# function, otherwise it will not work. 
# The function "function" allows you to create a function. The round brackets behind the word function states the function's
# arguments. In this case, we want to pass a vector called x to the function sdDesc to calcualte the standard deviation.
# What the function actually does is defined in the squirly brackets. The correct way to go about writing a function is to 
# write the code to do the calculations and then to insert it into a function to make it work.
# Note: Be careful when using dplyr commands within functions, because syntax changes occur. To avoid problems without
# wanting to read up on the issue (which you may: https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html), 
# just use Base R functions. 
# Let's use the function we have created!
sdDesc(x)


#MEAN X
# Nothing happens! Why? Usually, a function automatically prints the last ouputed value, if it is not assigned a name (which
# is what we did above). For example: 
sdDesc <- function(x){
  df <- data.frame(x, meanX)
  df$diff <- df$x-df$meanX
  df$diffSq <- df$diff^2
  df$sumDiffSq <- sum(df$diffSq)
  df$var <- df$sumDiffSq/nrow(df)
  sqrt(df$var)
}

sdDesc(x)

# But we want to see the whole dataframe. This can be achieved with print():
sdDesc <- function(x){
  df <- data.frame(x, meanX)
  df$diff <- df$x-df$meanX
  df$diffSq <- df$diff^2
  df$sumDiffSq <- sum(df$diffSq)
  df$var <- df$sumDiffSq/nrow(df)
  df$sd <- sqrt(df$var)
  print(df)
}

a <- sdDesc(x)
sdDesc(y)  

# This was only one tiny example concerning functions and what one can do with them. Let's expand our knowledge a little bit more.
# As with other functions, it is possible to set parameters for the execution of a function and to provide default values. For
# example:
sdDesc <- function(x, a=2){
  df <- data.frame(x, meanX)
  df$diff <- df$x-df$meanX
  df$diffSq <- df$diff^2
  df$sumDiffSq <- sum(df$diffSq)
  df$var <- df$sumDiffSq/nrow(df)
  df$sd <- sqrt(df$var)
  df$sdRounded <- round(df$sd, a)
  print(df)
}

sdDesc(x)
sdDesc(x, 3)

############# Calculating different coefficients
######
# Packages for coefficients
library(tidyverse)
library(stats)
library(Hmisc)
library(psych)
library(vcd)

#To calculate the different types of coefficients, it is necessary to consider which measurement level 
#each variable has. Nominal variables should be factors (or in any case numeric vectors), ordinal variables 
#must be ordered factors (!). Besides being able to calculate them, one should obviously know what they
#actually do, so as to be able to check whether they can actually be applied to the data (weaknesses of
#coefficients) and to be able to interpret them (see Benninghaus, Deskriptive Statistik on the course
#handout). It is not necessary to know how the coefficients are actually calculated for the exam. See
#the slideshow for an overview of the different coefficients and functions.

#Let's get an overview over the different variables in the diamonds dataset we will be using! Which 
#measurement level do they have?
summary(diamonds[1:200,])
psych::describe(diamonds[1:200,])

#We can also check by converting the DF to a tibble! Let's create two subsets of the data to allow 
#faster processing of the data.
diamonds100 <- as.tibble(diamonds[sample(1:nrow(diamonds), 100, replace=FALSE),])
diamonds1000 <- as.tibble(diamonds[sample(1:nrow(diamonds), 1000, replace=FALSE),])

# Another way to find out what kind of variables we are dealing with! 
class(diamonds100$color)
# Use levels(diamonds100$color) to find out the values the factor can take on!

stats::cor(as.numeric(diamonds100$cut), as.numeric(diamonds100$color), method="kendall") 
#Or "pearson" (default), "kendall", "spearman", but only shows statistic
?diamonds
assocstats(table(diamonds100$cut, diamonds100$color)) # Contingency coefficient, Cramer's V

class(diamonds100$price)
a <- cor.test(as.numeric(diamonds100$cut), as.numeric(diamonds100$color), method = "kendall")
b <- cor.test(as.numeric(diamonds1000$cut), as.numeric(diamonds1000$color), method = "kendall")

c <- cor.test(as.numeric(diamonds100$cut), as.numeric(diamonds100$color), method = "spearman")
d <- cor.test(as.numeric(diamonds1000$cut), as.numeric(diamonds1000$color), method = "spearman")

e <- cor.test(diamonds1000$price, diamonds1000$carat, method = "pearson")

library(DescTools)
SomersDelta(diamonds100$cut, diamonds100$color)

######### What is z-standardization?
## THIS SECTION IS NOT RELEVANT FOR THE EXAM, BUT IS MEANT TO ILLUSTRATE WHAT THE STANDARD NORMAL DISTRIBUTION AND 
## Z-STANDARDIZATION IS.
## Let's imagine we have a sample dataset showing the age of bachelor students at the university.
(a <- round(rnorm(500, mean=20, sd=2)))
a <- round(rnorm(500, mean=20, sd=2))
unstandard <- a[a>17]
standExp <- data.frame(unstandard)

## The distribution is as follows:
ggplot(standExp) + geom_histogram(aes(unstandard))

#According to the z-standardization formula (which is applied to each value of a given variable x), first the 
#mean of all values in the sample is minused from each value. 
meanOfAge<- mean(standExp$unstandard)
standExp$xMinusMu <- (standExp$unstandard-meanOfAge)
ggplot(standExp) + geom_histogram(aes(xMinusMu))
#As we can see, the distribution is now centered on zero, but it is exactly the same. We have basically shifted
#so that the mean value is at its center and equal to zero. 
mean(standExp$xMinusMu)

#Now, let's divide out the standard deviation.
SD <- sd(standExp$unstandard)
standExp$divBySD <- with(standExp, xMinusMu/SD)
ggplot(standExp) + geom_histogram(aes(divBySD))
#Dividing the standard deviaition out changes the distribution so that it becomes similar to the standard normal
#distribution. The standard normal distribution is a theoretical distribution in which, within the different standard
#deviaitions around 0 lie a particular number of cases.
#Whether a distribution of a z-standardized variable looks like the standard normal distribution depends on the actual
#values, how many there are in the sample and how they are actually distributed. Consider the following example,
#which we will revisit later. Highlight the block of code and observe how the graph is changing:
genericVar <- 1:200
avSample30<- replicate(2000, mean(sample(1:6, size=30, replace=TRUE)))
df3 <- data.frame(avSample30, genericVar)
ggplot(df3, aes(avSample30)) + 
  geom_histogram(aes(y=..density..), colour="black" , fill="white") + 
  labs(x="Bla", y="Density") +
  stat_function(fun=dnorm, args=list(mean=mean(df3$avSample30), 
                                     sd=sd(df3$avSample30)), colour ="black", size=1)


####################################### Revision excercise 1 ###########################################
library(tidyverse)
# Draw a random sample of 300 diamonds to work with for this excercise. 
diamondsSample<- sample_n(diamonds, 300)

#1. Get an overview over the different kinds of variables in the dataframe with one function.

#2. Create a dataset with the 30 most expensive diamonds and call it expDiamonds. Use the pipe and the 
# function slice().

#3. Create a chart that shows the number of diamonds for each type of cut. Add a descriptive heading 
# to the graph. Save it. Look at the ggsave documentation. What useful options does it have?
# dpi, scale, width, height

#4. Maybe the diversity in cut of the 30 most expensive dimamonds can be explained by another variable? Create
# a bar chart which also shows the distribution of clarity over the different categories of cut.

#5. Create a graph showing the distribution of the price variable of diamonds. Give it a descriptive title.

#6. Determine the value of the most valuable diamond.

#7. Create a boxplot gaphic which permit the comparison of the distributions of the two variables x (length) and 
# width (y). Rename the variable x to length and y to width first. 
# Note: in order to be able to portray both these variables in a single boxplot graphic, it is necessary
# to convert the two variables into long (as opposed to tidy) format! This means you need to create a column with 
# the two variable names and a column containing the values of these two variables. Use gather!)

#8. Create a table showing the relative frequency with which the different types of cuts occur in Diamonds.

#9. Create a table showing the relative frequency with which diamonds with different cuts and colours occur in Diamonds.

#10. Add margins to both sides of the table.

#11. Cross-tabulate clarity and colour and comment on the results.

#12. Check by means of correltation whether there is a relationship between cut and colour (Hints: What measurement 
# level do these two variables have? The slideshow on Studon contains the correlation coefficients you can calculate for the different measurement
# levels. Assign the output with the arrow to be able to look at it!).


################################################

###################################### Revision excercise 1: Answers #############################################
library(tidyverse)
# Draw a random sample of 300 diamonds to work with for this excercise. 
diamondsSample<- sample_n(diamonds, 300)

#1. Get an overview over the different kinds of variables in the dataframe with one function.
library(psych)
psych::describe(diamondsSample)
summary(diamondsSample)

#2. Create a dataset with the 30 most expensive diamonds and call it expDiamonds. Use the pipe and the 
# function slice().
?slice
expDiamonds <- diamondsSample %>%
  arrange(desc(price)) %>%
  slice(1:30)

#3. Create a chart that shows the number of diamonds for each type of cut. Add a descriptive heading 
# to the graph. Save it. 
expDiamonds %>% 
  ggplot() + geom_bar(aes(cut)) + labs(title="The 30 most expensive diamonds")
ggsave("30diamonds.pdf")
# Look at the ggsave documentation. What useful options does it have?
#dpi, scale, width, height

#4. Maybe the diversity in cut of the 30 most expensive dimamonds can be explained by another variable? Create
# a bar chart which also shows the distribution of clarity over the different categories of cut.
expDiamonds %>% 
  ggplot() + geom_bar(aes(cut, fill=clarity), position="fill") + labs(title="Cut and clarity of the 30 most 
                                                                      expensive diamonds")
# From ?diamonds : clarity a measurement of how clear the diamond is (I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1,
# IF (best)) 

#5. Create a graph showing the distribution of the price variable of diamonds. Give it a descriptive title.
diamonds %>% 
  ggplot() + geom_histogram(aes(price)) + labs(title="The distirbution of the depth of the diamonds dataset")

#6. Determine the value of the most valuable diamond.
max(diamonds$price)

#7. Create a boxplot gaphic which permit the comparison of the distributions of the two variables x (length) and 
# width (y). Rename the variable x to length and y to width first. 
# Note: in order to be able to portray both these variables in a single boxplot graphic, it is necessary
# to convert the two variables into long (as opposed to tidy) format! This means you need to create a column with 
# the two variable names and a column containing the values of these two variables. Use gather!)
expDiamonds %>%
  rename(length = x, width = y) %>%
  select(length, width) %>%
  gather("variables", "values") %>%
  ggplot() + geom_boxplot(aes(variables, values))

#8. Create a table showing the relative frequency with which the different types of cuts occur in Diamonds.
(tab1 <- round(prop.table(table(Diamonds$cut))*100, 2))

#9. Create a table showing the relative frequency with which diamonds with different cuts and colours occur in Diamonds.
(tab2 <- round(prop.table(table(Diamonds$cut, Diamonds$color))*100, 2))
# From ?diamonds: diamond colour, from D (best) to J (worst)

#10. Add margins to both sides of the table.
(tab2 <- round(addmargins(prop.table(table(Diamonds$cut, Diamonds$color))*100), 2))

#11. Cross-tabulate clarity and colour and comment on the results.
crossTab <- with(diamonds, table(clarity, color))
(crossTabDiamonds <- addmargins(round((prop.table(crossTab, margin=2)*100),2)))

#12. Check by means of correltation whether there is a relationship between cut and colour (Hints: What measurement 
# level do these two variables have? The slideshow on Studon contains the correlation coefficients you can calculate for the different measurement
# levels. Assign the output with the arrow to be able to look at it!).
library(stats)
corTestResult<- cor.test(as.numeric(diamonds$cut), as.numeric(diamonds$color), method="kendall")
corTestResult


############################### Doing regression in R ###############################################
library(tidyverse)
load("diamonds100.RData")
#Here, we are working with a RANDOM SAMPLE, diamonds100, which each of us created on their own. Since these samples
#are random, but come form the same subset of all the diamonds in the world, the values you will find are similar,
#but NOT the same as the ones that are described in the comments. 
#Calculating the coefficients of the model: #object=lm(outcomeVar ~ predictorVar, data=DF)
lm(price ~ carat, diamonds100)
#The output is not very informative, however. We do not know, for example, if the the coefficients are significant.
#That is because the model's output needs to be assigned a name and applied summary(). 
regCaratPrice <- lm(price ~ carat, diamonds100)
summary(regCaratPrice)

#Let's make a graph:
ggplot(diamonds100, aes(carat, price)) + geom_point() + geom_smooth(method=lm)

# What does this info tell us?
# Y = b_0 + b_1(x_i) 
# How much is the predicted price for a diamond of one carat?
(y  <-  -2673.4 + 8280.3*1)
# How much is the predicted price for a diamond of two carat?
(y  <-  -2673.4 + 8280.3*2)

# The square root of R^2 is Pearson's r. R^2 does not change whether the two variables with which it is calculated
# are standardized or not. 
# Let's use the scale() function to z-standardize (see the powerpoint) the variables and compare.
diamonds100$priceSD<- scale(diamonds100$price)
diamonds100$caratSD<- scale(diamonds100$carat)
regCaratPriceSD <- lm(priceSD ~ caratSD, diamonds100)
summary(regCaratPriceSD)
# R^2 remains the same. What changes are the intercept and the X coefficient. 

# To take the squareroot to get Pearson's r:
(rCaratPrice <-  sqrt(9.319e-01)) #That number in brackets was copied and pasted form the output of summary().
#So Pearson's r is 0.96. For every one standard deviation change in carat, price increases by 0.96 standard deviations.
#Let's look at the graph again.
ggplot(diamonds100, aes(caratSD, priceSD)) + geom_point() + geom_smooth(method=lm)
#It also looks the same. The reason is that although the variables were standardized, their relative location to one 
#another stays the same. What changes are the X and Y axis labels. 

#Does the model fit the data? 
# Look at R^2. Multiply by 100 to talk about it in percentages. Here, carat can 
# account for 86.85% of the variance in price.

# Is the model significant? 
# Look at the F-test's p-value.
# It says: 2.2e-16, which is a very small number. How does that work?
### 2.5e-5 means 2.5 times ten to the minus five power, or 0.000025 (i.e. move the comma back 5 places)
### 2.5e5 means 2.5 times ten to the five power, or 250000 which is the same as 250.000 (i.e. move the comma forward 5 places)
# If p is smaller than .05, then the zero hypothesis (that there is no relationship between the two variables in the
# population) can be rejected. 
# The p-value is (much) smaller than .05. The H_0 can be rejected. We can be more certain that their is a relationship 
# between carat and price not just for this sample, but for other samples and by implication the entire population.

# Are the assumptions concerning residuals met (ASSUMPTIONS ARE NOT IMPORTANT FOR THE EXAM)?
plot(regCaratPrice) 
# This command creates 4 plots. See the slideshow!


