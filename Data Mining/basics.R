# Entering data with the C command
typos = c(2,3,0,3,1,0,0,1)
typos

# Functions on the data
mean(typos)
median(typos)
max(typos)
var(typos)

# Data with Vectors

typos.draft1 = c(2,3,0,3,1,0,0,1)
typos.draft2 = c(0,3,0,3,1,0,0,1)

typos.draft1 = c(2,3,0,3,1,0,0,1)
typos.draft2 = typos.draft1 # make a copy
typos.draft2[1] = 0 # assign the first page 0 typos
typos.draft2

# Accessing the Vector Array

typos.draft2 # print out the value [1] 0 3 0 3 1 0 0 1
typos.draft2[2]

# Slicing data

typos.draft2[4] # 4th page
typos.draft2[-4] # all but the 4th page
typos.draft2[c(1,2,3)] # fancy, print 1st, 2nd and 3rd.

max(typos.draft2)
typos.draft2 == 3
which(typos.draft2 == 3)

n = length(typos.draft2) # how many pages
pages = 1:n # how we get the page numbers
pages # pages is simply 1 to number of pages
pages[typos.draft2 == 3] # logical extraction. Very useful
head(typos.draft2)

(1:length(typos.draft2))[typos.draft2 == max(typos.draft2)] # All combined

#Time for stocks tracking

x = c(45,43,46,48,51,46,50,47,46,45)
mean(x)
median(x)
max(x)
min(x)
summary(x)
x = c(x,48,49,51,50,49) # append values
length(x)
x[16] = 41
cat(x)
x[17:20] = c(40,38,35,40)

#The moving average simply means to average over some previous number of days. 
#Suppose we want the 5 day moving average (50-day or 100-day is more often used). 
#Here is one way to do so. We can do this for days 5 through 20 as the other days don’t have enough data.

day = 5
mean(x[day:(day+4)])

#The trick is the slice takes out days 5,6,7,8,9
day:(day+4)

#cumulative maximum
cummax(x)
cummin(x)

#For example, suppose the yearly number of whales beached in Texas during the period 1990 to 1999 is
#74 122 235 111 292 111 211 133 156 79

whale=c(74,122,235,111,292,111,211,133,156,79)
mean(whale)
var(whale)
sd(whale)

#6 fill ups mileage.
miles = c(65311, 65624, 65908, 66219, 66499, 66821, 67145, 67447)
diff(miles) #Number of files per fill-up

#commute times for 10days
times =c(17,16,20,24,22,15,21,15,17,22)
#fix value 24 should be 18
times[4] = c(18)
cat(times)
sum(times >= 20)
less17 <- sum(times <=17)
(less17/length(times)) * 100

#Types of data 
#categorical = race, gender, etc..
#discrete numeric = age
#continuous numeric = weight
#A simple, intuitive way to keep track of these is to ask what is the mean (average)? 
#If it doesn’t make sense then the data is categorical (such as the average of a non-smoker and a smoker), 
#if it makes sense, but might not be an answer (such as 18.5 for age when you only record integers integer)
#then the data is discrete otherwise it is likely to be continuous.

#Tables smokers and non
x=c("Yes","No","No","Yes","Yes")
table(x)
factor(x)


#Suppose, a group of 25 people are surveyed as to their beer-drinking preference. 
#The categories were (1) Domestic can, (2) Domestic bottle, (3) Microbrew and (4) import. The raw data is
#3411343313212123231111431
beer = c(3 ,4 ,1 ,1 ,3 ,4 ,3 ,3 ,1 ,3 ,2 ,1 ,2 ,1 ,2 ,3 ,2 ,3 ,1 ,1 ,1 ,1 ,4 ,3 ,1)
barplot(beer)
barplot(table(beer))
barplot(table(beer)/length(beer)) 
table(beer)/length(beer)

#pie chart
beer.counts = table(beer) # store the table result
pie(beer.counts) # first pie -- kind of dull
names(beer.counts) = c("Domestic\n can","Domestic\n bottle",
                         "Microbrew","Import") # give names > pie(beer.counts) # prints out names
pie(beer.counts,col=c("purple","green2","cyan","white")) # now with colors

#The median average deviation (MAD) is also a useful, resistant measure of spread
air <-data("airquality")
head(airquality)
mad(airquality$Temp)

#Stem and Leaf
#Suppose you have the box score of a basketball game and find the following points per game for players on both teams
#2 3 16 23 14 12 4 13 2 0 0 0 6 28 31 14 4 8 2 5

scores <- c(2 ,3 ,16 ,23 ,14 ,12 ,4 ,13 ,2 ,0 ,0 ,0 ,6 ,28 ,31 ,14 ,4 ,8 ,2 ,5)
apropos("stem")
stem(scores)
stem(scores,scale=2)
