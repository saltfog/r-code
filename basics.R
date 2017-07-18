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

(1:length(typos.draft2))[typos.draft2 == max(typos.draft2)] # All combined

x = c(45,43,46,48,51,46,50,47,46,45)
day=5;
mean(x[day:(day+4)])
day:(day+4)

#cumulative maximum
cummax(x)

#cumulative min
cummin(x)

whale = c(74, 122, 235, 111, 292, 111, 211, 133, 156, 79)  
mean(whale)
var(whale)
sqrt(var(whale))
sqrt( sum( (whale - mean(whale))^2 /(length(whale)-1)))
sd(whale)

#Suppose you keep track of your mileage each time you fill up. At your last 6 fill-ups the mileage was
#65311 65624 65908 66219 66499 66821 67145 67447
miles = c(65311, 65624, 65908, 66219, 66499, 66821, 67145, 67447)
x = diff(miles)
cat(x)

#Commute Times
commute <- c(17, 16, 20, 24, 22, 15, 21, 15, 17, 22)
comute_diff = diff(commute)
cat(comute_diff)














data("mtcars")
plot(cars$dist~cars$speed, # y~x
     main="Relationship between car distance & speed", # Plot Title
     xlab="Speed (miles per hour)", #X axis title
     ylab="Distance travelled (miles)", #Y axis title
     xlim=c(0,30), #Set x axis limits from 0 to 30
     ylim=c(0,140), #Set y axis limits from 0 to 140
     xaxs="i", #Set x axis style as internal
     yaxs="i", #Set y axis style as internal 
     col="red", #Set the color of plotting symbol to red
     pch=3) #Set the plotting symbol to filled dots

