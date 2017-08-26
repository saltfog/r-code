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

#Plotting
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


#Binomial estimator
#Number of Sixes in a Dice Roll simulation
x <- rbinom(1,25,1/6)
phat <- x/25
phat

#Function to repeat 1000 times to get a 6 in a 1000 rolls
doone <- function(){   
x <- rbinom(1,50,1/6) 
p <- x/50
p 
}
p.sim<-replicate(1000,doone())
hist(p.sim,breaks=15)


#Simulated normally distributed data - rnorm()
x<-rnorm(1000)
plot(density(x),xlim=c(-8,16))

y<-rnorm(1000,mean=8)
lines(density(y),col="blue")

lines(density(rnorm(1000,sd=2)),col="red")
lines(density(rnorm(1000,mean=8,sd=2)),col="green")

lines(density(rnorm(1000,sd=4)),col="purple")
lines(density(rnorm(1000,mean=8,sd=4)),col="cyan")

#David Vose:  "Why simulate when you can calculate?"

#a number of ants (say, 3) are at theanthill;
# Ants move randomly to sites tocollect food (1 time unit);
# Poisson-simulated daily food units atsites (here 8 sites);
# If food is present, ants load it andmove to anthill (1 time unit);
# if no food is present, ants moveupwards or downwards within sites,with probability 0.5 (1 time unit);
# if all sites have been visited with nofood to be found, the ant returns tothe ant hill and is done (1 time unit).

#9 objects to keep track of.  We assign initial values, #and write them to a text file for later use:
my.text<-"time.ants<-0
          at.anthill<-rep(TRUE,3)
           done.all<-0
           done<-rep(0,3)
           food<-c(rpois(8,lambda=10),0,0)
           site<-c(10,10,10)
           carry<-c(0,0,0)
           visited.sites<-list(numeric(0),numeric(0),numeric(0))
           total.visited.sites<-rep(0,3)"
writeLines(my.text,con="~/r-code/initialize.txt")

#We write the loop code in a filele for later use:
my.text2<-"while(!done.all){
time.ants<-time.ants + 1
at.anthill<-(site==10)
done.all<-prod(done)
done<-ifelse(done,1,
ifelse(total.visited.sites==8,1,0))
for(i in 1:3){
carry[i]<-ifelse(food[site[i]]>0,1,0)
food[site[i]]<-ifelse(carry[i],food[site[i]]-1,0)
if(!at.anthill[i] & !carry[i]){
visited.sites[[i]]<-unique(c(visited.sites[[i]],site[i]))
total.visited.sites[i]<-length(visited.sites[[i]])}
}
site <-ifelse(done,10,
ifelse(carry,10,
ifelse(at.anthill,sample(1:8,3,replace=T),
site+(-1)^rbinom(3,1,0.5))))
site[site==9]<-1
site[site==0]<-8}" 
writeLines(my.text2,con="~/r-code/loop.txt")

#Start the simulation

total.time<-numeric(100)
for(k in 1:100){ 
  source("~/r-code/initialize.txt")
  source("~/r-code/loop.txt")
  total.time[k]<-time.ants }
hist(total.time); box()

