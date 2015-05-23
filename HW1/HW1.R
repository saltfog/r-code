#read in CSV file

tab = read.csv("msleep_ggplot2.csv")
class(tab)

head(tab)
dim(tab)
colnames(tab)
tab$name
tab$sleep_total
#Vectors can be combined using the c function. 
#For example, we can add a final number, 1000, to the sleep totals:
c(tab$sleep_total, 1000)

#ploting
plot(tab$brainwt, tab$sleep_total)
plot(tab$brainwt, tab$sleep_total, log="x")

#Summary
summary(tab$sleep_total)

#subsetting data
tab[ c(1,2), ]
tab[ tab$sleep_total > 18, ]
plot(tab$sleep_total > 18,)
tab$sleep_total[ c(1,2) ]
tab$sleep_total[ c(1,2) > 18]
mean(tab$sleep_total[ c(1,2)] > 18, rm.na = true)
tab$sleep_total[ c(1,2) ] >18
mean(19.7,19.4,19.9,18.1)

#which
which(tab$sleep_total > 18)
tab$sleep_total[ which(tab$sleep_total > 18)[1] & which(tab$sleep_rem <3)[1]]
tab[c(1)]

#sorting, order, rank, merge
sort(tab$sleep_total)
order(tab$sleep_total)
tab[order(tab$sleep_total),1:6] #least amount of sleep

rank(c(1,2,2,3))

tab[rank(tab$sleep_total),1]

idx = match(c("Cow","Owl monkey","Cheetah"), tab$name)
tab[idx]

#create vector
vec = c()
vec = c(tab$order)
fac = factor(vec)
fac
fac == "Rodentia"
levels(fac)

#count instances if column
table(tab$order)

fac2 = factor(vec1, levels=c(tab$order))
fac2
levels(fac2)

#split
s = split(tab$sleep_total, tab$order)
s
tapply(tab$sleep_total, tab$order, mean)
s[["Primates"]]
sapply(s,sd)
