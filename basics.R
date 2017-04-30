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
