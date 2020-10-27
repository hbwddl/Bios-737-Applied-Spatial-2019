#####################################
# R code for IBS 592 Quantitative Methods
# Data break 1 - Myrtle Tree Data
# From Peter Diggle's website
#####################################

library(csrplus)

######################################
# Any line beginning with "#" is a comment
# and is ignored by R.
#####################################
# First, set the path where you saved
# the data.  This is a character string
# which is why it is in quotes.
####################################

path = "~/Classes/BIOS 737/Homework/Homework 1/"

#####################################
# Read in the data
# The files consist of rows containing two
# values each, the first is the x-coordinate
# the second the y-coordinate.
# The R function "scan" will read in 
# a text file and assign the names given
# in the "list" argument.
#
# We can either type in the entire path
# name like this:
######################################

myrtles.healthy = scan("~/Classes/BIOS 737/Homework/Homework 1/myrtles.healthy.d",list(x=0,y=0))

#####################################
# or we can use the path name above and 
# the "paste" command which links two
# character strings separated by whatever
# we set "sep" to be (in this case nothing by
# using "").
#
# IMPORTANT NOTES:  R doesn't allow spaces in
# the path name or file name.  I tend to substitute
# periods to break up the names without spaces.
# (For example "myrtles.healthy" and "spatial.class".
#####################################

myrtles.healthy2 = scan(paste(path,"myrtles.healthy.d",sep=""),list(x=0,y=0))

#####################################
# "myrtles.healthy" and "myrtles.healthy2" 
# are "data frames" containing two values each, 
# "x" and "y".
#####################################
# Let's see what we have.  Typing the 
# name "myrtles.healthy" and hitting return
# prints out the values.
#####################################

myrtles.healthy

####################################
# The "names" command just give the 
# names of the variables inside the data frame.
####################################

names(myrtles.healthy)

###################################
# To access the value within a data frame
# type the name of the frame, a dollar sign,
# then the name of the variable.
#####################################

myrtles.healthy$x

####################################
# To find out how many observations are
# in myrtles.healty$x, use the "length"
# command.
####################################

length(myrtles.healthy$x)

####################################
# To see if myrtles.healthy and myrtles.healthy2
# are the same...
####################################

myrtles.healthy$x - myrtles.healthy2$x

myrtles.healthy$y - myrtles.healthy2$y

#####################################
# Since we get all zeros, we only need to 
# use one of them.
######################################
# Let's read in the other data sets
# (all myrtles and diseased myrtles).
#####################################

myrtles.all = scan(paste(path,"myrtles.d",sep=""),list(x=0,y=0))
myrtles.diseased = scan(paste(path,"myrtles.diseased.d",sep=""),list(x=0,y=0))

#####################################
# Let's plot the data
#####################################

plot(myrtles.healthy$x, myrtles.healthy$y,xlab="X",ylab="Y",pch=19,main="Myrtle Trees")

#####################################
# The "points" command adds points to a plot,
# and the "pch" option changes the "plot character".
# Let's add the diseased myrtle locations and plot
# them as "D"s.
#####################################

points(myrtles.diseased$x,myrtles.diseased$y,pch=1)
legend("topleft",legend=c("Healthy","Diseased"),pch=c(19,1))

#####################################
# Notice that the plot is sort of square, but
# that the range of values for x is different
# from that for y.  Let's set the limits
# so that they are the same.  First, we find 
# min and max of the x and y coordinates for
# ALL myrtle locations (healthy and diseased).
#####################################

min(myrtles.all$x)
max(myrtles.all$x)
min(myrtles.all$y)
max(myrtles.all$y)

#####################################
# We can also use the "range" command
# to do this.
#####################################

range(myrtles.all$x)
range(myrtles.all$y)

#####################################
# Looks like if we set the plot boundaries
# for (0,215) for x and y, we'll catch all
# of the points.  We use the "xlim" and
# "ylim" parameters in the plot command.
# NOTE: we can continue a command onto the next
# line if we end with a comma and don't include
# a closing paranthesis until we are ready.
# ALSO NOTE: "c(0,215)" concatenates the values
# 0 and 215 into a vector.
#####################################

plot(myrtles.healthy$x,myrtles.healthy$y,xlim=c(0,215),
      ylim=c(0,215))

###################################
# Finally, to make sure R draws the plotting area
# as a square, we introduce the "par" command.
# "par" sets plotting parameters and is a very,
# very, very, very, very, very, very important
# command with lots of uses.  You have to set
# "par" before plotting, but the settings stay until
# the next "par" command resets them.
# "pty" = "plot type" and "pty=s" means "set plot type
# to square".  
####################################

par(pty="s")
plot(myrtles.healthy$x,myrtles.healthy$y,xlim=c(0,215),
      ylim=c(0,215))

####################################
# We can also use "par" to put put multiple plots
# in the same window.
# "mfrow" means "multiple figures by row".
# "mfrow=c(1,2)"  means "multiple figures, one row 
# containing two figures".  Let's try it.
####################################

par(pty="s",mfrow=c(1,2))

plot(myrtles.healthy$x,myrtles.healthy$y,xlim=c(0,215),
      ylim=c(0,215),pch=19)
title("Healthy Trees")

plot(myrtles.diseased$x,myrtles.diseased$y,xlim=c(0,215),
      ylim=c(0,215),pch=19)
title("Diseased Trees")

#######################################
# Now let's try some calculations that might
# help with the homework.
#######################################

#######################################
# To generate realizations from CSR in the range
# of values of the data we use "runif"
# command that generates uniformly distributed
# random numbers.
# NOTE: We don't want to generate on the interval 
# (0,215) since we want to limit the range of values
# to the range of the data.  We extended the region to
# get a square plot, but we want to limit simulations
# to the area with data.
#########################################
# Let's set the number of events to simulate to
# match the observed number of events.

num.events = length(myrtles.healthy$x)

CSR.x <- runif(num.events,min(myrtles.all$x),max(myrtles.all$x))
CSR.y <- runif(num.events,min(myrtles.all$y),max(myrtles.all$y))

plot(CSR.x,CSR.y)

#########################################
# Oops need to reset "mfrow" using "par"
#########################################

par(mfrow=c(1,1))
plot(CSR.x,CSR.y)

##########################################
# Better, now to make a loop for a bunch of
# CSR simulations and a "movie".
# We can make a "for" loop using the following
# syntax.  (Curly brackets mark the beginning
# and ending of the loop, and "1:100" means 
# "the set of integers from 1 to 100")  
#########################################

for (i in 1:100) {
CSR.x = runif(num.events,min(myrtles.all$x),max(myrtles.all$x))
CSR.y = runif(num.events,min(myrtles.all$y),max(myrtles.all$y))

#plot(CSR.x,CSR.y)
}

########################################
# Let's calculate a clustering statistic due
# to Pielou (1959).  This statistic requires us
# to find the distance from each event to its
# nearest neighbor.  (This also gives us a chance
# to try out some other R functions.)
# The statistic equation is given in the data
# break handout.
##########################################
# First, for each event, calculate the distance
# to all other events.
# We access individual x or y values by 
# brackets with the index, i.e., x[1] is the
# first element of x.
# We can find the distance between (x[1],y[1])
# and all other observations by...
###########################################

dist1 = sqrt( (myrtles.all$x[1] - myrtles.all$x)^2 + (myrtles.all$y[1] - myrtles.all$y)^2 )

##########################################
# This is a little tricky since 
# "myrtle.all$x[1] - myrtle.all$x" is a number (x[1])
# minus a vector (x).  In R this results in subtracting the number from 
# all elements of the vector.
##########################################

##########################################
# Now we want to find the minimum element of "dist1" that
# is NOT 0.  We can use a nifty feature of R
# namely we can put logical expressions inside
# brackets and get only the elements where that
# expression is true.  For instance, 

dist1[dist1!=0] 

# gives the elements of dist1 that are not equal to zero.  So

min(dist1[dist1!=0])

# gives the nearest neighbor distance!  Now we just need it for
# all values.  Let's use a loop to get this.  Set a vector
# of zeros with length equal to the number of locations in
# myrtles.all (length(myrtles.all$x)).

mindist.all <- 0*(1:length(myrtles.all$x))

for (i in 1:length(myrtles.all$x)) {
  dist = sqrt( (myrtles.all$x[i] - myrtles.all$x)^2 + (myrtles.all$y[i] - myrtles.all$y)^2 )
  mindist.all[i] = min(dist[dist!=0])
}

###########################################
# Now we need to calculate Pielou's statistic.

n.all = length(myrtles.all$x)
area.all = (max(myrtles.all$x) - min(myrtles.all$x)) * (max(myrtles.all$y)-min(myrtles.all$y))
lambda.all = n.all/area.all
pielou.all = pi*lambda.all*sum(mindist.all^2)/n.all

bd.x <- c(min(myrtles.all$x),min(myrtles.all$x),max(myrtles.all$x),max(myrtles.all$x))
bd.y <- c(min(myrtles.all$y),max(myrtles.all$y),min(myrtles.all$y),max(myrtles.all$y))
pielou.fun.all <- pielou(myrtles.all$x,myrtles.all$y,n.all,bd.x,bd.y,lambda.all)
###########################################
# To get a Monte Carlo p-value we need to 
# do these same calculations to data generated under
# CSR.  First define the number of simulations.
#############################################

num.sim = 99

# then define a vector to hold the values of Pielou's 
# statistic for each simulated data set.

pielou.all.sim = 0*(1:num.sim)
pielou.all.sim.fun = rep(0,num.sim)

# now set up the simulation loop

for (sim in 1:num.sim) {

  # define CSR data
  CSR.x = runif(num.events,min(myrtles.all$x),max(myrtles.all$x))
  CSR.y = runif(num.events,min(myrtles.all$y),max(myrtles.all$y))

  #define vector of min NN distances
  mindist.sim = 0*(1:length(myrtles.all$x))

  # find min distances (a loop within the simulation loop)

  for (i in 1:length(CSR.x)) {
     dist = sqrt( (CSR.x[i] - CSR.x)^2 + (CSR.y[i] - CSR.y)^2 )
     mindist.sim[i] = min(dist[dist!=0])
  }

  # calculate pielou.all.sim[sim] (Pielou's statistic for the "sim-th" CSR data set).
  pielou.all.sim[sim] = pi*lambda.all*sum(mindist.sim^2)/n.all

  pielou.all.sim.fun[sim] <- pielou(CSR.x,CSR.y,n.all,bd.x,bd.y,lambda.all)
}


# make a histogram of the CSR values

par(pty="m")  # makes plot type "maximum" (rectangular in window).

hist(pielou.all.sim,xlim=c(0.5,max(pielou.all.sim)))

# add a vertical line showing Pielou's statistic from the observed data.
# ("segments(x1,y1,x2,y2)" draws a line segments between (x1,y1) and (x2,y2).

segments(pielou.all,0,pielou.all,100) 

# calculate Monte Carlo p-value (the number of statistics from simulated
# data that exceed the statistic from the observed data divided by the
# number of simulations + 1.

p.val = length(pielou.all.sim[pielou.all.sim>pielou.all])/(num.sim+1)


###################################################################
# now do this for the healthy and diseased subsets

mindist.healthy = 0*(1:length(myrtles.healthy$x))

for (i in 1:length(myrtles.healthy$x)) {
  dist = sqrt( (myrtles.healthy$x[i] - myrtles.healthy$x)^2 + (myrtles.healthy$y[i] - myrtles.healthy$y)^2 )
  mindist.healthy[i] = min(dist[dist!=0])
}

n.healthy = length(myrtles.healthy$x)
# NOTE: We still use area.all to cover the entire study area.
lambda.healthy = n.healthy/area.all
pielou.healthy = pi*lambda.healthy*sum(mindist.healthy^2)/n.healthy
print(paste("Peilou's statistic, healthy myrtles:",pielou.healthy))


mindist.diseased = 0*(1:length(myrtles.diseased$x))

for (i in 1:length(myrtles.diseased$x)) {
  dist = sqrt( (myrtles.diseased$x[i] - myrtles.diseased$x)^2 + (myrtles.diseased$y[i] - myrtles.diseased$y)^2 )
  mindist.diseased[i] = min(dist[dist!=0])
}


n.diseased = length(myrtles.diseased$x)
# NOTE: We still use area.all to cover the entire study area.
lambda.diseased = n.diseased/area.all
pielou.diseased = pi*lambda.diseased*sum(mindist.diseased^2)/n.diseased
print(paste("Peilou's statistic, diseased myrtles:",pielou.diseased))


