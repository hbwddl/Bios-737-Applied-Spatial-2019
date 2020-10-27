#Set working Directory

setwd("~/Classes/BIOS 737/Homework/Homework 1")

#Set up Libraries

library(csrplus)

#Read in the data

old_anasazi <- read.csv("anasazi.old.csv",header=T)
new_anasazi <- read.csv("anasazi.new.csv",header=T)
all_anasazi <- rbind(old_anasazi,new_anasazi)

#Plot the data

range(all_anasazi$long)
range(all_anasazi$lat)

plot(old_anasazi$long,old_anasazi$lat,pch=3,xlab="Longitude",ylab="Latitude",main="Anasazi Settlements",xlim=c(0,125),ylim=c(0,190))
points(new_anasazi$long,new_anasazi$lat,pch=20)
legend("topright",legend=c("Old","New"),pch=c(3,20))

plot(old_anasazi$long,old_anasazi$lat,pch=3,xlab="Longitude",ylab="Latitude",main="Old Anasazi Settlements",xlim=c(0,125),ylim=c(0,190))
plot(new_anasazi$long,new_anasazi$lat,pch=20,xlab="Longitude",ylab="Latitude",main="New Anasazi Settlements",xlim=c(0,125),ylim=c(0,190))

#Generating CSR data
n.all <- nrow(all_anasazi)
n.old <- nrow(old_anasazi)
n.new <- nrow(new_anasazi)

n.sim <- 99

#Calculating Pielou's Statistic with function
bd.x <- c(min(all_anasazi$long),min(all_anasazi$long),max(all_anasazi$long),max(all_anasazi$long),min(all_anasazi$long))
bd.y <- c(min(all_anasazi$lat),max(all_anasazi$lat),max(all_anasazi$lat),min(all_anasazi$lat),min(all_anasazi$lat))

bd.area <- (max(all_anasazi$long) - min(all_anasazi$long))*(max(all_anasazi$lat)-min(all_anasazi$lat))

lambda.all <- n.all/bd.area
lambda.old <- n.old/bd.area
lambda.new <- n.new/bd.area

csr.pielou.all.fun <- rep(0,n.sim)
csr.pielou.all.hand <- rep(0,n.sim)

for(i in 1:n.sim){
  csr.x <- runif(n.all,min(all_anasazi$long),max(all_anasazi$long))
  csr.y <- runif(n.all,min(all_anasazi$lat),max(all_anasazi$lat))
  csr.pielou.all.fun[i] <- pielou(csr.x,csr.y,n.all,bd.x,bd.y,lambda.all)
  
  #Calculating Pielou's statistic by hand for the csr data
  mindist.csr <- rep(0,n.all)
  for(j in 1:n.all){
    dist = sqrt( (csr.x[j] - csr.x)^2 + (csr.y[j] - csr.y)^2 )
    mindist.csr[j] = min(dist[dist!=0])
  }
  csr.pielou.all.hand[i] <- pi*lambda.all*sum(mindist.csr^2)/n.all
}

#Simulating CSR data and calculating pielou's statistic with the same number of observations as the old settlements
csr.pielou.old.fun <- rep(0,n.sim)
csr.pielou.old.hand <- rep(0,n.sim)

for(i in 1:n.sim){
  csr.x <- runif(n.old,min(all_anasazi$long),max(all_anasazi$long))
  csr.y <- runif(n.old,min(all_anasazi$lat),max(all_anasazi$lat))
  csr.pielou.old.fun[i] <- pielou(csr.x,csr.y,n.old,bd.x,bd.y,lambda.old)
  
  #Calculating Pielou's statistic by hand for the csr data
  mindist.csr <- rep(0,n.old)
  for(j in 1:n.old){
    dist = sqrt( (csr.x[j] - csr.x)^2 + (csr.y[j] - csr.y)^2 )
    mindist.csr[j] = min(dist[dist!=0])
  }
  csr.pielou.old.hand[i] <- pi*lambda.old*sum(mindist.csr^2)/n.old
}

#Simulating CSR Data and calculating pielou's statistic with the same number of observations as the new settlements
csr.pielou.new.fun <- rep(0,n.sim)
csr.pielou.new.hand <- rep(0,n.sim)

for(i in 1:n.sim){
  csr.x <- runif(n.new,min(all_anasazi$long),max(all_anasazi$long))
  csr.y <- runif(n.new,min(all_anasazi$lat),max(all_anasazi$lat))
  csr.pielou.new.fun[i] <- pielou(csr.x,csr.y,n.new,bd.x,bd.y,lambda.new)
  
  #Calculating Pielou's statistic by hand for the csr data
  mindist.csr <- rep(0,n.new)
  for(j in 1:n.new){
    dist = sqrt( (csr.x[j] - csr.x)^2 + (csr.y[j] - csr.y)^2 )
    mindist.csr[j] = min(dist[dist!=0])
  }
  csr.pielou.new.hand[i] <- pi*lambda.new*sum(mindist.csr^2)/n.new
}

#Calculating Pielou's Statistic for all settlements
all.pielou.fun <- pielou(all_anasazi$long,all_anasazi$lat,n.all,bd.x,bd.y,lambda.all)

mindist.all <- rep(0,n.all)

for (i in 1:n.all) {
  dist = sqrt( (all_anasazi$long[i] - all_anasazi$long)^2 + (all_anasazi$lat[i] - all_anasazi$lat)^2 )
  mindist.all[i] = min(dist[dist!=0])
}

all.pielou <- pi*lambda.all*sum(mindist.all^2)/n.all

#Calculating Pielou's statistic for old settlements
old.pielou.fun <- pielou(old_anasazi$long,old_anasazi$lat,n.old,bd.x,bd.y,lambda.old)

mindist.old <- rep(0,n.old)
for (i in 1:n.old){
  dist = sqrt( (old_anasazi$long[i] - old_anasazi$long)^2 + (old_anasazi$lat[i] - old_anasazi$lat)^2 )
  mindist.old[i] = min(dist[dist!=0])
}

old.pielou <- pi*lambda.old*sum(mindist.old^2)/n.old

#Calculating Pielou's statistic for new settlements
new.pielou.fun <- pielou(new_anasazi$long,new_anasazi$lat,n.new,bd.x,bd.y,lambda.new)

mindist.new <- rep(0,n.new)

for (i in 1:n.new) {
  dist = sqrt( (new_anasazi$long[i] - new_anasazi$long)^2 + (new_anasazi$lat[i] - new_anasazi$lat)^2 )
  mindist.new[i] = min(dist[dist!=0])
}

new.pielou <- pi*lambda.new*sum(mindist.new^2)/n.new

all.pval <- sum(csr.pielou.all.hand > all.pielou)/(n.sim+1)
old.pval <- sum(csr.pielou.old.hand > old.pielou)/(n.sim+1)
new.pval <- sum(csr.pielou.new.hand > new.pielou)/(n.sim+1)

##Making histograms!
hist(csr.pielou.all.hand,xlim=c(0.7,max(csr.pielou.all.hand)),main="Histogram of Pielou's Statistic, all settlements")
abline(v=all.pielou)

hist(csr.pielou.old.hand,xlim=c(0.8,max(csr.pielou.old.hand)),main="Histogram of Pielou's Statistic, old Settlements")
abline(v=old.pielou)

hist(csr.pielou.new.hand,xlim=c(0.7,max(csr.pielou.new.hand)),main="Histogram of Pielou's Statistic, new settlements")
abline(v=new.pielou)
