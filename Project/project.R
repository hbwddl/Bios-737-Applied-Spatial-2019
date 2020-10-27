##Bios 737 Project
#Set up libraries
library(splancs)
library(KernSmooth)

#Read in the data
setwd("~/Classes/BIOS 737/Project")
old.anasazi <- read.csv("anasazi.old.csv",header=T,stringsAsFactors = F)
new.anasazi <- read.csv("anasazi.new.csv",header=T,stringsAsFactors = F)
all.anasazi <- rbind(old.anasazi,new.anasazi)

par(pty="s",mfrow=c(1,1))
plot(all.anasazi,xlim=c(-10,130),ylim=c(-10,200),pch=20,main="All Settlements",xlab="Longitude",ylab="Latitude")

# mypoly <- getpoly()
# 
# write.csv(mypoly,file="projpoly.csv")

mypoly <- read.csv("projpoly.csv",header=T,stringsAsFactors = F)
names(mypoly) <- c("x","y")
polygon(mypoly)

old.bw <- 15
new.bw <- 15

expand.amt <- 1.5

xmin <- min(old.anasazi$long,new.anasazi$long)-expand.amt*old.bw
xmax <- max(old.anasazi$long,new.anasazi$long)+expand.amt*old.bw
ymin <- min(old.anasazi$lat,new.anasazi$lat)-expand.amt*old.bw
ymax <- max(old.anasazi$lat,new.anasazi$lat)+expand.amt*old.bw

old.bd <- list(c(xmin,xmax),c(ymin,ymax))
new.bd <- list(c(xmin,xmax),c(ymin,ymax))

grid.n <- 51

par(pty="s",mfrow=c(1,2))
old.dens <- bkde2D(old.anasazi,old.bw,gridsize=c(grid.n,grid.n),old.bd)
new.dens <- bkde2D(new.anasazi,new.bw,gridsize = c(grid.n,grid.n),new.bd)

grid.x <- seq(from=xmin,to=xmax,length.out = grid.n)
grid.y <- seq(from=ymin,to=ymax,length.out = grid.n)

grid.all <- expand.grid(grid.x,grid.y)
names(grid.all) <- c("x","y")

inside <- inout(grid.all,mypoly)

old.dens$fhat[!inside] <- NA
new.dens$fhat[!inside] <- NA

#Plot the density and points
par(mfrow=c(1,2))
contour(old.dens$x1,old.dens$x2,old.dens$fhat,main="Old Settlements",drawlabels = F,xlab="Longitude",ylab="Latitude")
polygon(mypoly)
points(old.anasazi,pch=19)

persp(old.dens$x1,old.dens$x2,old.dens$fhat,theta=-25,phi=45,xlab="Longitude",ylab="Latitude",main="Density of Old Settlements",zlab="")

contour(new.dens$x1,new.dens$x2,new.dens$fhat,main="New Settlements",drawlabels = F)
polygon(mypoly)
points(new.anasazi,pch=19)

persp(new.dens$x1,new.dens$x2,new.dens$fhat,theta=-25,phi=45,xlab="Longitude",ylab="Latitude",main="Density of New Settlements",zlab="")


#Define relative risk surface
log.risk <- log(new.dens$fhat/old.dens$fhat)

log.risk[!is.finite(log.risk)] <- NA

par(mfrow=c(1,1))
contour(old.dens$x1,old.dens$x2,log.risk,
        levels=c(-1,-.5,0,.5,1,1.5,2,2.5,3,3.5),
        lwd=c(1,1,2,3,3,3,3,3,3,3),
        main="Log Relative Risk Surface of Old Vs. New Settlements"
        )
polygon(mypoly)

#persp(old.dens$x1,old.dens$x2,log.risk,theta=-25,phi=45,xlab="Longitude",ylab="Latitude",main="Log Relative Risk Surface",zlab="")

#Calculate test statistic for data
data.test.stat <- sum( (log.risk[is.finite(log.risk) & !is.na(log.risk)]/(diff(old.dens$x1)[1]*diff(old.dens$x2)[1]))^2 )

#Simulations to do this

n.sim <- 999
n.old <- nrow(old.anasazi)
n.new <- nrow(new.anasazi)
n.all <- nrow(all.anasazi)

#Compare Risk Surface to Random Labeling risk surface
ind <- 1:n.all

#Create list to store test statistics
sim.test.stat <- rep(NA,n.sim)

#Create list to store risk surfaces
sim.rr <- list()

for(i in 1:n.sim){
  #Generate Random labels
  old.lab <- sample(ind,size=n.old)
  old.x <- all.anasazi$long[old.lab]
  old.y <- all.anasazi$lat[old.lab]
  old.pts <- as.points(old.x,old.y)
  
  new.lab <- ind[-old.lab]
  new.x <- all.anasazi$long[new.lab]
  new.y <- all.anasazi$lat[new.lab]
  new.pts <- as.points(new.x,new.y)
  
  old.rand <- bkde2D(old.pts,old.bw,gridsize=c(grid.n,grid.n),old.bd)
  new.rand <- bkde2D(new.pts,new.bw,gridsize=c(grid.n,grid.n),new.bd)
  
  old.rand$fhat[!inside] <- NA
  new.rand$fhat[!inside] <- NA
  
  ratio.rand <- log(new.rand$fhat/old.rand$fhat)
  
  sim.rr[[i]] <- ratio.rand
  
  sim.test.stat[i] <- sum( (ratio.rand[is.finite(ratio.rand) & !is.na(ratio.rand)]/(diff(old.rand$x1)[1]*diff(old.rand$x2)[1]))^2 )
}

#I am getting runaway test stat values! Ugh!
hist(sort(sim.test.stat),xlim=c(0,5),main="Histogram of integrated deviation squared test statistic",xlab="Test Statistic")
abline(v=data.test.stat,lty=2)

#Find envelopes for the simulated Risk Ratios
#Find 97.5 quantile
q97.5 <- matrix(data=NA,nrow=grid.n,ncol=grid.n)
this.point.rr <- rep(NA,n.sim)
for(i in 1:grid.n){
  for(j in 1:grid.n){
    for(k in 1:n.sim){
      this.point.rr[k] <- sim.rr[[k]][i,j]
    }
  q97.5[i,j] <- quantile(this.point.rr,probs=.975,na.rm=T)
  }
}

#find 2.5 quantile
q2.5 <- matrix(data=NA,nrow=grid.n,ncol=grid.n)
this.point.rr <- rep(NA,n.sim)
for(i in 1:grid.n){
  for(j in 1:grid.n){
    for(k in 1:n.sim){
      this.point.rr[k] <- sim.rr[[k]][i,j]
    }
    q2.5[i,j] <- quantile(this.point.rr,probs=.025,na.rm=T)
  }
}

#Plot where the data's log relative risk surface is outside the envelopes
contour(old.dens$x1,old.dens$x2,log.risk,
                levels=c(-1,-.5,0,.5,1,1.5,2,2.5,3,3.5),
                lwd=c(1,1,2,3,3,3,3,3,3,3),
                main="Gaussian Kernel, Bandwidth = 15"
)
polygon(mypoly)
above <- which(log.risk>q97.5,arr.ind=T)
points(old.dens$x1[above[,1]],old.dens$x2[above[,2]],pch="+")

below <- which(log.risk<q2.5,arr.ind=T)
points(old.dens$x1[below[,1]],old.dens$x2[below[,2]],pch="-")


#Find the K function of the data
#distances at which to calculate k
dists <- seq(from=1,to=100,by=1)

#Find the K function of the old settlements
names(old.anasazi) <- c("x","y")
p.old.anasazi <- as.points(old.anasazi)
khat.old <- khat(p.old.anasazi,as.points(mypoly),dists)

#Find the K function of the new settlements
names(new.anasazi) <- c("x","y")
p.new.anasazi <- as.points(new.anasazi)
khat.new <- khat(p.new.anasazi,as.points(mypoly),dists)

#Find the K function of all the settlements
names(all.anasazi) <- c("x","y")
p.all.anasazi <- as.points(all.anasazi)
khat.all <- khat(p.all.anasazi,as.points(mypoly),dists)

#Simulate CSR Data, find K functions
khat.sim.old <- matrix(data=NA,nrow=n.sim,ncol=length(dists))
khat.sim.new <- matrix(data=NA,nrow=n.sim,ncol=length(dists))
khat.sim.all <- matrix(data=NA,nrow=n.sim,ncol=length(dists))

for(i in 1:n.sim){
  old.csr <- csr(as.points(mypoly),n.old)
  khat.sim.old[i,] <- khat(old.csr,as.points(mypoly),dists)
  
  new.csr <- csr(as.points(mypoly),n.new)
  khat.sim.new[i,] <- khat(new.csr,as.points(mypoly),dists)
  
  all.csr <- csr(as.points(mypoly),n.all)
  khat.sim.all[i,] <- khat(all.csr,as.points(mypoly),dists)
  
  if(i %% 50 == 0){
    print(i)
  }
}

khat.old.q2.5 <- apply(khat.sim.old,2,quantile,probs=.025,names=F)
khat.old.q97.5 <- apply(khat.sim.old,2,quantile,probs=.975,names=F)

khat.new.q2.5 <- apply(khat.sim.new,2,quantile,probs=.025,names=F)
khat.new.q97.5 <- apply(khat.sim.new,2,quantile,probs=.975,names=F)

khat.all.q2.5 <- apply(khat.sim.all,2,quantile,probs=.025,names=F)
khat.all.q97.5 <- apply(khat.sim.all,2,quantile,probs=.975,names=F)


##Calculate L functions for everything
lhat.old <- sqrt(khat.old/pi)
lhat.new <- sqrt(khat.new/pi)
lhat.all <- sqrt(khat.all/pi)

lhat.old.q2.5 <- sqrt(khat.old.q2.5/pi)
lhat.old.q97.5 <- sqrt(khat.old.q97.5/pi)

lhat.new.q2.5 <- sqrt(khat.new.q2.5/pi)
lhat.new.q97.5 <- sqrt(khat.new.q97.5/pi)

lhat.all.q2.5 <- sqrt(khat.all.q2.5/pi)
lhat.all.q97.5 <- sqrt(khat.all.q97.5/pi)

#Plot L functions for everything
par(mfrow=c(3,1),pty="m")

plot(dists,lhat.old-dists,ylim=c(-3,5),main="L Plot for Old Settlements",xlab="Distance",ylab="Lhat - Distance",type='n')
lines(dists,lhat.old-dists,lty=1)
lines(dists,lhat.old.q2.5-dists,lty=2)
lines(dists,lhat.old.q97.5-dists,lty=2)

plot(dists,lhat.new-dists,ylim=c(-3,5),main="L Plot for New Settlements",xlab="Distance",ylab="Lhat - Distance",type='n')
lines(dists,lhat.new-dists,lty=1)
lines(dists,lhat.new.q2.5-dists,lty=2)
lines(dists,lhat.new.q97.5-dists,lty=2)

plot(dists,lhat.all-dists,ylim=c(-3,5),main="L Plot for All Settlements",xlab="Distance",ylab="Lhat - Distance",type='n')
lines(dists,lhat.new-dists,lty=1)
lines(dists,lhat.all.q2.5-dists,lty=2)
lines(dists,lhat.all.q97.5-dists,lty=2)

