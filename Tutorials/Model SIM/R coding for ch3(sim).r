rm(list=ls(all=T))
source("sfc3.0.R")
sim=sfc.model("ch3.txt",modelName="sim")
sim<-sfc.check(sim,fill=T) # it did not ask me to enter the initial values and timeline because it has already been specified in ch3.txt
sim
#simulates the model, by default the Iterations are 1000, one can reduce the Iterations to 100 if the computer is struggling!
datasim<-simulate(sim, maxIter=100)

#this replicates table 3.4 page 69 of GL
round(t(datasim$baseline[c(1,2,3,66),c("g_d","y","t_d","yd","c_d","h_s","h_h")]),digits=1)

#needed for the scenario building
init = datasim$baseline[66,]
sim.shock1<-sfc.addScenario(model=sim,vars=list(c("g_d")),values=list(c(25)),inits=1960,ends=2010,starts=init)
shock1<-simulate(sim.shock1, maxIter=100)


# I can only bring variation in exogenous variables or parameters (not in endogenous as they are determined inside the system) to see how does that affect the whole system of equation.
#This replicates figure 3.1 page72
plot(sim$time,shock1$scenario[,"y"],type="l",xlab="",ylab="",lty=2, lwd=2, col="blue")
lines(sim$time,(sim$time>1959)*(shock1$scenario["2010","y"]-shock1$scenario["1958","y"])+shock1$scenario["1958","y"], lwd=2)
legend(x=1980,y=110,legend=c("GDP","Steady State GDP"),lty=c(2,1),bty="n", lwd=c(2,2), col=c("blue", "black"))
grid()

# lets look at the steady state solution of the system
#stationary state is the one in which the level of variables is constant,, they dont grow over time.
#steady state is the one in which a constant relationship is achieved through iterations of the system of equations.
# so the coding actually run iterations for every period from 1945 to 2010 and the economy achieves a study state at some period which can be seen in the plot.

plot(sim$time,datasim$baseline[,"yd"],type="l",col="blue",lwd=2,xlab="",ylab="")
lines(sim$time,datasim$baseline[,"c_d"], lwd=2,xlab="",ylab="",lty=2, col="red" )                              
lines(sim$time,vector(length=length(sim$time))+datasim$baseline["2010","c_d"],lwd=2)
legend(x=1970,y=50,legend=c("Disposable Income","Consumption","Steady State"),lty=c(1,2,1),col=c("blue","red", "black"),lwd=c(2,2,2))

#This replicates figure 3.3 page 75
plot(sim$time,datasim$baseline[,"dh_h"],type="l",lwd=2,xlab="",ylab="")                                                                 
par(new=T)
plot(sim$time,(datasim$baseline[,"h_h"]),lty=2, lwd=2,type="l",axes=F,ylab="",xlab="")
axis(4,pretty(c(0, 1.1*max(datasim$baseline[,"h_h"],na.rm=T))))
legend(x=1970,y=50,legend=c("Wealth Level","Household Savings"),lty=c(2,1),lwd=2,bty="n")
grid()


#This replicates figure 3.4 page 76
plot(sim$time,shock1$scenario[,"yd"],type="l",lwd=2,xlab="",ylab="",lty=2)
lines(sim$time,shock1$scenario[,"c_d"],col="blue",lwd=2)
lines(sim$time,shock1$scenario[,"h_h"],col="red",lwd=2)
legend(x=1970,y=90,legend=c("Disposable Income","Consumption","Wealth"),col=c("black","blue","red"),lwd=c(2,2,2))
grid()