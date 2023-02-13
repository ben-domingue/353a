sim<-function(th,delta,rho,resp=FALSE) { #simulate data from andrich's irf
    ##eqn 21.17 in handbook, u=1
    n<-cosh(rho)
    gamma<-cosh(rho)+cosh(th-delta)
    p<-cosh(rho)/gamma 
    if (resp) rbinom(length(p),1,p) else p
}

th<-seq(-4,4,length.out=1000)
y1<-sim(th,delta=0,rho=1)

plot(th,y1,type='l',ylim=0:1) #let's make sure this looks ok by first just checking out item response functions (pr(x=1) are solid and pr(x=0) are dashed)
lines(th,1-y1,lty=2)

lines(th,sim(th,delta=1,rho=1),col='red')
lines(th,1-sim(th,delta=1,rho=1),col='red',lty=2)

lines(th,sim(th,delta=0,rho=2),col='blue')
lines(th,sim(th,delta=0,rho=.2),col='green')


th<-rnorm(1000) #generate a grabbag of thetas
y<-sim(th,delta=.25,rho=.75,resp=TRUE)
like<-function(pars,th,resp) { #a likelihood function we'll use for ML
    delta<-pars[1]
    rho<-pars[2]
    gamma<-cosh(rho)+cosh(th-delta)
    term1<-(cosh(rho))^resp
    term2<-(cosh(th-delta))^(1-resp)
    -sum(log(term1*term2/gamma))
}

est<-optim(c(0,1),like,th=th,resp=y) #doing ML
est$par ##these should match the values we fed to sim()

##let's do this at scale
deltaL<-rnorm(50) #50 items
rhoL<-runif(50,min=.25,max=1.25)
est<-list()
for (i in 1:length(deltaL)) {
    y<-sim(th,delta=deltaL[i],rho=rhoL[i],resp=TRUE)
    est[[i]]<-optim(c(0,1),like,th=th,resp=y)$par
}
est<-do.call("rbind",est)

par(mfrow=c(1,2))
plot(deltaL,est[,1]) #looks good!
plot(rhoL,est[,2]) #what is going on here?

plot(th,cosh(th),cex=2,pch=19)
points(th,cosh(-th),col='red',cex=.5)
