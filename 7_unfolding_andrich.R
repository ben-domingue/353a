sim<-function(th,delta,rho,resp=FALSE) {
    ##eqn 21.17 in handbook, u=1
    n<-cosh(rho)
    gamma<-cosh(rho)+cosh(th-delta)
    p<-cosh(rho)/gamma 
    if (resp) rbinom(length(p),1,p) else p
}

th<-seq(-4,4,length.out=1000)
y1<-sim(th,delta=0,rho=1)
plot(th,y1,type='l',ylim=0:1)

lines(th,sim(th,delta=1,rho=1),col='red')
lines(th,sim(th,delta=0,rho=2),col='blue')
lines(th,sim(th,delta=0,rho=.2),col='green')


th<-rnorm(1000)
y<-sim(th,delta=.25,rho=.75,resp=TRUE)
like<-function(pars,th,resp) {
    delta<-pars[1]
    rho<-pars[2]
    gamma<-cosh(rho)+cosh(th-delta)
    term1<-(cosh(rho))^resp
    term2<-(cosh(th-delta))^(1-resp)
    -sum(log(term1*term2/gamma))
}

optim(c(0,1),like,th=th,resp=y)

deltaL<-rnorm(50)
rhoL<-runif(50,min=.25,max=1.25)
est<-list()
for (i in 1:length(deltaL)) {
    y<-sim(th,delta=deltaL[i],rho=rhoL[i],resp=TRUE)
    est[[i]]<-optim(c(0,1),like,th=th,resp=y)$par
}
est<-do.call("rbind",est)

par(mfrow=c(1,2))
plot(deltaL,est[,1])
plot(rhoL,est[,2])

plot(th,cosh(th),cex=2,pch=19)
points(th,cosh(-th),col='red',cex=.5)
