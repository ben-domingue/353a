sim<-function(th,delta,eps) { 
    ##see equations 21.2-21.4 in andrich chapter
    psi<-list()
    psi[[1]]<-rep(1,length(th))
    k<-th-delta+eps
    psi[[2]]<-exp(k)
    k<-2*(th-delta)
    psi[[3]]<-exp(k)
    psi<-do.call("cbind",psi)
    den<-rowSums(psi)
    p<-psi/den
    resp<-numeric()
    for (i in 1:length(th)) resp[i]<-which(rmultinom(1,1,p[i,])[,1]>0)-1
    resp
}

like.pcm<-function(pars,th,resp) {
    delta<-pars[1]
    eps<-pars[2]
    psi<-list()
    psi[[1]]<-rep(1,length(th))
    k<-th-delta+eps
    psi[[2]]<-exp(k)
    k<-2*th-2*delta
    psi[[3]]<-exp(k)
    psi<-do.call("cbind",psi)
    den<-rowSums(psi)
    p<-psi/den
    x0<-as.numeric(resp==0)
    x1<-as.numeric(resp==1)
    x2<-as.numeric(resp==2)
    p<-p[,1]*x0+p[,2]*x1+p[,3]*x2
    -sum(log(p))
}
like.andrich<-function(pars,th,resp) {
    delta<-pars[1]
    rho<-pars[2]
    gamma<-cosh(rho)+cosh(th-delta)
    term1<-(cosh(rho))^resp
    term2<-(cosh(th-delta))^(1-resp)
    -sum(log((term1*term2)/gamma))
}

rmse<-function(x,y) sqrt(mean((x-y)^2))
out<-list()
for (N in c(50,100,250,500,1000,10000)) {
    th<-rnorm(N)
    deltaL<-runif(25,min=-1,max=1)
    est.pcm<-est.andrich<-list()
    for (i in 1:length(deltaL)) {
        x<-sim(th,delta=deltaL[i],eps=1)
        x2<-ifelse(x==2,0,x)
        est.pcm[[i]]<-optim(c(0,1),like.pcm,th=th,resp=x)$par
        est.andrich[[i]]<-optim(c(0,.2),like.andrich,th=th,resp=x2)$par
    }
    est.pcm<-do.call("rbind",est.pcm)
    est.andrich<-do.call("rbind",est.andrich)
    x1<-rmse(deltaL,est.pcm[,1])
    x2<-rmse(deltaL,est.andrich[,1])
    out[[as.character(N)]]<-c(N,x1,x2)
}
z<-do.call("rbind",out)

plot(log10(z[,1]),z[,3],pch=19,col='red',ylim=c(0,max(z[,3])),xlab='log10(N)',ylab='rmse for location')
points(log10(z[,1]),z[,2],pch=19)




