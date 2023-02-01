##generate via sequential model
simseq<-function(th,b,a) {
    K<-length(b)+1
    invlogit<-function(z) 1/(1+exp(-z))
    p1<-invlogit(a*(th-b[1])) #probability you took step 1
    p2<-invlogit(a*(th-b[2])) #probability you took step 2 if you took step 1
    p<-cbind(1-p1,p1*(1-p2),p1*p2)
    colnames(p)<-paste("truep",0:(ncol(p)-1),sep='')
    resp<-numeric()
    for (i in 1:np) resp[i]<-which(rmultinom(1,1,p[i,])[,1]>0)-1
    resp
}
##estimate sequential response model via known theta using ML
estseq<-function(pars, #we're going to optimize pars. it will have structure c(a,b0,b1) where a is the discrimination and b parameters are steps in the sequential model
                 resp,th) {
    invlogit<-function(z) 1/(1+exp(-z))
    p1<-invlogit(pars[1]*(th-pars[2])) #probability you took step 1
    p2<-invlogit(pars[1]*(th-pars[3])) #probability you took step 2 if you took step 1
    p<-cbind(1-p1,p1*(1-p2),p1*p2)
    x0<-ifelse(resp==0,1,0)
    x1<-ifelse(resp==1,1,0)
    x2<-ifelse(resp==2,1,0)
    z<-p[,1]*x0+p[,2]*x1+p[,3]*x2
    -1*sum(log(z))
}

a<-1
np<-5000
##
b0vals<-runif(50,-1,0)
b1vals<-b0vals+.5+runif(50,0,1)
L<-list()
for (i in 1:length(b0vals)) {
    b<-c(b0vals[i],b1vals[i])
    redo<-TRUE
    while (redo) {
        print(i)
        th<-rnorm(np)
        resp<-simseq(th,b,a)
        est<-optim(c(1,0,2),estseq,resp=resp,th=th)$par
        if (est[1]>.1) redo<-FALSE #convergence problems associated with a
    }
    L[[i]]<-c(a=a,b0=b[1],b1=b[2],a.est=est[1],b0.est=est[2],b1.est=est[3])
}
df<-data.frame(do.call("rbind",L))

par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,1,1))
hist(df$a.est)
plot(df$b0,df$b0.est); abline(0,1)
plot(df$b1,df$b1.est); abline(0,1)

##now let's get devious. what if we simulate via pcm and estimate using sequential model?
##generate via pcm
simpcm<-function(th,b,a) {
    K<-length(b)+1
    psi<-list()
    psi[[1]]<-rep(1,length(th))
    for (k in 1:(K-1)) {
        kern<-k*th-sum(b[1:k])
        psi[[k+1]]<-exp(a*kern)
    }
    psi<-do.call("cbind",psi)
    den<-rowSums(psi)
    p<-psi/den
    colnames(p)<-paste("truep",0:(ncol(p)-1),sep='')
    resp<-numeric()
    for (i in 1:np) resp[i]<-which(rmultinom(1,1,p[i,])[,1]>0)-1
    resp
}

a<-1
np<-5000
##
b0vals<-runif(50,-1,0)
b1vals<-b0vals+.5+runif(50,0,1)
L<-list()
for (i in 1:length(b0vals)) {
    b<-c(b0vals[i],b1vals[i])
    redo<-TRUE
    while (redo) {
        print(i)
        th<-rnorm(np)
        resp<-simpcm(th,b,a)
        est<-optim(c(1,0,2),estseq,resp=resp,th=th)$par
        if (est[1]>.1) redo<-FALSE #convergence problems associated with a
    }
    L[[i]]<-c(a=a,b0=b[1],b1=b[2],a.est=est[1],b0.est=est[2],b1.est=est[3])
}
df<-data.frame(do.call("rbind",L))

par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,1,1))
hist(df$a.est)
plot(df$b0,df$b0.est); abline(0,1)
plot(df$b1,df$b1.est); abline(0,1)



