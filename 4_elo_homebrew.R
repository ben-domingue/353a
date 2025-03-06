source("00-elofuns.R")


sim<-function(Np,Ni,N=10000) {
    id<-1:Np
    item<-paste("item",1:Ni)
    L<-list()
    for (i in 1:N) {
        id0<-sample(id,1)
        item0<-sample(item,1)
        L[[i]]<-data.frame(id=id0,item=item0)
    }
    df<-data.frame(do.call("rbind",L))
    df$t<-1:nrow(df)
    z<-data.frame(id=id,th=rnorm(Np))
    df<-merge(df,z)
    z<-data.frame(item=item,b=rnorm(Ni))
    df<-merge(df,z)
    df$p<-invlogit(df$th-df$b)
    df$resp<-rbinom(nrow(df),1,df$p)
    df$th0<-df$b0<-NA ##these will be estimates
    df
}

##static example
Np<-500
Ni<-25
xxx<-sim(Np=Np,Ni=Ni,N=10000)
df<-est(xxx,K=.25)

ff<-function(x) x[nrow(x),,drop=FALSE]
par(mgp=c(2,1,0),mfrow=c(1,2))
L<-split(df,df$id)
x<-data.frame(do.call("rbind",lapply(L,ff)))
plot(x$th,x$th0,xlab='true theta',ylab='elo estimate')
L<-split(df,df$item)
x<-data.frame(do.call("rbind",lapply(L,ff)))
plot(x$b,x$b0,xlab='true diff',ylab='elo estimate')
