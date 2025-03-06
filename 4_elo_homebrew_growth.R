source("00-elofuns.R")

##now with growth
sim.growth<-function(Np,Ni,N,prop.grow=.25) {
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
    z<-data.frame(id=id,th=rnorm(Np),gr=rbinom(Np,1,prop.grow)) #gr is the growth group
    df<-merge(df,z)
    z<-data.frame(item=item,b=rnorm(Ni))
    df<-merge(df,z)
    ##
    T<-max(df$t)
    df$th<-ifelse(df$gr==1,df$th+df$t/T,df$th)
    ##
    df$p<-invlogit(df$th-df$b)
    df$resp<-rbinom(nrow(df),1,df$p)
    df$th0<-df$b0<-NA
    df
}
Np<-500
Ni<-70
xxx<-sim.growth(Np=Np,Ni=Ni,N=10000,prop.grow=.25)

out<-list()
K<-.3
#for (K in c(.1,.3,.5)) {
    df<-est(df=xxx,K=K)
    ##
    ff<-function(x) x[nrow(x),,drop=FALSE]
    ##
    L<-split(df,df$id)
    x<-data.frame(do.call("rbind",lapply(L,ff)))
    c1<-cor(x$th,x$th0)
    e1<-rmse(x$th-x$th0)
    ##
    L<-split(df,df$item)
    x<-data.frame(do.call("rbind",lapply(L,ff)))
    c2<-cor(x$b,x$b0)
    e2<-rmse(x$b-x$b0)
    out[[as.character(K)]]<-c(c1,e1,c2,e2)
                                        #}

L<-split(df,df$id)
f<-function(x) {
    x$t<-x$t/nrow(df)
    m<-lm(th0~t,x)
    sl<-coef(m)[2]
    c(unique(x$gr),sl)
}
z<-t(sapply(L,f))
by(z[,2],z[,1],summary)

f<-function(x) {
    n<-nrow(x)
    delta<-x$th[n]-x$th0[n]
    c(n,delta)
}
z<-t(sapply(L,f))
S<-by(z[,2]^2,z[,1],mean)
plot(S)

z<-df[df$gr==1,]
L<-split(z,z$id)
plot(NULL,xlim=c(1,nrow(df)),ylim=c(-3,3))
for (i in 1:length(L)) lines(L[[i]]$t,L[[i]]$th0)

##let's look at hte items
L<-split(df,df$item)
plot(NULL,xlim=c(1,nrow(df)),ylim=c(-3,3))
for (i in 1:length(L)) lines(L[[i]]$t,L[[i]]$b0)
lm(b0~t,df) #uhoh
lm(th0~t,df)
