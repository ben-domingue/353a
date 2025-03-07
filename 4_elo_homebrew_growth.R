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
    df$th<-ifelse(df$gr==1,df$th+df$t/T,df$th) ##here we are adding growth
    ##
    df$p<-invlogit(df$th-df$b)
    df$resp<-rbinom(nrow(df),1,df$p)
    df$th0<-df$b0<-NA
    df
}
Np<-500
Ni<-70
xxx<-sim.growth(Np=Np,Ni=Ni,N=10000,prop.grow=.25) ###xxx is the simulated data

out<-list()
K<-.3 ##feel free to experiment with K
df<-est(df=xxx,K=K) ##df is now `est` but with estimates
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
out[[as.character(K)]]<-c(c1,e1,c2,e2) #this contains info on the quality of our esitmates. would be useful if we wanted to identify a `K` values via CV

##let's look at estimates growth as a function of `gr` status
L<-split(df,df$id)
f<-function(x) {
    x$t<-x$t/nrow(df)
    m<-lm(th0~t,x)
    sl<-coef(m)[2]
    c(unique(x$gr),sl)
}
z<-t(sapply(L,f))
by(z[,2],z[,1],summary)

##theory: error should decrease for theta estimates based on more observations
f<-function(x) {
    n<-nrow(x)
    delta<-x$th[n]-x$th0[n]
    c(n,delta)
}
z<-t(sapply(L,f))
S<-by(z[,2]^2,z[,1],mean)
plot(S)


##let's look at the items
L<-split(df,df$item)
plot(NULL,xlim=c(1,nrow(df)),ylim=c(-3,3))
for (i in 1:length(L)) lines(L[[i]]$t,L[[i]]$b0)
lm(b0~t,df) 
lm(th0~t,df)
##we observe some degree of growth in the items. what do you make of this (esp relative to incomplete growth in the growth group)
