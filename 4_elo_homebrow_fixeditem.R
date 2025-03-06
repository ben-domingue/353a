
last.value<-function(x) {
    w<-which(!is.na(x))
    x<-x[w[length(w)]]
}
update<-function(th,b,resp,K=1) {
    p<-1/(1+exp(-(th-b)))
    th.new<-th+K*(resp-p)
    th.new
}
invlogit<-function(x) 1/(1+exp(-x))
rmse<-function(x) sqrt(mean(x^2))
est<-function(K,df) {
    id<-unique(df$id)
    item<-unique(df$item)
    ##initialize first value
    df<-df[order(df$t),]
    for (i in id) {
        ii<-which(i==df$id)
        df$th0[ii[1]]<-0
    }
    ##estimate
    for (i in 1:nrow(df)) {
        print(i/nrow(df))
        test<-is.na(df$th0[i])
        if (test) {
            z<-df[df$id==df$id[i] & df$t<df$t[i],,drop=FALSE]
            th0<-z$th0[nrow(z)]
            pars<-update(th0,df$b[i],df$resp[i],K=K)
            df$th0[i]<-pars
        } else {
            df$th0[i]<-0
        }
    }
    df
}
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
    df$th0<-NA
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
    out[[as.character(K)]]<-c(c1,e1)
                                        #}

L<-split(df,df$id)
f<-function(x) {
    x$t<-x$t/nrow(df)
    m<-lm(th0~t,x)
    sl<-coef(m)[2]
    if (unique(x$gr)==1) {
    m<-lm(th~t,x)
    sl2<-coef(m)[2]
    } else sl2<-NA
    c(unique(x$gr),sl,sl2)
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


