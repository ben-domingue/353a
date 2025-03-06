update<-function(th,b,resp,K=1) {
    p<-1/(1+exp(-(th-b)))
    th.new<-th+K*(resp-p)
    c(th.new)
}
invlogit<-function(x) 1/(1+exp(-x))
rmse<-function(x) sqrt(mean(x^2))
est.fixed<-function(df,K) {
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
        test<-is.na(df$th0[i])
        if (test) {
            z<-df[df$id==df$id[i] & df$t<df$t[i],,drop=FALSE]
            th0<-z$th0[nrow(z)]
            b0<-df$b[i]
            pars<-update(th0,b0,df$resp[i],K=K)
            df$th0[i]<-pars[1]
        } else {#also set single NA values to 0
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
    df$th<-ifelse(df$gr==1,df$th+2*sin(2*df$t*pi/T),df$th)
    ##
    df$p<-invlogit(df$th-df$b)
    df$resp<-rbinom(nrow(df),1,df$p)
    df$th0<-NA
    df
}

Np<-50
Ni<-150
xxx<-sim.growth(Np=Np,Ni=Ni,N=20000,prop.grow=.5)

out<-list()
K<-.5 ##feel free to experiment with K
df<-est.fixed(df=xxx,K=K)
##
ff<-function(x) x[nrow(x),,drop=FALSE]
##
L<-split(df,df$id)
x<-data.frame(do.call("rbind",lapply(L,ff)))
c1<-cor(x$th,x$th0)
e1<-rmse(x$th-x$th0)
out[[as.character(K)]]<-c(c1,e1)

##plots of the growers
z<-df[df$gr==1,]
L<-split(z,z$id)
par(mfrow=c(5,5),mar=c(0,0,0,0))
for (i in 1:length(L)) {
    plot(NULL,xlim=c(1,nrow(df)),ylim=c(-3,3),xaxt='n',yaxt='n')
    lines(L[[i]]$t,L[[i]]$th0)
}


