set.seed(1010103010)
th<-rnorm(5000)
b<-sort(rnorm(25))
p<-outer(th,b,'-')
invlogit<-function(x) 1/(1+exp(-x))
p<-invlogit(p)

resp<-matrix(NA,nrow=nrow(p),ncol=ncol(p))
for (i in 1:ncol(p)) resp[,i]<-rbinom(nrow(p),1,p[,i])

levs<-cut(b,c(-Inf,seq(-3,3,by=1),Inf),labels=FALSE)
Q<-list()
for (i in 1:length(b)) {
    tmp<-rep(0,max(levs))
    tmp[1:levs[i]]<-1
    Q[[i]]<-tmp
}
Q<-do.call("rbind",Q)

library(GDINA)
m <- GDINA(resp,Q,model="DINA")

coef(m)
coef(m,what='lambda')

co<-coef(m,what='delta')
co<-do.call("rbind",co)
plot(data.frame(cbind(b,co)))

map <- personparm(m, what = "MAP")
colMeans(map)
