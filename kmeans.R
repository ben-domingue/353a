N<-1000
mu<-c(-1,0,1,2)
sigma<- .5
x<-list()
for (i in 1:length(mu)) x[[i]]<-rnorm(N,mean=mu[i],sd=sigma)
x<-unlist(x) ##no group information!

##define initial cluster assignment
M<-sort(rnorm(length(mu),mean=0,sd=2))

##assign/recenter
delta<-1
Mlist<-list()
Mlist[[1]]<-M
counter<-1
while (delta>.01) {
    d<-outer(x,M,'-')
    gr<-apply(abs(d),1,which.min)
    M0<-Mlist[[length(Mlist)]]
    M<-as.numeric(by(x,gr,mean))
    Mlist[[as.character(counter)]]<-M
    delta<-max(abs(M-M0))
    counter<-counter+1
}
tab<-do.call("rbind",Mlist)

par(mfrow=c(1,2),mgp=c(2,1,0))
plot(NULL,xlim=c(1,nrow(tab)),ylim=c(-3,3),ylab='group mean estimates')
cols<-colorRampPalette(c("blue", "red"))( ncol(tab))
for (i in 1:ncol(tab)) {
    lines(tab[,i],col=cols[i])
    abline(h=mu[i],col=cols[i])
}
plot(gr,xlab="observation",ylab='group membership')
table(sort(rep(1:4,N)),gr) ##confusion matrix
