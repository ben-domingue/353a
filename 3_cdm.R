library(GDINA)
##https://doi.org/10.18637/jss.v093.i14

frac20$dat
frac20$Q
m <- GDINA(frac20$dat,frac20$Q,model="DINA")
coef(m)
frac20$Q #make sure you can make sense of this net to the above line, coef(m)

coef(m,what='lambda') #what is this? 

co<-coef(m,what='delta')
co<-do.call("rbind",co)
plot(data.frame(cbind(colMeans(frac20$dat),co)))

map <- personparm(m, what = "MAP")
map[1:5,]

#sum scores and proportions of attributes
rs<-rowSums(frac20$dat)
L<-split(map,rs)
z<-sapply(L,colMeans)
plot(NULL,xlim=range(rs),ylim=0:1,xlab='sum scores',ylab='proportion of correct responses by attribute')
for (i in 1:8) lines(z[i,])

#comparing model-implied proportions and observed ones
Q<-frac20$Q
co.empirical<-list()
for (i in 1:nrow(Q)) {
    index<-which(Q[i,]==1)
    submap<-map[,index,drop=FALSE]
    gr<-rowSums(submap)==length(index)
    gr<-split(frac20$dat[,i],gr)
    co.empirical[[as.character(i)]]<-sapply(gr,mean,na.rm=TRUE)
}
co.empirical<-do.call("rbind",co.empirical)
par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1))
for (ii in 1:2) {
    plot(co[,ii],co.empirical[,ii],xlab='model',ylab='empirical',type='n')
    if (ii==1) co[,ii]->xv else rowSums(co)->xv
    text(xv,co.empirical[,ii],1:nrow(co))
    abline(0,1)
} #so we see good agreement! obviously, estimating the delta values is easy *if* you know the map values. but, of course, getting those is the hard part! 

#nonsense Q
set.seed(10103101)
Q<-frac20$Q
for (i in 5:8) Q[,i]<-rbinom(nrow(Q),1,mean(Q[,i],na.rm=TRUE))
m2 <- GDINA(frac20$dat,Q,model="DINA")
anova(m,m2)

