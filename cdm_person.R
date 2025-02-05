library(GDINA)
##https://doi.org/10.18637/jss.v093.i14

frac20$dat
frac20$Q
m <- GDINA(frac20$dat,frac20$Q,model="DINA")

##Now let's look at person ability estimates
map <- personparm(m, what = "MAP")
map[1:5,] ##what do we have here?

coef(m,'lambda') #this is a different perspective. what is this?

## sum scores and proportions of attributes: let's look at the proportion of respondents with a given sum score who have an attribute.
## before you run the below, formulate a hypothesis about what you will see!!
rs<-rowSums(frac20$dat)
L<-split(map,rs)
z<-sapply(L,colMeans)
plot(NULL,xlim=range(rs),ylim=0:1,xlab='sum scores',ylab='proportion of correct responses by attribute')
for (i in 1:8) lines(z[i,])

## comparing model-implied proportions and observed ones
## let's put things together and look at the item parameters as compared to descriptive statistics if we treat ability as known.
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
    legend('topleft',bty='n',legend=ifelse(ii==1,'guess','1-slip'))
    if (ii==1) co[,ii]->xv else rowSums(co)->xv
    text(xv,co.empirical[,ii],1:nrow(co))
    abline(0,1)
} #so we see good agreement! obviously, estimating the delta values is easy *if* you know the map values. but, of course, getting those is the hard part! 


