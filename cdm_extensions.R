library(GDINA)
##https://doi.org/10.18637/jss.v093.i14

frac20$dat
frac20$Q
m <- GDINA(frac20$dat,frac20$Q,model="DINA")

##let's combine the irt and cdm approaches: what happens if we regress estimated attributes on theta estimated from a 1pl?
map <- personparm(m, what = "MAP")
library(mirt)
m.irt<-mirt(frac20$dat,1,'Rasch')
th<-fscores(m.irt)
co<-list()
for (i in 1:8) co[[i]]<-glm(map[,i]~th[,1],family='binomial')$coef

th<-seq(min(th),max(th),length.out=1000)
plot(NULL,xlim=range(th),ylim=c(0,1))
for (i in 1:8) {
    z<-co[[i]][1]+co[[i]][2]*th
    p<-1/(1+exp(-z))
    lines(th,p)
} ##what do you think?



##nonsense Q: let's replace our Q matrix with nonsense. what happens?
set.seed(10103101)
Q<-frac20$Q
for (i in 5:8) Q[,i]<-rbinom(nrow(Q),1,mean(Q[,i],na.rm=TRUE))
m2 <- GDINA(frac20$dat,Q,model="DINA")
anova(m,m2)
