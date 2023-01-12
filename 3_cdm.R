library(GDINA)
##https://doi.org/10.18637/jss.v093.i14

frac20$dat
frac20$Q
m <- GDINA(frac20$dat,frac20$Q,model="DINA")
coef(m)
coef(m,what='lambda')

map <- personparm(m, what = "MAP")
map[1:5,]


#sum scores and proportions of attributes
rs<-rowSums(frac20$dat)
L<-split(map,rs)
z<-sapply(L,colMeans)
plot(NULL,xlim=range(rs),ylim=0:1)
for (i in 1:8) lines(z[i,])

#nonsense Q
Q<-frac20$Q
for (i in 5:8) Q[,i]<-rbinom(nrow(Q),1,mean(Q[,i],na.rm=TRUE))
m2 <- GDINA(frac20$dat,Q,model="DINA")
anova(m,m2)

#can you do something with irt to ask how hierarchical q matirx is?
