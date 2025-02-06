library(GDINA)
##https://doi.org/10.18637/jss.v093.i14

m <- GDINA(frac20$dat,frac20$Q, model = "DINA",
               att.dist="higher.order",higher.order=list(nquad=31,model = "2PL"))

th<-personparm(m,"HO") # higher-order ability
map<-personparm(m,"MAP")
cor(data.frame(th=th[,1],map[,1:3])) ##what do you think?
coef(m,'lambda') ##how do these look relative to what 
