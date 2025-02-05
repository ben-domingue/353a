library(GDINA)
##https://doi.org/10.18637/jss.v093.i14

frac20$dat
frac20$Q

dat <- sim10GDINA$simdat
Q <- sim10GDINA$simQ
                                        # --- Higher order G-DINA model ---#
mod12 <- GDINA(dat = dat, Q = Q, model = "DINA",
               att.dist="higher.order",higher.order=list(nquad=31,model = "2PL"))
th<-personparm(mod12,"HO") # higher-order ability
map<-personparm(mod12,"MAP")
cor(data.frame(th=th[,1],map[,1:3])) ##what do you think?
