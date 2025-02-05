library(GDINA)
##https://doi.org/10.18637/jss.v093.i14

frac20$dat
frac20$Q

dat <- sim10GDINA$simdat
Q <- sim10GDINA$simQ
                                        # --- Higher order G-DINA model ---#
mod12 <- GDINA(dat = dat, Q = Q, model = "DINA",
               att.dist="higher.order",higher.order=list(nquad=31,model = "2PL"))
personparm(mod12,"HO") # higher-order ability
