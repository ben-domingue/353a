library(GDINA)
##https://doi.org/10.18637/jss.v093.i14

frac20$dat
frac20$Q
m <- GDINA(frac20$dat,frac20$Q,model="DINA")

##we're going to look at the item parameters (for first three items) from two slightly different perspectives. make sure you can make sense of these two perspectives
coef(m)[1:3]
co<-coef(m,what='delta')
co<-do.call("rbind",co)
co[1:3,]

plot(data.frame(cbind(colMeans(frac20$dat),co)))

