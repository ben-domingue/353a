library(GDINA)
##https://doi.org/10.18637/jss.v093.i14

frac20$dat
frac20$Q
m <- GDINA(frac20$dat,frac20$Q,model="DINA")
summary(m) #peruse the output

##item parameters
co<-coef(m,what='delta')
co<-do.call("rbind",co)
co[1:3,] ##here are the 'delta' parameters, see 'alt formulation' on slide with heading 'The DINA model: One example of skills -> knowledge states'
##can you compute the guessing and slip parameters for these items?

##here is a different version of item parameters. can you map what you are seeing here onto what we just looked at?
coef(m)[1:3]

##now let's look at the item parameters as compared to the item p-values (average proportion correct for each item)
plot(data.frame(pvalue=colMeans(frac20$dat),guess=co[,1],`one.minus.slip`=rowSums(co)))

