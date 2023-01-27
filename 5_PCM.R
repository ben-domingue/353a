library(mirt)

##note: we're now going to apply pcm to same data. we just applied grm. this is a lawless space! ¯\_(ツ)_/¯
mod.pcm <- mirt(Science, 1,itemtype=rep("gpcm",ncol(Science))) 
mod.pcm #q: what do we have here?
coef(mod.pcm) #q: please stop and try to interpret these!

coef(mod.pcm,IRTpars=TRUE) #q: is this better?
plot(mod.pcm, type = 'trace')

##let's double check
extr <- extract.item(mod.pcm,1)
Theta <- matrix(seq(-6,6, length.out=2000))
pr <- probtrace(extr, Theta) #min() of first item
Theta[which(pr[,2]>pr[,1])[1]] #you should be again able to match these values to estimates for the $Comfort item. 
Theta[which(pr[,3]>pr[,2])[1]] #and this value
Theta[which(pr[,4]>pr[,3])[1]] #and this value


runif(nrow(Science))->test
ifelse(Science$Comfort==2 & test<.5,1,Science$Comfort)->Science$Comfort #what am i doing here? 
apply(Science,2,table)
mod.pcm.fake <- mirt(Science, 1,itemtype=rep("Rasch",ncol(Science))) 
coef(mod.pcm,IRTpars=TRUE) 
coef(mod.pcm.fake,IRTpars=TRUE) #q: can you make sense of this?
plot(mod.pcm.fake, type = 'trace')


