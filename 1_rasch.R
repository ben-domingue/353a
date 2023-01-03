# Getting started ---------------------------------------------------------
## this will be our maiden voyage estimating IRT models
## the main goal is to get oriented with the key output 
## that we get from applications of these models to item response data

resp1 <- read.table("https://github.com/ben-domingue/252L/raw/master/data/emp-rasch.txt", header=FALSE)

## fit the rasch model
library(mirt) # might have to run install.packages("mirt")
m1 <- mirt(resp1, 1, itemtype = "Rasch")
m1 #here is the object containing the estimated rasch model. it contains lots of stuff, we're just seeing a quick summary here

##it has plot methods attached that will generate item response functions (or trace lines, as they are called here)
plot(m1, type = "trace") ## which is the easiest item? the most difficult item?

## we can use the below to get item parameters
coef(m1)

##that was unwieldy, here is a better way of getting that output
get_coef <- function(mod) {
  co <- coef(mod)
  co <- co[-length(co)]#why do i get rid of this last bit?
  do.call("rbind", co)
}
coef<-get_coef(m1) #first column is alpha (note that they don't vary), second column is 'difficulty', ignore third and fourth [we'll talk about all of this in a bit]
coef

## in particular, i would look over this closely vis-a-vis the relevant part of the mirt manual:
## Rasch Only one intercept estimated, and the latent variance of
##      theta is freely estimated. If the data have more than two
##      categories then a partial credit model is used instead (see
##      'gpcm' below).
##           P(x = 1|theta, d) = \frac{1}{1 + exp(-(theta + d))}      

##note that there is something different when we compare "our" version of the Rasch model to the mirt version.
##It's very important that you note this difference!
##so, be able to make sure you can explain this!
plot(colMeans(resp1,na.rm=TRUE),coef[,2],xlab="p-values",ylab="raw mirt estimates")

## here is a fun way of looking at comparing the estimated icc to empirical data
itemfit(m1, empirical.plot = 3)
