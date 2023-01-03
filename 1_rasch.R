################################################################
##we're going to start by simulating data according to the rasch model

##number of items and people.
##we'll start small just so that you can see everything, but you'll want to make this bigger downstream.
ni<-10
np<-200
##now we're going to simulate data according to this model and examine some key properties
set.seed(12311)
##first let's describe the individual level part of the model
th<-rnorm(np)
th.mat<-matrix(th,np,ni,byrow=FALSE) #these are the true abilities. we don't observe them, which will be a real problem for us downstream. but we'll not worry about that today. 
th.mat #abilities, on per row
##now the item level part.
b<-rnorm(ni)
b.mat<-matrix(b,np,ni,byrow=TRUE) #these are the item difficulties
b.mat #difficulties, one per item

################################################################
##now we have to put this stuff together. what we want is a probability of a correct response for a person to an item
##we're going to use what you may know from logistic regression
inv_logit<-function(x) exp(x)/(1+exp(x))
##now the probability of a correct response is:
pr<-inv_logit(th.mat-b.mat) #note this is pairwise multiplication not matrix multiplication.

##we can simulate data using those probabilities
resp<-pr
for (i in 1:ncol(resp)) resp[,i]<-rbinom(nrow(resp),1,resp[,i])
resp<-data.frame(resp)
resp

################################################################
## this will be our maiden voyage estimating IRT models
## the main goal is to get oriented with the key output 
## that we get from applications of these models to item response data

## fit the rasch model
library(mirt) # might have to run install.packages("mirt")
m1 <- mirt(resp, 1, itemtype = "Rasch")
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
coef<-get_coef(m1) #second column is difficulty, we'll talk about the other columns later today
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
plot(colMeans(resp,na.rm=TRUE),coef[,2],xlab="p-values",ylab="raw mirt estimates")

## here is a fun way of looking at comparing the estimated icc to empirical data
itemfit(m1, empirical.plot = 3)
