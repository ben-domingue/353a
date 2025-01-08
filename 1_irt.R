################################################################
##first, we're going to simulate some data

##number of items and people.
##we'll start small just so that you can see everything, but you'll want to make this bigger downstream.
ni<-30
np<-2000
##now we're going to simulate data according to this model and examine some key properties
set.seed(12311)
##first let's describe the individual level part of the model
th<-rnorm(np)
th.mat<-matrix(th,np,ni,byrow=FALSE) #these are the true abilities. we don't observe them, which will be a real problem for us downstream. but we'll not worry about that today. 
##now the item level part.
##this is going to look like logistic regression, meaning we will have a slope and an intercept
a<-rep(1,ni)
a<-exp(rnorm(ni,sd=.3))
b<-rnorm(ni)
a.mat<-matrix(rep(a,np),np,ni,byrow=TRUE)
b.mat<-matrix(b,np,ni,byrow=TRUE) #these are the item difficulties

################################################################
##now we have to put this stuff together. what we want is a probability of a correct response for a person to an item
##we're going to use what you may know from logistic regression
inv_logit<-function(x) exp(x)/(1+exp(x))
##now the probability of a correct response is:
pr<-inv_logit(a.mat*(th.mat+b.mat)) #note this is pairwise multiplication not matrix multiplication.
##also, note that i am treating b.mat as item easiness params to make life simpler with mirt output

##we can simulate data using those probabilities
resp<-pr
for (i in 1:ncol(resp)) resp[,i]<-rbinom(nrow(resp),1,resp[,i])


##so let's estimate parameters 
library(mirt)
mod<-mirt(data.frame(resp),1,itemtype="2PL")

##Let's first look at parameter estimates
co<-coef(mod,simplify=TRUE)$items
co
plot(co[,2],b) #thoughts?
hist(a)

##We can also plot IRFs:
plot(mod,type='trace')

##Finally, we can get theta estimates and compare to truth
th.est<-fscores(mod)
info0<-plot(th,th.est[,1])


##We didn't simulate any guessing behavior. Can you modify above call to mirt to estimate the 3PL and see what it gives you? In particular, what are values for guessing parameters?


##################################################
##Information
th.seq<-seq(-5,5,length.out=1000)
info.mod<-testinfo(mod,th.seq)


##Now let's simulate data based on different architectures
sim<-function(b,a=1) {
    a.mat<-matrix(rep(a,np),np,ni,byrow=TRUE)
    b.mat<-matrix(b,np,ni,byrow=TRUE) #these are the item difficulties
    pr<-inv_logit(a.mat*(th.mat+b.mat)) #note this is pairwise multiplication not matrix multiplication.
    resp<-pr
    for (i in 1:ncol(resp)) resp[,i]<-rbinom(nrow(resp),1,resp[,i])
    library(mirt)
    mod1<-mirt(data.frame(resp),1,itemtype="2PL")
    testinfo(mod1,th.seq)
}

plot(th.seq,info.mod,type='l')
##
b<-rnorm(ni,mean=1.5)
info.1<-sim(b)
lines(th.seq,info.1,col='red')
##
b<-rnorm(ni,mean=-1.5)
info.2<-sim(b)
lines(th.seq,info.2,col='green')
##
b<-rnorm(ni,mean=0,sd=2)
info.3<-sim(b)
lines(th.seq,info.3,col='blue')
##
b<-rnorm(ni,mean=0,sd=1)
info.3<-sim(b,a=.75)
lines(th.seq,info.3,col='pink',lwd=4)
