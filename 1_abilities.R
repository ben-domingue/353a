##going to use  simulation code similar to in last example. feel free to omit code between large comment bars

#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
##number of items and people.
##we'll start small just so that you can see everything, but you'll want to make this bigger downstream.
ni<-20
np<-200
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

#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
##now let's consider 2 ways of estimating ability
library(mirt)
mod<-mirt(data.frame(resp),1,itemtype="2PL")
th1<-fscores(mod)

##via ML after we get item parameters from mirt
loglik.2pl<-function(th,x,a,b) {
    p<-1/(1+exp(-1*(a*th-b)))
    q<-1-p
    -1*sum(x*log(p)+(1-x)*log(q)) #to get maxim from optim
}
co<-coef(mod)
co<-co[-length(co)]
co<-do.call("rbind",co)
a<-co[,1]
b<- -1*co[,2]
th2<-numeric()
for (i in 1:nrow(resp)) th2[i]<-optim(0,loglik.2pl,x=resp[i,],a=a,b=b,method="Brent",lower=-5,upper=5)$par

z<-data.frame(true=th,th.mirt=th1,th.ml=th2)
plot(z)

