##stealing simulation bit from 1_rasch.R
################################################################
################################################################
################################################################
################################################################
##we're going to start by simulating data according to the rasch model

##number of items and people.
##we'll start small just so that you can see everything, but you'll want to make this bigger downstream.
ni<-20
np<-500
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
################################################################
################################################################
################################################################
library(mirt)
m0<-mirt(data.frame(resp),1,'Rasch')

library(lme4)
##need to restructure data
df<-list()
for (i in 1:ncol(resp)) {
    df[[i]]<-cbind(1:nrow(resp), #person
                   i, #item
                   resp[,i]
                   )
}
df<-data.frame(do.call("rbind",df))
names(df)<-c("person","item","resp")
m1<-glmer(resp~0+as.character(item)+(1|person),family='binomial',df)

##compare
par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1))
##item part
co<-coef(m0)
co<-co[-length(co)]
co<-do.call("rbind",co)
fe<-fixef(m1)
nms<-gsub("as.character(item)","",names(fe),fixed=TRUE)
fe<-fe[order(as.numeric(nms))]
plot(co[,2],fe,xlab='mirt item param',ylab='lme4 item param'); abline(0,1)
##person part
th.mirt<-fscores(m0)
th.lmer<-ranef(m1)
plot(jitter(th.mirt[,1]),jitter(th.lmer[[1]][,1]),xlab='mirt theta',ylab='lme4 theta'); abline(0,1)

