##simulate data
N<-5000
n<-50
item<-1:n
id<-1:N
th<-sort(rnorm(N))
b<-rnorm(n)
pi<-.1
g<-.4
guesser<-rbinom(N,1,pi)
##
df<-expand.grid(id=id,item=item)
df<-merge(df,data.frame(item=item,b=b))
df<-merge(df,data.frame(id=id,th0=th,guesser=guesser))
f<-function(x) 1/(1+exp(-x))
df$p0<-f(df$th0-df$b)
df$p<- ifelse(df$guesser==1,g,df$p0)
df$y<-rbinom(nrow(df),1,df$p)
df<-df[,c("id","item","y","b")]
   
person<-function(x,pars) { #this function will be used to compute posterior probability each person is a gusser
    g<-pars[1]
    pi<-pars[2]
    p<-pars[3] #probability that individual i was a guesser
    ##likelihood of responses under guessing
    n<-sum(x$y)
    N<-nrow(x)
    like.guess<-g^n*(1-g)^(N-n)
    ##likelihood of responses under not guessing
    ##first get ML estimate of theta
    f<-function(th,b,resp) {
        pp<-1/(1+exp(-1*(th-b)))
        ll<-resp*log(pp)+(1-resp)*log(1-pp)
        -1*sum(ll)
    }
    th.est<-optim(0,f,b=x$b,resp=x$y,lower=-5,upper=5,method='Brent')$par
    ##then use that to generate likelihood  theta
    f<-function(x) 1/(1+exp(-x))
    pr<-f(th.est-x$b)
    pr<-ifelse(x$y==1,pr,1-pr)
    ll.noguess<-sum(log(pr))
    ##posterior for pi
    pi*like.guess/(pi*like.guess+(1-pi)*exp(ll.noguess))
}

##estimation
L<-split(df,df$id)
tab<-list()
##initial estimates
pi0<- .2
g0<-.3
tab$`0`<-c(pi0,g0)
p<-rep(pi0,length(unique(df$id)))
##
counter<-1
delta<-1
while (delta>.005) {
    ##1. update p
    for (j in 1:length(L)) p[j]<-person(x=L[[j]],pars=c(g0,pi0,p[j]))
    ##2. update g by looking at mean(x*p_i) [the average response for the people in the guessing group]
    tmp0<-data.frame(id=names(L),post=p)
    tmp<-merge(df,tmp0)
    g0.old<-g0
    g0<-mean(tmp$y[tmp$post>.5])
    ##3. update pi by looking at mean(p_i)    
    pi0.old<-pi0
    pi0<-mean(tmp0$post)
    tab[[as.character(counter)]]<-(c(pi0,g0))
    counter<-counter+1
    delta<-abs(pi0-pi0.old)+abs(g0-g0.old)
}

##visualize results
z<-do.call("rbind",tab)
plot(z[,1],ylim=0:1,type='l')
abline(h=pi)
mtext(side=4,las=2,at=pi,'pi')
lines(z[,2],col='red')
abline(h=g,col='red')
mtext(side=4,las=2,at=g,'g')
##like colored lines should meet
