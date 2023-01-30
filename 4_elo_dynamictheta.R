#https://cran.r-project.org/web/packages/elo/vignettes/running_elos.html
#we're going to give the elo algorithm a relatively hard challenge as ability is going to be pretty unstructured

N<-16 #number of teams
nweek<-200 #ability will vary across weeks
ngames.week<-(N-1) #each team will play a round robin each week
th0<-rnorm(N,sd=.5) #each team starts with a random ability at the outset
th<-list()
for (i in 1:N) {
    tmp<-c(th0[i],rnorm(nweek-1,mean=0,sd=.3)) #the team's ability over weeks is a random walk!
    th[[i]]<-cumsum(tmp)
}
names(th)<-paste("team",1:N)


#we're again going to use an item response model to simulate outcomes.
#this is completely unnecessary (we could generate it directly from the BLT with the theta values) but i can't help myself :)
getresp<-function(th) { 
    a<-1
    b<-rnorm(100)
    invlogit<-function(x) 1/(1+exp(-x))
    k<-th-b
    k<-k*a 
    p<-invlogit(k)
    resp<-rbinom(length(p),1,p)
    rs<-sum(resp)
}
##note that we're also throwing away information here when we go from a difference in sum scores to just win/loss! 

out<-list()
for (i in 1:nweek) {
    for (j in 1:N) {
        index<-sample((1:N)[-j],ngames.week)
        for (index.tmp in index) {
            th1<-getresp(th[[j]][i])
            th2<-getresp(th[[index.tmp]][i])
            out[[paste(i,j,index.tmp)]]<-data.frame(team.Home=j,team.Visitor=index.tmp,points.Home=th1,points.Visitor=th2,week=i)
        }
    }
}
df<-data.frame(do.call("rbind",out))
df$team.Home<-paste("team",df$team.Home)
df$team.Visitor<-paste("team",df$team.Visitor)

library(elo)
m<-elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor +group(week),
              data = df, k = 20)
est<-as.matrix(m)
est<-est[,names(th)]

M<-mean(unlist(th))
S<-sd(unlist(th))
for (i in 1:length(th)) th[[i]]<-(th[[i]]-M)/S
M<-mean(est)
S<-sd(est)
for (i in 1:ncol(est)) est[,i]<-(est[,i]-M)/S


NN<-floor(sqrt(N))+1
NN2<-ifelse(NN*(NN-1)>N,NN-1,NN)
par(mfrow=c(NN,NN2),mar=rep(0,4),oma=rep(.2,4))
rdiff<-numeric()
for (i in 1:length(th)) {
    plot(NULL,xlim=c(1,nweek),ylim=c(-3,3))
    lines(1:nweek,th[[i]],lwd=2)
    lines(1:nweek,est[,i],col='red',lwd=2)
    rdiff[i]<-cor(th[[i]],est[,i])
}
summary(rdiff)
