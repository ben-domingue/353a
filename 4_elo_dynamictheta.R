#https://cran.r-project.org/web/packages/elo/vignettes/running_elos.html

N<-25
nweek<-100
ngames.week<-5
#static case
th0<-rnorm(N,sd=.5)
th.grow<-rnorm(N,mean=0,sd=.1)
th<-list()
for (i in 1:N) th[[i]]<-th0[i]+th.grow[i]*1:nweek #nweek weeks to mimic structure of tournament data

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

out<-list()
for (i in 1:nweek) {
    for (ng in 1:ngames.week) {
        test<-TRUE
        while (test) {
            index<-sample(1:N)
            test<-any(index==1:N)
        }
        for (j in 1:N) {
            th1<-getresp(th[[j]][i])
            th2<-getresp(th[[index[j]]][i])
            out[[paste(i,j,ng)]]<-data.frame(team.Home=j,team.Visitor=index[j],points.Home=th1,points.Visitor=th2,week=i)
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


M<-mean(unlist(th))
S<-sd(unlist(th))
for (i in 1:length(th)) th[[i]]<-(th[[i]]-M)/S
M<-mean(est)
S<-sd(est)
for (i in 1:ncol(est)) est[,i]<-(est[,i]-M)/S

par(mfrow=c(5,5),mar=rep(0,4),oma=rep(.2,4))
rdiff<-numeric()
for (i in 1:length(th)) {
    plot(NULL,xlim=c(1,nweek),ylim=c(-3,3))
    lines(1:nweek,th[[i]])
    lines(1:nweek,est[,i],col='red')
    rdiff[i]<-cor(th[[i]],est[,i])
}
