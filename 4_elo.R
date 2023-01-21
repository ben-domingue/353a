##question to pose in slides: how could you reduce correlation between the elo and theta values?

##might be interesting to write down what the sum score is for each person (sum of probabilities) and compare that to the key equation in BLT model?


#static case
th<-rnorm(100)
a<-1
b<-rnorm(100)
invlogit<-function(x) 1/(1+exp(-x))
k<-outer(th,b,'-')
k<-k*a #discrimination
p<-invlogit(k)
resp<-p
for (i in 1:ncol(p)) resp[,i]<-rbinom(nrow(p),1,p[,i])
rs<-rowSums(resp)

#each person plays M games
M<-5
N<-length(rs)
out<-list()
for (i in 1:N) {
    ii<-sample((1:N)[-i],M)
    out[[i]]<-data.frame(team.Home=i,team.Visitor=ii,points.Home=rs[i],points.Visitor=rs[ii])
}
df<-data.frame(do.call("rbind",out))
df$team.Home<-paste("team",df$team.Home)
df$team.Visitor<-paste("team",df$team.Visitor)

df0<-df[ df$points.Home != df$points.Visitor,]
library(elo)
m<-elo.glm(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = df0)

co<-coef(m)
co<-co[grepl("team",names(co))] #i can't figure out how to get home field out of this. gah!! see neutral() function, https://cran.r-project.org/web/packages/elo/vignettes/comparison_methods.html#winloss-logistic-regression?
z<-data.frame(team=names(co),elo.est=as.numeric(co))
tmp<-data.frame(team=paste('team',1:length(th)),th=th,rs=rs)
z$team<-gsub('`','',z$team)
z<-merge(z,tmp)
plot(z[,-1])
cor(z[,-1])
