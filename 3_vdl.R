##can you recover assocations between speed & abilities using estimates ignorant of 'the other side'?

simfun<-function(rho,miss=FALSE,np=1000,ni=50) {
    library(LNIRT)
    simLNIRT(np,ni,rho=rho)->z
    z$Y->resp
    z$RT->rt
    ##
    ncol(resp)->ni
    if (miss) {
        for (i in 1:nrow(resp)) {
            round(runif(1,min=40,max=60))->n
            sample(ni,n)->index
            resp[i,]->hold
            NA->resp[i,]
            hold[index]->resp[i,index]
            rt[i,]->hold
            NA->rt[i,]
            hold[index]->rt[i,index]
        }
    }
    ##
    library(mirt)
    mirt(data.frame(resp),1,itemtype="2PL")->mod
    fscores(mod)->th
    ##
    library(lme4)
    matrix(1:ni,byrow=TRUE,nrow=nrow(rt),ncol=ncol(rt))->item
    matrix(1:nrow(th),byrow=FALSE,nrow=nrow(rt),ncol=ncol(rt))->student
    coef(mod)->co
    do.call("rbind",co[-length(co)])->co
    matrix(co[,2],byrow=TRUE,nrow=nrow(rt),ncol=ncol(rt))->diff
    data.frame(rt=as.numeric(rt),item=as.numeric(item),student=as.numeric(student),diff=as.numeric(diff))->z
    z[!is.na(z$rt),]->z
    #lmer(rt~1+diff+(1|student),resp.lim)->m
    #lmer(rt~factor(item)+ (1|student),z)->m
    lmer(rt~1+(1|item)+(1|student),z)->m
    ranef(m)$student->re
    data.frame(student=rownames(re),ti=-1*re[,1])->p
    data.frame(student=1:nrow(th),th=th[,1])->tmp
    merge(p,tmp)->p
    c(rho,cor(p[,-1])[1,2])
}
rhovals<-runif(100,-.75,.75)
library(parallel)
mclapply(rhovals,simfun,mc.cores=30,miss=FALSE,ni=50,np=500)->out


par(mgp=c(2,1,0),oma=rep(.5,4))
lapply(out,as.numeric)->out
do.call("rbind",out)->tab
plot(tab,xlab=bquote("True Correlation ("*tau[p]*","*theta[p]*")"),
     ylab=bquote("Estimated Correlation ("*tau[p]*","*theta[p]*")"),
     pch=19)
abline(0,1)



