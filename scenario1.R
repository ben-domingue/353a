source("00funs.R") ##https://github.com/ben-domingue/353a/blob/main/00funs.R


################################3
##simulate data with naughty cdm
cdm.sim<-function(a,N=1000,sk.offset=0,nsk=6,bound=NULL) {
    ##skills
    th<-rnorm(N)
    sk<-runif(nsk)
    p<-a*(outer(th,sk-mean(sk),'-'))
    p<-p+sk.offset ##controlling prevalence of skills
    p<-apply(p,2,function(x) 1/(1+exp(-(x))))
    sk<-apply(p,2,function(x) rbinom(nrow(p),1,p))
    ##qmatrix
    S<-TRUE
    while (S) {
        qm<-matrix(rbinom(50*nsk,1,.65),50,nsk)
        S<-any(c(colMeans(qm),rowMeans(qm))==0)
    }        
    ##response probabilities 
    pL<-respL<-list()
    if (is.null(bound)) {
        g<-runif(nrow(qm),min=0,max=.35)
        s<-runif(nrow(qm),min=0,max=.35)
    } else {
        g<-rep(bound,nrow(qm))
        s<-rep(bound,nrow(qm))
    }
    for (i in 1:nrow(qm)) {
        ii<-which(qm[i,]==1)
        z<-sk[,ii,drop=FALSE]
        rm<-rowMeans(z)
        p.tmp<-ifelse(rm==1,1-s[i],g[i])
        respL[[i]]<-rbinom(N,1,p.tmp)
        pL[[i]]<-p.tmp
    }
    resp<-do.call("cbind",respL)
    p.true<-do.call("cbind",pL)
    resp<-as.data.frame(resp)
    names(resp)<-paste("i",1:ncol(resp),sep='')
    ##cv imv values
    om<-oos.compare.newresp(resp,qm,truep=p.true)
    ##
    om
}
a<-sort(runif(100,min=0,max=3))
library(parallel)

out2<-list()
for (i in c(.1,.2,.3)) out2[[as.character(i)]]<-mclapply(a,cdm.sim,mc.cores=10,sk.offset=1.5,bound=i)



#pdf("/home/bdomingu/Dropbox/Apps/Overleaf/CDM_predictions/scenario1.pdf",width=6,height=3)
par(mgp=c(2,1,0),mfrow=c(3,2),mar=c(3,3,1,1),oma=rep(.5,4))
####
for (i in 1:length(out2)) {
    plot(NULL,xlim=c(0,3),ylim=c(0,.6),xlab='a',ylab='RMSE(oos resp,est)')
    pf<-function(x,y,...) {
        m<-loess(y~x)
        lines(x,predict(m),...,lwd=2)
    }
    f<-function(out,...) {
        z<-do.call("rbind",out)
        irt<-z[,4]
        cdm<-z[,5]
        irt.p<-z[,6]
        cdm.p<-z[,7]
        pf(a,irt,col='blue',...)
        pf(a,cdm,col='red',...)
        pf(a,irt.p,col='blue',...,lty=2)
        pf(a,cdm.p,col='red',...,lty=2)
    }
                                        #f(out1)
    f(out2[[i]])
    legend("topright",bty='n',lty=c(1,1,2,2),col=c("blue","red","blue","red"),cex=.7,
           c("(resp,IRT)","(resp,CDM)","(True,IRT)","(True,CDM)"))
#####
    plot(NULL,xlim=c(0,3),ylab="IMV",xlab='a',ylim=c(0,.15))
    f<-function(out,...) {
        om<-do.call("rbind",out)
        abline(h=0)
        pf(a,om[,1],...)
        pf(a,om[,2],col='blue',...,lty=2)
        pf(a,om[,3],col='red',...,lty=2)
    }
    f(out2[[i]])
    legend("topright",bty='n',lty=c(1,2,2),col=c("black","blue","red"),cex=.7,
           c("(IRT,CDM)","(IRT,True)","(CDM,True)")
           )
}
##legend("topright",bty='n',lwd=1,lty=c(1,2),title=expression(delta),legend=c(0,1.5))
##dev.off()
