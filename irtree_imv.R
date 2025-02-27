library("irtrees")
library("lme4")
library(imv)


## linresp examples
# define mapping matrix and prepare data for analysis
linmap <- cbind(c(0, 1, 1), c(NA, 0, 1)) 
x <- dendrify(linresp, linmap)
x$id<-x$person
x$person<-NULL
x$resp<-x$value
x$value<-NULL

################################################################################
##OOS for dichotomous IMV over k-folds
x$bigid<-paste(x$id,x$item)
L<-split(x,x$bigid)
gr<-sample(1:4,length(L),replace=TRUE)

om<-numeric()
for (i in unique(gr)) {
    print(i)
    Li<-L[gr!=i]
    Lo<-L[gr==i]
    ##
    x<-data.frame(do.call("rbind",Li))
    ## The multidimensional model for linear response trees
    m0 <- glmer(resp ~ 0 + item:node + (0 + node | id), 
                family = binomial, data = x)
    ## The unidimensional model for linear response trees
    m1 <- glmer(resp ~ 0 + item:node + (1 | id), 
                family = binomial, data = x)
    oos<-data.frame(do.call("rbind",Lo))
    p0<-predict(m0,newdata=oos,type='response')
    p1<-predict(m1,newdata=oos,type='response')
    om[as.character(i)]<- imv::imv.binary(oos$resp,p0,p1)
}
mean(om)

################################################################################
##IMV for polytomous response over 1 fold
###
imv_c<-function(y,pctt.tab,p1,p2) {
  nn<-length(unique(y$resp))
  om<-numeric()
  iis<-0:(nn-1)
  for (ii in iis) {
    ns<-om.tmp<-numeric()
    jjs<-iis[-match(ii,iis)]
    for (jj in jjs) {
      y2<-y[y$resp %in% c(ii,jj),]
      resp<-ifelse(y2$resp==ii,1,0)
      ##irt p-values for being
      p1.ii<-y2[[paste(p1,ii,sep='')]]
      p1.jj<-y2[[paste(p1,jj,sep='')]]
      p2.ii<-y2[[paste(p2,ii,sep='')]]
      p2.jj<-y2[[paste(p2,jj,sep='')]]
      ##
      z<-data.frame(resp=resp,
                    p1=p1.ii/(p1.ii+p1.jj),
                    p2=p2.ii/(p2.ii+p2.jj)
      )
      j0<-as.character(jj)
      om.tmp[j0]<-imv::imv.binary(z[,1],z[,2],z[,3])
      ns[as.character(jj)]<-nrow(z)
    }
    om[ii+1]<-sum(om.tmp*ns)/sum(ns)
  }
  omega_c <- sum(om*pctt.tab)/sum(pctt.tab)
  return(omega_c)
}

1->i
print(i)
Li<-L[gr!=i]
Lo<-L[gr==i]
##
x<-data.frame(do.call("rbind",Li))
## The multidimensional model for linear response trees
m0 <- glmer(resp ~ 0 + item:node + (0 + node | id), 
            family = binomial, data = x)
## The unidimensional model for linear response trees
m1 <- glmer(resp ~ 0 + item:node + (1 | id), 
            family = binomial, data = x)
oos<-data.frame(do.call("rbind",Lo))

L<-split(oos,oos$bigid)
##add the unobserved nodes (a response of 0 will just have one row in the tree, need both)
n<-sapply(L,nrow)
ll<-L[n==1]
zz<-do.call("rbind",ll)
zz$resp<-NA
zz$node<-'node2'
zz$sub<-sub('node1','node2',zz$sub)
oos<-data.frame(rbind(oos,zz))

p0<-predict(m0,newdata=oos,type='response')
p1<-predict(m1,newdata=oos,type='response')

oos$p0<-p0
oos$p1<-p1
L<-split(oos,oos$bigid)
##probabilities
f<-function(L,pvalue) {
    infun<-function(z,pvalue) {
        z<-z[order(z$node),] #node1 always first
        z$p<-z[[pvalue]]
        p<-numeric()
        p[1]<-1-z$p[1]
        p[2]<-(z$p[1])*(1-z$p[2])
        p[3]<-(z$p[1])*(z$p[2])
        p
    }
    z<-lapply(L,infun,pvalue=pvalue)
    data.frame(do.call("rbind",z))
}
tab0<-f(L,'p0')
tab1<-f(L,'p1')
names(tab0)<-paste("m0.",0:2,sep='')
names(tab1)<-paste("m1.",0:2,sep='')

resp.oos<-sapply(L,function(x) sum(x$resp,na.rm=TRUE)) #the out-of-sample responses
y<-data.frame(resp=resp.oos,tab0,tab1)

resp<-as.numeric(linresp)
pctt.tab <- c()
for (i in 0:(length(unique(resp))-1)){
    pctt.tab <- c(pctt.tab, sum(resp == i)/length(resp))
}

imv_c(y,pctt.tab,p1='m0.',p2='m1.')
