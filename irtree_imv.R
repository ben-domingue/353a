library("irtrees")
library("lme4")
library(imv)




################################################################################
##OOS for dichotomous IMV over k-folds
## linresp examples
# define mapping matrix and prepare data for analysis
linmap <- cbind(c(0, 1, 1), c(NA, 0, 1)) 
x <- dendrify(linresp, linmap)
x$id<-x$person
x$person<-NULL
x$resp<-x$value
x$value<-NULL
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

linmap <- cbind(c(0, 1, 1), c(NA, 0, 1)) 
x <- dendrify(linresp, linmap)
x$id<-x$person
x$person<-NULL
x$resp<-x$value
x$value<-NULL
x$bigid<-paste(x$id,x$item)
L<-split(x,x$bigid)
gr<-sample(1:4,length(L),replace=TRUE)
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

################################################################################
##IMV for polytomous response over 1 fold
##nested versus linear

##nested
nesmap <- cbind(c(0, 0, 1, 1), c(0, 1, NA, NA), c(NA, NA, 0, 1))
x <- dendrify(nesresp, nesmap)
x$id<-x$person
x$person<-NULL
x$resp<-x$value
x$value<-NULL
x$bigid<-paste(x$id,x$item)
x.nest<-x
## m.nest <- glmer(value ~ 0 + item:node + (0 + node | person),
##   family = binomial, data = nesrespT))
  
##linear
linmap<-rbind(c(0,NA,NA),c(1,0,NA),c(1,1,0),c(1,1,1))
x <- dendrify(nesresp, linmap)
x$id<-x$person
x$person<-NULL
x$resp<-x$value
x$value<-NULL
x$bigid<-paste(x$id,x$item)
x.lin<-x
## m.lin <- glmer(resp ~ 0 + item:node + (0 + node | id), 
##             family = binomial, data = x)

id<-unique(x.lin$bigid)
all(id==unique(x.nest$bigid)) #checking
gr<-sample(1:4,length(id),replace=TRUE)
gr<-data.frame(bigid=id,gr=gr)
1->i
print(i)
##
x.lin<-merge(gr,x.lin)
x.nest<-merge(gr,x.nest)
## The unidimensional model for linear response trees
m.lin <- glmer(resp ~ 0 + item:node + (1 | id), 
            family = binomial, data = x.lin[x.lin$gr!=1,])
## The unidimensional model for linear response trees
m.nest <- glmer(resp ~ 0 + item:node + (1 | id), 
            family = binomial, data = x.nest[x.nest$gr!=1,])

##add notes & compute category probs
##linear
oos<-x.lin[x.lin$gr==1,]
L<-split(oos,oos$bigid)
n<-sapply(L,nrow)
##
ll<-L[n==1]
zz<-do.call("rbind",ll)
zz$resp<-NA
zz$node<-'node2'
zz$sub<-sub('node1','node2',zz$sub)
zz1<-zz
zz$node<-'node3'
zz$sub<-sub('node1','node3',zz$sub)
oos<-data.frame(rbind(oos,zz1,zz))
##
ll<-L[n==2]
zz<-do.call("rbind",ll)
zz<-zz[zz$node=='node2',]
zz$resp<-NA
zz$node<-'node3'
zz$sub<-sub('node2','node3',zz$sub)
oos<-data.frame(rbind(oos,zz))
##
p<-predict(m.lin,newdata=oos,type='response')
oos$p0<-p
L<-split(oos,oos$bigid)
##probabilities
f<-function(L,pvalue) {
    infun<-function(z,pvalue) {
        z<-z[order(z$node),] #node1 always first
        z$p<-z[[pvalue]]
        p<-numeric()
        p[1]<-1-z$p[1]
        p[2]<-(z$p[1])*(1-z$p[2])
        p[3]<-(z$p[1])*(z$p[2])*(1-z$p[3])
        p[4]<-(z$p[1])*(z$p[2])*(z$p[3])
        p
    }
    z<-lapply(L,infun,pvalue=pvalue)
    data.frame(do.call("rbind",z))
}
tab.lin<-f(L,'p0')
names(tab.lin)<-paste("mlin.",0:3,sep='')

##add notes & compute category probs
##nested
oos<-x.nest[x.nest$gr==1,]
zz<-oos[oos$node=='node2',]
zz$resp<-NA
zz$node<-'node3'
zz$sub<-sub('node2','node3',zz$sub)
zz1<-zz
zz<-oos[oos$node=='node3',]
zz$resp<-NA
zz$node<-'node2'
zz$sub<-sub('node3','node2',zz$sub)
oos<-data.frame(rbind(oos,zz1,zz))
##
p<-predict(m.nest,newdata=oos,type='response')
oos$p0<-p
L<-split(oos,oos$bigid)
##probabilities
f<-function(L,pvalue) {
    infun<-function(z,pvalue) {
        z<-z[order(z$node),] #node1 always first
        z$p<-z[[pvalue]]
        p<-numeric()
        p[1]<-(1-z$p[1])*(1-z$p[2])
        p[2]<-(1-z$p[1])*(z$p[2])
        p[3]<-(z$p[1])*(1-z$p[3])
        p[4]<-(z$p[1])*(z$p[3])
        p
    }
    z<-lapply(L,infun,pvalue=pvalue)
    data.frame(do.call("rbind",z))
}
tab.nest<-f(L,'p0')
names(tab.nest)<-paste("mnest.",0:3,sep='')

##
L<-split(oos,oos$bigid)
f<-function(x) {
    x<-x[order(x$node),]
    ii<-which(is.na(x$resp))
    if (length(ii)==0) print(x)
    z<-x[-c(1,ii),,drop=FALSE]
    if (x$node[ii]=='node3') {
        if (z$resp==0) tr<-0 else tr<-1
    } else {
        if (z$resp==0) tr<-2 else tr<-3
    }
    tr
}
resp.oos<-sapply(L,f)
y<-data.frame(resp=resp.oos,tab.lin,tab.nest)

resp<-as.numeric(nesresp) ##not quite right, should be purely out of sample
pctt.tab <- c()
for (i in 0:(length(unique(resp))-1)){
    pctt.tab <- c(pctt.tab, sum(resp == i)/length(resp))
}

imv_c(y,pctt.tab,p1='mlin.',p2='mnest.')
