library("irtrees")
library("lme4")
library(imv) #devtools::install_github("ben-domingue/imv", ref="main")

df<-irwpkg::irw_fetch("teq_novak_2021_selfesteem")
irw.table<-irwpkg::irw_long2resp(df)
irw.table$id<-NULL
data<-as.matrix(irw.table)

data<-linresp
################################################################################
##IMV for polytomous response over 1 fold
##nested versus linear

##nested
nesmap <- cbind(c(0, 0, 1, 1), c(0, 1, NA, NA), c(NA, NA, 0, 1))
x <- dendrify(data, nesmap)
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
x <- dendrify(data, linmap)
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
##
x.lin<-merge(gr,x.lin)
x.nest<-merge(gr,x.nest)

om<-numeric()
for (fold in 1:4) {
    print(fold)
    ## The unidimensional model for linear response trees
    m.lin <- glmer(resp ~ 0 + item:node + (0+node | id), 
                   family = binomial, data = x.lin[x.lin$gr!=fold,])
    ## The unidimensional model for linear response trees
    m.nest <- glmer(resp ~ 0 + item:node + (0+node | id), 
                    family = binomial, data = x.nest[x.nest$gr!=fold,])

    ##add nodes & compute category probs
    ##linear
    oos<-x.lin[x.lin$gr==fold,]
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

    ##nested
    oos<-x.nest[x.nest$gr==fold,]
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

    ##get out-of-sample responses
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

    resp<-as.numeric(data) ##not quite right, should be purely out of sample
    pctt.tab <- c()
    for (i in 0:(length(unique(resp))-1)){ #I got lazy and construct `pctt.tab` using all responses (should be OOS)
        pctt.tab <- c(pctt.tab, sum(resp == i)/length(resp))
    }

    om[fold]<-imv_c(y,pctt.tab,p1='mlin.',p2='mnest.')
}
summary(om)


##hm?
## > summary(om)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.0085 -0.0002  0.0027  0.0003  0.0032  0.0045 
