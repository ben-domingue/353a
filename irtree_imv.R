library("irtrees")
library("lme4")
library(imv)


####################################
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
