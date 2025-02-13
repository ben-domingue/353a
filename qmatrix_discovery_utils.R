##utility.R

## Fix singularity by adding random Gaussian noise
## 'index' are the rows/columns affected
fix.sing <- function(m, epsilon.div=1000) {
  if(any(rowSums(m) == 0) || any(colSums(m) == 0))
    warning('fix.sing: warning: NULL columns or rows in matrix')
  return(m + rmat(nrow(m), ncol(m)) / mean(abs(m), na.rm=T) / epsilon.div)
}

## Create a random matrix
rmat <- function(m, n=NULL) {
  if(is.null(n)) n=m
  matrix(runif(m*n),m,n)
}

#ɾ??Сֵ?ʹ?ֵ
del.zeros <- function(m0, m1=NULL) {
  if(is.null(m1)) m1 <- m0
  m.rm <- is.na(match(rowMeans(m0),0:1)) # NA if not 0 or 1
  m.cm <- is.na(match(colMeans(m0),0:1)) # NA if not 0 or 1
  # m.rm <- is.na(match(rowMeans(m0),0)) # NA if not 0
  # m.cm <- is.na(match(colMeans(m0),0)) # NA if not 0
  m2 <- m1[m.rm,]
  m3 <- m2[,m.cm]
  print(paste(c('dim before and after: ', dim(m1), dim(m3)), collapse=' '))
  return(m3)
}

#####################################################################
########?Էֽ??õ???W????????????????ɢ???????????��Ƶ?Q????
###############################################################
discreQ_fun<-function(q.matrix,W){
  #????Q??????ֵ?ͷֽ??õ??Ļ?????W֮???????ؾ??󣬲?????ϵ???ɴ???С??W????????????????
  Q.col<- apply(cor(q.matrix,W),1,which.max) 
  if(any(duplicated(Q.col))) { Q.col<-re.order(W,q.matrix)[,1]}
  Q.byorder<-W[,Q.col]
  #####??ֵ????ɢ???????ݸ???????ֵ1ֱ?ӽ??бȽ??ж?
  Q.d<-Q_thres(Q.byorder)    #??ɢ??
  return(Q.d)
}
######################################################################
##??Q????????????????????m2??????ƥ????????????m ,???ڶԷֽ??õ???Q????̰???㷨????
###########################################################################
re.order <- function(m,m2) {
  if(!all(dim(m) == dim(m2))) stop('m and m2 dimensions must be equal')
  mm2.cor <- t(cor(m,m2))
  cor.col <- max.col(mm2.cor)
  if(!any(duplicated(cor.col)))
    return(m[,cor.col])
  ## else we have to work harder...
  m.min <- min(mm2.cor) - 1
  r <- NULL
  for(i in 1:ncol(mm2.cor)) {
    cor.max <- which.max(mm2.cor)
    i.row <- row(mm2.cor)[cor.max]
    i.col <- col(mm2.cor)[cor.max]
    r <- rbind(r, c(i.col, i.row))
    mm2.cor[i.row,] <- m.min
    mm2.cor[,i.col] <- m.min
  }
  return(r)
}
####??ԭʼ????ֱ?ӽ????жϣ?????????1??????ֵ1??????Ϊ0
Q_thres<-function(W){
  J<-nrow(W)
  K<-ncol(W)
  Q_c<-matrix(0,J,K)
  for (j in 1:J) {
    for (k in 1:K) {
      if(W[j,k]>=1) Q_c[j,k]<-1 else Q_c[j,k]<-0
    }
  }
  return(Q_c)
}

################################
compute_rss_fun<-function(R,knum){
  #knum<-JJ
  Rt<-del.zeros(R)
  res<-nmf(t(Rt),seq(1,knum),method = 'snmf/l',.opt='vp5')  #
  onerss<-res$measures$rss
  return(onerss)
}



getq<-function(resp0,Kmax=5) {
    ## determine K
    R <- resp0
    J<-ncol(R)
    N<-nrow(R)
    JJ<-(J+1)/2+1
    if (JJ>Kmax) JJ<-Kmax
    rss<-compute_rss_fun(R,JJ)  
    ##calculate the st based on rss########################
    st<-rep(0,JJ)
    f<-rss
    ep<-0.5     #????ĸΪ0ʱ??????
    for (j in 2:(JJ-1)) {
        if((f[j]-f[j+1])==0){
            st[j]<-abs(f[j]-f[j-1])/ep
        }else{
            st[j]<-abs(f[j]-f[j-1])/abs(f[j+1]-f[j])
        }
    }
    K<-which.max(st)
    ## q_matrix identification
    R0<- resp0
    R<-del.zeros(R0)  
    ##estimate the Q-matrix used the SNMF method
    R.nmf<- nmf(t(!R),K,method = 'snmf/l') 
    R.W<-R.nmf@fit@W
    R.W_converted <- ifelse(R.W > 1, 1, 0)
    list(qmatrix=R.W_converted,factored=R.nmf)
}
