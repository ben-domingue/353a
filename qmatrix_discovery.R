library(CDM)
library(NMF)
##get code from here: https://osf.io/vrjd5/?view_only=7be6c23b4cc0474181d7235b3803ed68
source('utility.R') 
source('compute_rss_fun.R')

df <- irwpkg::irw_fetch("frac20")

x0 <- df %>% 
  dplyr::arrange(item)
resp0<-data.frame(irw::long2resp(x0))
id<-resp0$id
resp0$id<-NULL

R<-resp0

getq<-function(R,Kmax=5) {
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
    time1=as.POSIXlt(Sys.time()) 
    R.nmf<- nmf(t(!R),K,method = 'snmf/l') 
    time2=as.POSIXlt(Sys.time())
    use.time=difftime(time2,time1,units="secs")
    R.W<-R.nmf@fit@W
    R.W_converted <- ifelse(R.W > 1, 1, 0)
    R.W_converted
}
getq(R)
