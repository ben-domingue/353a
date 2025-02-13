##based on ##code from here: https://osf.io/vrjd5/?view_only=7be6c23b4cc0474181d7235b3803ed68

library(CDM)
library(NMF)
source("qmatrix_discovery_utils.R")

##using simulated dataste from GDINA, see help, ?GDINA::sim30DINA
x<-GDINA::sim30DINA 
R<-x$simdat
out<-getq(R,Kmax=5) #Kmax is meant to keep things from running forever. a limit on number of skills it will consider (and also appropriate given how this data was simulated)
##note how difficult it is to assess compared to x$simQ
out$qmatrix
x$simQ
str(out$factored@fit) ##can you see the dimensions for W and H being what we would expect?

##empirical dataset
df <- irwpkg::irw_fetch("frac20")
x0 <- df %>% 
  dplyr::arrange(item)
resp0<-data.frame(irw::long2resp(x0))
id<-resp0$id
resp0$id<-NULL
R<-resp0
out<-getq(R,Kmax=10) 
out$qmatrix ##what do you think about this relative to q matrices we have discussed for frac20


