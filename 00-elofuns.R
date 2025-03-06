last.value<-function(x) {
    w<-which(!is.na(x))
    x<-x[w[length(w)]]
}
update<-function(th,b,resp,K=1) {
    p<-1/(1+exp(-(th-b)))
    th.new<-th+K*(resp-p)
    b.new<-b+K*(p-resp)
    c(th.new,b.new)
}
invlogit<-function(x) 1/(1+exp(-x))
rmse<-function(x) sqrt(mean(x^2))

est<-function(df,K) {
    id<-unique(df$id)
    item<-unique(df$item)
    ##initialize first value
    df<-df[order(df$t),]
    for (i in id) {
        ii<-which(i==df$id)
        df$th0[ii[1]]<-0
    }
    for (i in item) {
        ii<-which(i==df$item)
        df$b0[ii[1]]<-0
    }
    ##estimate
    for (i in 1:nrow(df)) {
        test<-is.na(df$b0[i]) & is.na(df$th0[i])
        if (test) {
            z<-df[df$id==df$id[i] & df$t<df$t[i],,drop=FALSE]
            th0<-z$th0[nrow(z)]
            z<-df[df$item==df$item[i] & df$t<df$t[i],,drop=FALSE]
            b0<-z$b0[nrow(z)]
            pars<-update(th0,b0,df$resp[i],K=K)
            df$th0[i]<-pars[1]
            df$b0[i]<-pars[2]
        } else {#also set single NA values to 0
            df$b0[i]<-0
            df$th0[i]<-0
        }
    }
    df
}
