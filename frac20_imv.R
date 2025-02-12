source("00funs.R") #https://github.com/AnyaWMa/IRW-Qmatrix/blob/main/bd/00funs.R
##you'll need to also install irwpkg & imv:
remotes::install_github("hansorlee/irwpkg")
devtools::install_github("ben-domingue/imv", ref="main")

#########################irw

f<-function(tab) {
    df$resp <- as.numeric(df$resp)  # Ensure the response variable is numeric
    resp<-irwpkg::irw_long2resp(df)
    resp$id<-NULL
    makeqm<-function(df,resp=NULL) { #resp required for correct ordering
        ii<-grep("Qmatrix__",names(df))
        L<-split(df,df$item)
        qm<-lapply(L,function(x,ii) colMeans(x[,ii],na.rm=TRUE),ii)
        qm<-do.call("rbind",qm)
        if (!is.null(resp)) {
            cn<-colnames(resp)
            qm<-qm[cn,]
        }
        qm
    }
    qm<-makeqm(df,resp=resp)
    list(resp=resp,qm=qm)
}

om<-list()
tables<-'frac20'#c("frac20","cdm_ecpe","cdm_hr","mcmi_mokken")
for (table in tables) {
    df<-irwpkg::irw_fetch(table)
    L<-f(df)
    om[[table]]<-oos.compare(L$resp,L$qm,5,modeltype="2PL")
}
unlist(om)
