EM<-function(dat,init=c(.5,.5),N=10) {
    Estep<-function(n,N,p1,p2) {
        p1^n*(1-p1)^(N-n)->z1
        p2^n*(1-p2)^(N-n)->z2
        z1/(z1+z2)->pr1
        z2/(z1+z2)->pr2
        c(pr1*n,pr1*(N-n),pr2*n,pr2*(N-n))
    }
    Vectorize(Estep,vectorize.args="n")->Estep
    Mstep<-function(mat) {
        rowSums(mat)->rs
        c(rs[1]/(rs[1]+rs[2]),rs[3]/(rs[3]+rs[4]))
    }
    ##
    init -> new.est
    c(10,10)->old.est
    counter<-1
    ##
    while (max(abs(old.est-new.est))>.001) {
        new.est->old.est
        Estep(dat,p1=new.est[1],p2=new.est[2],N=N)->mat
        Mstep(mat)->new.est
        counter<-counter+1
    }
    new.est
}
EM(dat=c(5,9,8,4,7),init=c(.52,.5)) #http://www.nature.com/nbt/journal/v26/n8/full/nbt1406.html
