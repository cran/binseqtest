designSimon<-function(p0,p1,alpha=.05,beta=.2,type=c("optimal","minimax")){
    # need clinfun R package
    # for notation and description see ?ph2simon after loading clinfun
    if (!require(clinfun)) stop("need to install the clinfun R package first")
    type<-match.arg(type)
    x<-ph2simon(p0,p1,alpha,beta)
    # see print.ph2simon
    xout <- x$out
    nmax <- x$nmax
    n <- nrow(xout)
    if (type=="optimal"){
        index <- ((1:n)[xout[, 5] == min(xout[, 5])])[1]
    } else index<-1
    out<-xout[index, ]
    B<-designAb(Nk=c(out[c("n1","n")]),a=c(out[c("r1","r")]),theta0=p0,
         conf.level=1-alpha,alternative="greater")
    B
}

B<-designSimon(.2,.4,.05,.2)
plotBoundEst(B)