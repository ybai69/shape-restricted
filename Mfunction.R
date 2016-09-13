Mspline<-function(x,k,t){
  n=length(x)
  l=length(t)-2
  t<-c(rep(t[1],k-1),t,rep(t[length(t)],k-1))
  #l=length(t)-2*k
  m=l+k
  kk<-1
  M=matrix(0,nrow=length(t)-kk,ncol=n)
  for(j in 1:(length(t)-kk)){
    i1=x>=t[j]&x<t[j+1]
    M[j,i1]<-1/(t[j+1]-t[j])
  }
  while(kk<k){
    kk<-kk+1
    for(j in 1:(length(t)-kk)){
      i2=x>=t[j]&x<t[j+kk]
      M[j,i2]<-(kk*((x[i2]-t[j])*M[j,i2]+(t[j+kk]-x[i2])*M[j+1,i2]))/((kk-1)*(t[j+kk]-t[j]))
    }
    M<-as.matrix(M[-nrow(M),])
  }
  M[m,x==max(t)]<-k/(t[length(t)]-t[length(t)-k])
  return(M)
}
