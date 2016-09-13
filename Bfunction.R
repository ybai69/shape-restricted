Bspline<-function(x,k,t){
  n=length(x)
  l=length(t)-2
  t<-c(rep(t[1],k-1),t,rep(t[length(t)],k-1))
  m=l+k
  kk<-1
  B=matrix(0,nrow=length(t)-kk,ncol=n)
  for(j in 1:(length(t)-kk)){
    i1=x>=t[j]&x<t[j+1]
   B[j,i1]<-1
  }
  
  while(kk<k){
    kk<-kk+1
    for(j in 1:(length(t)-kk)){
      i2=x>=t[j]&x<t[j+kk]
      if(t[j+kk-1]-t[j]==0){
        temp1<-1
      }else{
        temp1<-(x[i2]-t[j])/(t[j+kk-1]-t[j])
      }
      if(t[j+kk]-t[j+1]==0){
        temp2<-1
      }else{
        temp2<-(t[j+kk]-x[i2])/(t[j+kk]-t[j+1])
      }
      B[j,i2]<-temp1*B[j,i2]+temp2*B[j+1,i2]
    }
    B<-as.matrix(B[-nrow(B),])
  }
  B[m,x==max(t)]<-1
  return(B)
}

