monotone <-function(x,t,k){
  n=length(x)
  l=length(t)-2
  m=l+k
  I=matrix(0,nrow=m,ncol=n)
  dsigma=sigma
  tt<-c(rep(t[1],k-1),t,rep(t[length(t)],k-1))
  
  # for(j in 1:m){
  #   i1=x>=tt[j]&x<tt[j+1]
  #   if(j<m){
  #     I[(j+1):m,i1]=0
  #   }
  #   if((j-k)>=1){
  #     I[1:(j-k),i1]=1
  #   }
  #   tt<-c(rep(t[1],k),t,rep(t[length(t)],k))
  #   for(i in (j-k+1):j){
  #     for(mm in i:j){
  #       I[i,i1]=I[i,i1]+(tt[mm+k+1]-tt[mm])*Mspline(x[i1],(k+1),t)[mm,]/(k+1)
  #     }
  #   }
  # }
  jj<-c()
  #count=0
  for(i in 1:n){
    for(j in 1:m){
      if(x[i]>=tt[j]&x[i]<=tt[j+1]){
        jj[i]<-j
        #count=count+1
        #break
      }
    }
  }
  for(i in 1:n){
    if(jj[i]<m){
      I[(jj[i]+1):m,i]=0
    }
    if((jj[i]-k)>=1){
      I[1:(jj[i]-k),i]=1
    }
  }
  for(i in 1:n){
    tt<-c(rep(t[1],k),t,rep(t[length(t)],k))
    for(ii in (jj[i]-k+1):jj[i] ){
      #print(ii)
      for(mm in ii:jj[i]){
        I[ii,i]=I[ii,i]+(tt[mm+k+1]-tt[mm])*Mspline(x[i],(k+1),t)[mm,]/(k+1)
      }
    }
  }

    
  
   

  for(i in 1:m){
    rng=max(I[i,])
    I[i,]=I[i,]/rng
    #dsigma[i,]=dsigma[i,]/rng
  }
  
  ans=new.env()
  ans$sigma=I
  #ans$dsigma=dsigma
  ans
}