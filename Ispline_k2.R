#the special case that k=2
monotone <-function(x,t)
{
  t<-c(t[1],t,t[length(t)])
  n=length(x)
  k=length(t)-4
  m=k+2
  sigma=matrix(1:m*n,nrow=m,ncol=n)
  dsigma=sigma
  for(j in 1:7){
    i1=x<=t[j]
    sigma[j,i1] = 0
    dsigma[j,i1] = 0
    i2=x>t[j]&x<=t[j+1]
    sigma[j,i2] = (x[i2]-t[j])^2 / (t[j+2]-t[j]) / (t[j+1]-t[j])
    dsigma[j,i2] = 2*(x[i2]-t[j]) / (t[j+2]-t[j]) / (t[j+1]-t[j])
    i3=x>t[j+1]&x<=t[j+2]
    sigma[j,i3] = 1-(x[i3]-t[j+2])^2/(t[j+2]-t[j+1])/(t[j+2]-t[j])
    dsigma[j,i3] = -2*(x[i3]-t[j+2])/(t[j+2]-t[j+1])/(t[j+2]-t[j])
    i4=x>t[j+2]
    sigma[j,i4]=1
    dsigma[j,i4]=0
  }
  
  for(i in 1:m){
    rng=max(sigma[i,])
    sigma[i,]=sigma[i,]/rng
    dsigma[i,]=dsigma[i,]/rng
  }

  ans=new.env()
  ans$sigma=sigma
  ans$dsigma=dsigma
  ans
}