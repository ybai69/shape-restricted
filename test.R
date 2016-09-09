n=length(x)
l=length(t)-k
m=l+k
for(j in 1:(l+1)){
  i1=x>=t[j]&x<=t[j+1]
  t(splineDesign(t,x,ord=1))
  t(splineDesign(c(rep(t[1],1),t,rep(t[length(t)],1)),x,ord=2))*2*6
  t(splineDesign(c(rep(t[1],2),t,rep(t[length(t)],2)),x,ord=3))*3*6
  }


x <- seq(0, 1, by = .01)
y <- exp(x) + rnorm(x, sd = .125)
br = c(10, 25, 100, 200, 400, 1000, 1e+10)
obs = 1:7
n<-length(x)
nk = min(obs[n <= br]) + add
t = 0:(nk - 1)/(nk - 1) * (max(x) - min(x)) + min(x)

Mspline(x,3,t)
Mspline(x[1],3,t)

x<-x[2]

