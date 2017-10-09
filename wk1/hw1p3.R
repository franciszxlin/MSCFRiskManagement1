# Problem 3 Practice on Delta-Gamma Approximation for VaR
# Function to calculate VaR and Eexpected shortfall
calcVarAndShortfallFromLossVector<-function(loss_vec, alp)
{
  loss_ordered=sort(loss_vec)
  n=length(loss_vec)
  var_pick=ceiling(n*alp)
  var=loss_ordered[var_pick]
  sf=mean(loss_ordered[var_pick:n])
  return(cbind(alp, var, sf))
}
# Function to calculate Black-Scholes call price 
bsCall<-function(notion, s, k, vol, r, t)
{
  d1=(log(s/k)+t*(r+vol^2/2))/(vol*sqrt(t))
  d2=d1-vol*sqrt(t)
  c=s*pnorm(d1)-k*exp(-r*t)*pnorm(d2)
  return(notion*c)
}
# Function to calculate Black-Scholes put price
bsPut<-function(notion, s, k, vol, r, t)
{
  d1=(log(s/k)+t*(r+vol^2/2))/(vol*sqrt(t))
  d2=d1-vol*sqrt(t)
  p=k*exp(-r*t)*pnorm(-d2)-s*pnorm(-d1)
  return(notion*p)
}
# Function to calculate Black-Scholes call delta
bsCallDelta<-function(notion, s, k, vol, r, t)
{
  d1=(log(s/k)+t*(r+vol^2/2))/(vol*sqrt(t))
  delta=pnorm(d1)
  return(notion*delta)
}
# Function to calculate Black-Scholes put delta
bsPutDelta<-function(notion, s, k, vol, r, t)
{
  d1=(log(s/k)+t*(r+vol^2/2))/(vol*sqrt(t))
  delta=pnorm(d1)-1
  return(notion*delta)
}
# Function to calculate Black-Scholes gamma
bsGamma<-function(notion, s, k, vol, r, t)
{
  d1=(log(s/k)+t*(r+vol^2/2))/(vol*sqrt(t))
  gamma=(1/(s*vol*sqrt(t)))*(1/sqrt(2*pi))*exp(-d1^2/2)
  return(notion*gamma)
}
# Function to calculate Black-Scholes call theta
bsCallTheta<-function(notion, s, k, vol, r, t)
{
  d1=(log(s/k)+t*(r+vol^2/2))/(vol*sqrt(t))
  d2=d1-vol*sqrt(t)
  theta=(1/250)*(-((s*vol/(2*sqrt(t)))*(1/sqrt(2*pi))*exp(-d1^2/2))-r*k*exp(-r*t)*pnorm(d2))
  return(notion*theta)
}
# Function to calculate Black-Scholes put theta
bsPutTheta<-function(notion, s, k, vol, r, t)
{
  d1=(log(s/k)+t*(r+vol^2/2))/(vol*sqrt(t))
  d2=d1-vol*sqrt(t)
  theta=(1/250)*(-((s*vol/(2*sqrt(t)))*(1/sqrt(2*pi))*exp(-d1^2/2))+r*k*exp(-r*t)*pnorm(-d2))
  return(notion*theta)  
}
# Simulation
N=1000
callNotion=-10
putNotion=-5
k=100
sig=0.40
r=0.05
t0=0.10
delt=0.04
t1=t0-delt
actualLoss=numeric(N)
deltaGammaLoss=numeric(N)
for (i in 1:N) 
{
  s0=matrix(100,nrow=10,ncol=1)
  s1=s0*exp((r-sig^2/2)*delt+sig*sqrt(delt)*rnorm(10))
  v0=bsCall(callNotion,s0,k,sig,r,t0)+bsPut(putNotion,s0,k,sig,r,t0)
  v1=bsCall(callNotion,s1,k,sig,r,t1)+bsPut(putNotion,s1,k,sig,r,t1)  
  actualLoss[i]=sum(v0-v1)
  theta=bsCallTheta(callNotion,s0,k,sig,r,t0)+bsPutTheta(putNotion,s0,k,sig,r,t0)
  delta=bsCallDelta(callNotion,s0,k,sig,r,t0)+bsPutDelta(putNotion,s0,k,sig,r,t0)
  gamma=bsGamma(callNotion,s0,k,sig,r,t0)+bsGamma(putNotion,s0,k,sig,r,t0)
  deltaGammaLoss[i]=sum(-theta*delt-delta*(s1-s0)-0.5*gamma*(s1-s0)^2)
}
plot(actualLoss,deltaGammaLoss,main="Delta-Gamma Approximation",xlab="Actual portfolio Loss",ylab="Delta-Gamma Approximation")
# Estimate 99% VaR from the empirical sample quantile
alpha=0.99
calcVarAndShortfallFromLossVector(actualLoss,alpha)
calcVarAndShortfallFromLossVector(deltaGammaLoss,alpha)