# Problem 1 Practice on VaR versus Shortfall VaR
# Part (a)
# Normal distributed returns
calcVarAndShortfallNormal<-function(mu, sig, alp)
{
  var=mu+sig*qnorm(alp)
  sf=mu+sig/(1-alp)*dnorm(qnorm(alp))
  return(cbind(alp,var,sf))
}
notional1=10000
mean=0
daily_sig=0.2/sqrt(250)
u=notional1*mean
sigma=notional1*daily_sig
alphas=c(0.90,0.95,0.975,0.99,0.995)
calcVarAndShortfall(u,sigma,alphas)
# t-4 distributed
calcVarAndShortfallT<-function(mu, sig, alp)
{
  var=mu+sig*qt(alp,df=4)
  sf=mu+sig*dt(qt(alp,df=4),df=4)/(1-alp)*(4+qt(alp,df=4)^2)/3
  return(cbind(alp,var,sf))
}
notional1=10000
mean=0
daily_sig=0.2/sqrt(500)
u=notional1*mean
sigma=notional1*daily_sig
calcVarAndShortfallT(u,sigma,alphas)
# Part (b)
# Normal distributed returns 
num_stock=10
notional2=1000
mean=0
daily_sig=0.2/sqrt(250)
u=num_stock*notional2*mean
sig=sqrt(num_stock)*notional2*daily_sig
calcVarAndShortfallNormal(u,sig,alphas)
# t-4 distributed
calcVarAndShortfallFromLossVector<-function(loss_vec, alp)
{
  loss_ordered=sort(loss_vec)
  n=length(loss_vec)
  var_pick=ceiling(n*alp)
  var=loss_ordered[var_pick]
  sf=mean(loss_ordered[var_pick:n])
  return(cbind(alp, var, sf))
}
N=10000
notional=1000
num_stock=10
mean=0
daily_sig=0.2/sqrt(500)
u=mean*notional
sig=daily_sig*notional
loss=numeric(N)
for (i in 1:N) 
{
  loss[i]=sum(u+sig*rt(num_stock,df=4))
}
for (i in 1:length(alphas))
{
  print(calcVarAndShortfallFromLossVector(loss, alphas[i]))
}
# Part (c)
# Normal distributed returns
delt=10
num_stock=10
notional2=1000
mean=0
daily_sig=0.2/sqrt(250)
u=mean*notional2
sig=daily_sig*notional2
N=10000    # Number of trails
loss=numeric(N)
for (i in 1:N)
{
  loss[i]=sum(u*delt+sig*sqrt(delt)*rnorm(10))
}
calcVarAndShortfallFromLossVector(loss,alphas[4])
# t-4 distributed
delt=10
N=10000
notional3=1000
num_stock=10
mean=0
daily_sig=0.2/sqrt(500)
u=mean*notional3
sig=daily_sig*notional3    # sigma for a single stock
loss=numeric(N)
for (i in 1:N)
{
  oneStockLoss=numeric(num_stock)
  for (j in 1:num_stock)
  {
    tFor10Days=sum(rt(10,df=4))
    oneStockLoss[j]=u*delt+sig*tFor10Days
  }
  loss[i]=sum(oneStockLoss)
}
calcVarAndShortfallFromLossVector(loss,alphas[4])
