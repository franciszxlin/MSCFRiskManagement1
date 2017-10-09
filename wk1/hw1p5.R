# Problem 5 
ua=0.1
ub=0.2
vola=0.25
volb=0.2
cor=0.2
td=250
alpha=0.99
mu=c(ua/td,ub/td)
cov=matrix(c(vola^2,cor*vola*volb,cor*vola*volb,volb^2),nrow=2,byrow=TRUE)
cov=cov/td
# Before portfolio change
w=c(100,50)
var_mu=t(w)%*%mu
var_vol=sqrt(t(w)%*%cov%*%w)
varA=var_mu+var_vol*qnorm(alpha)
# After portfolio changce
w=c(50,100)
var_mu=t(w)%*%mu
var_vol=sqrt(t(w)%*%cov%*%w)
varB=var_mu+var_vol*qnorm(alpha)
varB-varA