# Homework 4 Problem 3
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
# Parameters
p1<-0.0030
p2<-0.0100
p3<-0.0500
n<-100000
N<-100
# Part a
lossVec<-numeric(n)
c1<-qnorm(p1)
c2<-qnorm(p2)
c3<-qnorm(p3)
for (i in 1:n) 
{
  z1<-rnorm(N)
  z2<-rnorm(N)
  z3<-rnorm(N)
  lossVec[i]<-sum(z1<c1)+sum(z2<c2)+sum(z3<c3)
}
hold1<-calcVarAndShortfallFromLossVector(lossVec, 0.99)
hold1
# Part b
targetx<-hold1[1,2]*1.2
# Standard Monte Carlo Simulation
lossVec<-numeric(n)
c1<-qnorm(p1)
c2<-qnorm(p2)
c3<-qnorm(p3)
for (i in 1:n) 
{
  z1<-rnorm(N)
  z2<-rnorm(N)
  z3<-rnorm(N)
  lossVec[i]<-sum(z1<c1)+sum(z2<c2)+sum(z3<c3)
}
mean_est<-mean(lossVec>targetx)
se_est<-sd(lossVec>targetx)/sqrt(n)
mean_est
se_est
# Twisted exponential importance sampling methodology
# Now we solve for optimal theta
library(NLRoot)
func<-function(x) {
  100*((p1*exp(x))/(1+p1*(exp(x)-1))+(p2*exp(x))/(1+p2*(exp(x)-1))+(p3*exp(x))/(1+p3*(exp(x)-1)))-targetx
}
BFfzero(func,0,1)
# The optimal theta is 0.9718353
theta<-0.9718353
# Apply exponential twisting
newp1<-(p1*exp(theta))/(1+p1*(exp(theta)-1))
newp2<-(p2*exp(theta))/(1+p2*(exp(theta)-1))
newp3<-(p3*exp(theta))/(1+p3*(exp(theta)-1))
newLossVec<-numeric(n)
newc1<-qnorm(newp1)
newc2<-qnorm(newp2)
newc3<-qnorm(newp3)
for (i in 1:n)
{
  z1<-rnorm(N)
  y1<-(z1<newc1)
  rn1<-exp(-theta*y1)*(1+p1*(exp(theta)-1))
  r1<-prod(rn1)
  
  z2<-rnorm(N)
  y2<-(z2<newc2)
  rn2<-exp(-theta*y2)*(1+p2*(exp(theta)-1))
  r2<-prod(rn2)
  
  z3<-rnorm(N)
  y3<-(z3<newc3)
  rn3<-exp(-theta*y3)*(1+p3*(exp(theta)-1))
  r3<-prod(rn3)
  
  loss<-sum(z1<newc1)+sum(z2<newc2)+sum(z3<newc3)
  y=(loss>targetx)
  newLossVec[i]<-y*r1*r2*r3
}
mean_est_new=mean(newLossVec)
se_est_new=sd(newLossVec)/sqrt(n)
mean_est_new
se_est_new

