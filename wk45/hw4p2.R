# Homework 4 Problem 2
N<-100
V<-100
smp<-50000
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
# Function to calculate mean portfolio loss, standard deviation, VaR, and expected shortfall given a list of parameters
portLoss<-function(N, lossVal, smp_siz, rootRho, pd)
{
  c<-qnorm(pd)
  rootOneMinusRho<-sqrt(1-rootRho^2)
  portLossVec<-numeric(smp_siz)
  for (i in 1:smp_siz) 
  {
    z<-rnorm(1)
    zi<-rnorm(N)
    x<-rootRho*z+rootOneMinusRho*zi
    portLossVec[i]<-lossVal*sum(x<c)
  }
  meanLoss<-mean(portLossVec)
  sdLoss<-sd(portLossVec)
  holdVaR1<-calcVarAndShortfallFromLossVector(portLossVec,0.95)
  var1<-holdVaR1[1,2]
  holdVaR2<-calcVarAndShortfallFromLossVector(portLossVec,0.99)
  var2<-holdVaR2[1,2]
  holdVaR3<-calcVarAndShortfallFromLossVector(portLossVec,0.999)
  var3<-holdVaR3[1,2]
  return(c(meanLoss, sdLoss, var1, var2, var3))
}
# Part a
rootRho<-0.3
p1<-0.01/100
p2<-0.1/100
p3<-1/100
hold1<-portLoss(N, V, smp, rootRho, p1)
hold1
hold2<-portLoss(N, V, smp, rootRho, p2)
hold2
hold3<-portLoss(N, V, smp, rootRho, p3)
hold3

# Part b 
rootRho1<-0.6
rootRho2<-0.9
hold4<-portLoss(N, V, smp, rootRho1, p1)
hold4
hold5<-portLoss(N, V, smp, rootRho2, p1)
hold5

