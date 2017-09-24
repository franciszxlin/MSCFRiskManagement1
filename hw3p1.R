# Homework 3 problem 1 
# Part (a)
# This is a function to generate a specified number of random variables with the Frechet distribution.
# Inputs: 
# n - size of the sample of random variables with the Frechet distribution 
# alp - the specified constant value of alpha parameter in the Frechet c.d.f. 
# Output: a vector containing the generated sample with the Frechet distribution
genFrechetDist<-function(n, alp)
{
  u<-runif(n) 
  f<-(log(1/u))^(-1/alp)
  return(f) 
}
hold_test<-genFrechetDist(1000,2) # I used choice of alpha = 2 parameter for the Frechet distribution
# Part (c)
# This is a function to generate a specified number of random variables with the Pareto distribution.
# Inputs:
# n - size of the sample of random variables with the Pareto distribution
# alp - the specified alpha parameter 
# c - the specified c parameter
# Outputs: a vector containing the generated sample with the specified Pareto distribution
genParetoDist<-function(n, alp, c)
{
  u<-runif(n)
  f<-c*(1-u)^(-1/alp)-c
  return(f)
}
N<-1000
n<-250
alpha<-2
c<-1
hold_scaled_max=numeric(N)
for (i in 1:N)
{
  hold_max<-max(genParetoDist(n,alpha,c))
  hold_scaled_max[i]=hold_max/(1000^(1/2))
}
length(hold_scaled_max)
head(hold_scaled_max)
plot(sort(hold_scaled_max), sort(hold_test))
lines(sort(hold_test),sort(hold_test))