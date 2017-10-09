# Problem 2: The asymptotic distribution of the sample quantile
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
# Part (a)
N=1000
alpha=0.99
n1=100
n2=1000
n3=10000
# (1)
var_vec1=numeric(N)
for (i in 1:N)
{
  sample1=rnorm(n1)
  holdvar=calcVarAndShortfallFromLossVector(sample1, alpha)
  var_vec1[i]=holdvar[1,2]
}
mean(var_vec1)
sd(var_vec1)
var_mean=qnorm(alpha)
var_mean
var_sd=sqrt(alpha*(1-alpha)/n1)/dnorm(var_mean)
var_sd
qqnorm(var_vec1,main="Normal Probability Plot for n=100")
qqline(var_vec1)
# (2)
var_vec2=numeric(N)
for (i in 1:N)
{
  sample2=rnorm(n2)
  holdvar=calcVarAndShortfallFromLossVector(sample2, alpha)
  var_vec2[i]=holdvar[1,2]
}
mean(var_vec2)
sd(var_vec2)
var_mean=qnorm(alpha)
var_mean
var_sd=sqrt(alpha*(1-alpha)/n2)/dnorm(var_mean)
var_sd
qqnorm(var_vec2,main="Normal Probability Plot for n=1000")
qqline(var_vec2)
#(3)
var_vec3=numeric(N)
for (i in 1:N)
{
  sample3=rnorm(n3)
  holdvar=calcVarAndShortfallFromLossVector(sample3, alpha)
  var_vec3[i]=holdvar[1,2]
}
mean(var_vec3)
sd(var_vec3)
var_mean=qnorm(alpha)
var_mean
var_sd=sqrt(alpha*(1-alpha)/n3)/dnorm(var_mean)
var_sd
qqnorm(var_vec3,main="Normal Probability Plot for n=10000")
qqline(var_vec3)

# Part (b)
N=1000
alpha=0.99
n1=100
n2=1000
n3=10000
v=5
# (1)
var_vec1=numeric(N)
for (i in 1:N)
{
  sample1=rt(n1,df=v)
  holdvar=calcVarAndShortfallFromLossVector(sample1, alpha)
  var_vec1[i]=holdvar[1,2]
}
mean(var_vec1)
sd(var_vec1)
var_mean=qt(alpha,df=v)
var_mean
var_sd=sqrt(alpha*(1-alpha)/n1)/dt(var_mean,df=v)
var_sd
qqnorm(var_vec1,main="Normal Probability Plot for n=100")
qqline(var_vec1)
# (2)
var_vec2=numeric(N)
for (i in 1:N)
{
  sample2=rt(n2,df=v)
  holdvar=calcVarAndShortfallFromLossVector(sample2, alpha)
  var_vec2[i]=holdvar[1,2]
}
mean(var_vec2)
sd(var_vec2)
var_mean=qt(alpha,df=v)
var_mean
var_sd=sqrt(alpha*(1-alpha)/n2)/dt(var_mean,df=v)
var_sd
qqnorm(var_vec2,main="Normal Probability Plot for n=1000")
qqline(var_vec2)
#(3)
var_vec3=numeric(N)
for (i in 1:N)
{
  sample3=rt(n3,df=v)
  holdvar=calcVarAndShortfallFromLossVector(sample3, alpha)
  var_vec3[i]=holdvar[1,2]
}
mean(var_vec3)
sd(var_vec3)
var_mean=qt(alpha,df=v)
var_mean
var_sd=sqrt(alpha*(1-alpha)/n3)/dt(var_mean,df=v)
var_sd
qqnorm(var_vec3,main="Normal Probability Plot for n=10000")
qqline(var_vec3)

# Part (c)
N=1000
alpha=0.99
n1=100
n2=1000
n3=10000
# (1)
shortfall_vec1=numeric(N)
for (i in 1:N)
{
  sample1=rnorm(n1)
  holdvar=calcVarAndShortfallFromLossVector(sample1, alpha)
  shortfall_vec1[i]=holdvar[1,3]
}
mean(shortfall_vec1)
sd(shortfall_vec1)
qqnorm(shortfall_vec1,main="Normal Probability Plot for n=100")
qqline(shortfall_vec1)
# (2)
shortfall_vec2=numeric(N)
for (i in 1:N)
{
  sample2=rnorm(n2)
  holdvar=calcVarAndShortfallFromLossVector(sample2, alpha)
  shortfall_vec2[i]=holdvar[1,3]
}
mean(shortfall_vec2)
sd(shortfall_vec2)
qqnorm(shortfall_vec2,main="Normal Probability Plot for n=1000")
qqline(shortfall_vec2)
#(3)
shortfall_vec3=numeric(N)
for (i in 1:N)
{
  sample3=rnorm(n3)
  holdvar=calcVarAndShortfallFromLossVector(sample3, alpha)
  shortfall_vec3[i]=holdvar[1,3]
}
mean(shortfall_vec3)
sd(shortfall_vec3)
qqnorm(shortfall_vec3,main="Normal Probability Plot for n=10000")
qqline(shortfall_vec3)

N=1000
alpha=0.99
n1=100
n2=1000
n3=10000
v=5
# (1)
shortfall_vec1=numeric(N)
for (i in 1:N)
{
  sample1=rt(n1,df=v)
  holdvar=calcVarAndShortfallFromLossVector(sample1, alpha)
  shortfall_vec1[i]=holdvar[1,3]
}
mean(shortfall_vec1)
sd(shortfall_vec1)
qqnorm(shortfall_vec1,main="Normal Probability Plot for n=100")
qqline(shortfall_vec1)
# (2)
shortfall_vec2=numeric(N)
for (i in 1:N)
{
  sample1=rt(n2,df=v)
  holdvar=calcVarAndShortfallFromLossVector(sample1, alpha)
  shortfall_vec2[i]=holdvar[1,3]
}
mean(shortfall_vec2)
sd(shortfall_vec2)
qqnorm(shortfall_vec2,main="Normal Probability Plot for n=1000")
qqline(shortfall_vec2)
# (3)
shortfall_vec3=numeric(N)
for (i in 1:N)
{
  sample1=rt(n3,df=v)
  holdvar=calcVarAndShortfallFromLossVector(sample1, alpha)
  shortfall_vec3[i]=holdvar[1,3]
}
mean(shortfall_vec3)
sd(shortfall_vec3)
qqnorm(shortfall_vec3,main="Normal Probability Plot for n=10000")
qqline(shortfall_vec3)
