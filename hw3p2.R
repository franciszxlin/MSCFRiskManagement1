# Homework 3 Problem 2
#Part (b) make Hill plot from 10,000 random variables from the t-4 distribution
N<-10000
degree<-4
t4_sample<-rt(N,df=degree)
t4_sample_ordered<-sort(t4_sample, decreasing=TRUE)
hori_axis<-2:300
hill_est<-numeric(300-1)
for (i in 1:(300-1))
{
  hill_est[i]<-1/(mean(log(t4_sample_ordered[1:i+1]))-log(t4_sample_ordered[i+1]))
}
plot(hori_axis, hill_est, ylim=c(0,5))
# Part (c) make POT plot from the same sample
# A generic indicator function
# Input: a vector of booleans 
# Output: a vector of (0,1): 0 is false; 1 is true
indicator<-function(bool)
{
  ind_vec=numeric(length(bool))
  for (i in 1:length(bool))
  {
    if (bool[i]==TRUE) 
    {
      ind_vec[i]=1
    } else 
    {
      ind_vec[i]=0
    }
  }
  return(ind_vec)
}
# Construct POT plot 
pot_sorted<-sort(t4_sample,decreasing=FALSE)[1:100]
pot_est=numeric(length(pot_sorted))
for (i in 1:100)
{
  pot_est[i]=sum(max(0,pot_sorted-pot_sorted[i]))/sum(indicator(pot_sorted>pot_sorted[i]))
}
plot(pot_sorted,pot_est,ylim=c(0,0.2))

