# Homework 3 Problem 3
# Make the Hill and POT plots for an empirical data and then estimate the corresponding tail indices
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
# A function to construct Hill plot
# Inputs: 
# dtvec = a vector of data to be used to construct Hill Plot
# cutoff = the cutoff order statistic of the data used to construct the plot 
hillPlot<-function(dtvec, cutoff)
{
  dtvec_ordered<-sort(dtvec, decreasing=TRUE)
  hori<-2:cutoff
  hill_est<-numeric(cutoff-1)
  for (i in 1:(cutoff-1))
  {
    hill_est[i]<-1/(mean(log(dtvec_ordered[1:i+1]))-log(dtvec_ordered[i+1]))
  }
  plot(hori, hill_est, ylim=c(0,5))
}
# A function to construct POT plot
# inputs: 
# dtvecr = a vector of data to be used to construct the POT plot
# cutoff = the cutoff order statistic of the data used to construct the plot
potPlot<-function(dtvec, cutoff)
{
  dtvec_ordered<-sort(dtvec, decreasing=FALSE)[1:cutoff]
  pot_est=numeric(length(dtvec_ordered))
  for (i in 1:cutoff)
  {
    pot_est[i]<-sum(max(0, dtvec_ordered-dtvec_ordered[i]))/sum(indicator(dtvec_ordered>dtvec_ordered[i]))
  }
  plot(dtvec_ordered,pot_est)
}
# Test: using IBM data to test functions to construct Hill and POT plots 
ibm<-read.csv(file='C:\\Users\\zil20\\Desktop\\Github Repos\\MSCFRiskManagement1\\wk3\\ibm091716.csv',header=TRUE)
ibm_ret<-ibm$return
hillPlot(ibm_ret,1000)
potPlot(ibm_ret,10000)

