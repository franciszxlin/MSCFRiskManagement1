# Homework 4 Problem 1

# Parameters
csprds<-c(0.83, 0.9, 1.2, 1.86, 3.47, 5.85, 13.21)/100
recovery<-0.4
rf<-0.04
maturity<-10

Q<-t(matrix(c(-.1154, .1019, .0083, .0020, .0031, 0, 0, 0,
              .0091, -.1043, .0787, .0105, .0030, .0030, 0, 0,
              .0010, .0309, -.1172, .0688, .0107, .0048, 0, .0010,
              .0007, .0047, .0713, -.1711, .0701, .0174, .0020, .0049,
              .0005, .0025, .0089, .0813, -.2530, .1181, .0144, .0273,
              0, .0021, .0034, .0073, .0568, -.1928, .0479, .0753,
              0,0,.0142, .0142, .0250, .0928, -.4318, .2856,
              0, 0, 0, 0, 0, 0, 0, 0), nrow = 8, ncol = 8))
M<-t(matrix(c(0,.8838, .0720, .0173, .0269, 0, 0, 0,
              .0872, 0, .7545, .1007, .0288, .0288, 0,0,
              .0085, .2637, 0, .5870, .0913, .0410, 0, .0085,
              .0041, .0275, .4167, 0, .4097, .1017, .0117, .0286,
              .0020, .0099, .0352, .3213, 0, .4668, .0569, .1079,
              0, .0109, .0176, .0379, .2946, 0, .2484, .3906,
              0, 0, .0329, .0329, .0579, .2149, 0, .6614,
              0,0,0,0,0,0,0,1), nrow = 8, ncol = 8))
hld_lam<-t(c(.1154, .1043, .1172, .1711, .2530, .1929, .4318, .0001))

# Construct Aliasing Algorithm table
r1 = M[1,]
r2 = M[2,]
r3 = M[3,]
r4 = M[4,]
r5 = M[5,]
r6 = M[6,]
r7 = M[7,]
r8 = M[8,]
states = 1:8
N=length(states)
NN=N-1
# Alias tables
T1= matrix(0, nrow = NN, ncol =3)
T2= matrix(0, nrow = NN, ncol =3)
T3= matrix(0, nrow = NN, ncol =3)
T4= matrix(0, nrow = NN, ncol =3)
T5= matrix(0, nrow = NN, ncol =3)
T6= matrix(0, nrow = NN, ncol =3)
T7= matrix(0, nrow = NN, ncol =3)
T8= matrix(0, nrow = NN, ncol =3)
for (j in 1:N) {
  if (j==1) {
    L = cbind(states, r1)
  } 
  else if (j==2) {
    L = cbind(states, r2)
  }
  else if (j==3) {
    L = cbind(states, r3)
  }
  else if (j==4) {
    L = cbind(states, r4)
  }
  else if (j==5) {
    L = cbind(states, r5)
  }
  else if (j==6) {
    L = cbind(states, r6)
  }
  else if (j==7) {
    L = cbind(states, r7)
  }
  else {
    L = cbind(states, r8)    
  }
  L[,2]=NN*L[,2]
  for (i in 1:NN) {
    L = L[order(L[,2], decreasing = FALSE),]
    if (j==1) {
      T1[i,1]=L[1,2]
      T1[i,2]=L[1,1]
      T1[i,3]=L[N+1-i,1]
    } 
    else if (j==2) {
      T2[i,1]=L[1,2]
      T2[i,2]=L[1,1]
      T2[i,3]=L[N+1-i,1]
    }
    else if (j==3) {
      T3[i,1]=L[1,2]
      T3[i,2]=L[1,1]
      T3[i,3]=L[N+1-i,1]
    }
    else if (j==4) {
      T4[i,1]=L[1,2]
      T4[i,2]=L[1,1]
      T4[i,3]=L[N+1-i,1]
    }
    else if (j==5) {
      T5[i,1]=L[1,2]
      T5[i,2]=L[1,1]
      T5[i,3]=L[N+1-i,1]
    }
    else if (j==6) {
      T6[i,1]=L[1,2]
      T6[i,2]=L[1,1]
      T6[i,3]=L[N+1-i,1]
    }
    else if (j==7) {
      T7[i,1]=L[1,2]
      T7[i,2]=L[1,1]
      T7[i,3]=L[N+1-i,1]
    }
    else {
      T8[i,1]=L[1,2]
      T8[i,2]=L[1,1]
      T8[i,3]=L[N+1-i,1]   
    }
    L[N+1-i,2]=L[N+1-i,2]-(1-L[1,2])
    L=L[-1,]
  }
}
# Giant Alias Table
AT=rbind(T1,T2,T3,T4,T5,T6,T7,T8)
                                                                
# part a
dflt_lam<-numeric(7)
dflt_lam<-csprds/(1-recovery)
dflt_lam

# part b
paths<-10000
dtimes<-matrix(0, nrow=NN, ncol=paths)
for (i in 1:NN) 
{
  for (j in 1:paths)
  {
    clock<-0
    cur_state<-i
    while (clock < maturity & cur_state != 8)
    {
      clock<-clock+(-1/hld_lam[cur_state])*log(runif(1))
      V=NN*runif(1)
      I=ceiling(V)
      W=I-V
      Y=(W<=AT[(cur_state-1)*NN+I, 1])
      YN=Y+0
      cur_state=AT[(cur_state-1)*NN+I, 2]*YN+AT[(cur_state-1)*NN+I, 3]*(1-YN)
    }
    dtimes[i,j]<-min(maturity,clock)
  }
}
# plot the density plots
for (i in 1:NN)
{
  hist(dtimes[i,], main=paste("Density Plot For Starting State ",i,sep=""),freq=FALSE,xlab=paste("min(t(",i, "), 10)", sep=""))
}

# part c
ddtimes<-matrix(0, nrow=NN, ncol=paths)
pv<-matrix<-matrix(0, nrow=NN, ncol=paths)
# main loop to find default time
for (i in 1:NN)
{
  for (j in 1:paths)
  {
    lt<-0
    rt<-0
    bound<-(-log(runif(1)))
    lc<-0
    rc<-0
    cur_state<-i
    while (rt<maturity & rc < bound)
    {
      time<-(-1/hld_lam[cur_state])*log(runif(1))
      lt<-rt
      rt<-lt+time
      lc<-rc
      rc<-lc+dflt_lam[cur_state]*time
      V=NN*runif(1)
      I=ceiling(V)
      W=I-V
      Y=(W<=AT[(cur_state-1)*NN+I, 1])
      YN=Y+0
      cur_state=AT[(cur_state-1)*NN+I, 2]*YN+AT[(cur_state-1)*NN+I, 3]*(1-YN)
      if (cur_state==8)
      {
        break
      }
    }
    
    if (cur_state == 8) 
    {
      ddtimes[i,j]<-min(maturity, rt)
    } else
    {
      ddtimes[i,j]<-min(maturity,lt+(bound-lc)*(rt-lt)/(rc-lc))
    }
  }
}
bondprc<-numeric(NN)
bondse<-numeric(NN)
for (i in 1:NN)
{
  for (j in 1:paths)
  {
    if (ddtimes[i,j]<maturity) 
    {
      pv[i,j]<-recovery*exp(-rf*ddtimes[i,j])
    } else {
      pv[i,j]<-exp(-rf*ddtimes[i,j])
    }
  }
  bondprc[i]=mean(pv[i,])
  bondse[i]=sd(pv[i,])/sqrt(paths)
}
bondprc
bondse



