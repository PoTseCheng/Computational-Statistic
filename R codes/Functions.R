library(truncnorm)

Rawdata<- function (samplesize, some shit here idk){
  #key idea about this process: We will be using the descriptive statistics to
  #to generate the result
  
  #below are for the dummy variables, which is randomly sampled, we will follow the
  #suggested mean but not the SD. The reason is because the SD is realated to the sample
  #size while the mean is not. In this experiment, the sample size is a controlable variable.
  D_F<- sample(c(0,1), size=samplesize, replace=TRUE, prob = c(1-0.543, 0.543))
  D_U<- sample(c(0,1), size=samplesize, replace=TRUE, prob = c(1-0.079, 0.079))
  D_A<- sample(c(0,1), size=samplesize, replace=TRUE, prob = c(1-0.11, 0.11))
  D_G<- sample(c(0,1), size=samplesize, replace=TRUE, prob = c(1-0.364, 0.364))
  D_19<- sample(c(0,1), size=samplesize, replace=TRUE, prob = c(1-0.019, 0.019))
  G_O<- sample(c(0,1), size=samplesize, replace=TRUE, prob = c(1-0.061, 0.061))
  G_R<- sample(c(0,1), size=samplesize, replace=TRUE, prob = c(1-0.248, 0.248))
  A_R<- sample(c(0,1), size=samplesize, replace=TRUE, prob = c(1-0.083, 0.083))
  A_F<- sample(c(0,1), size=samplesize, replace=TRUE, prob = c(1-0.704, 0.704))
  A_LC<- sample(c(0,1), size=samplesize, replace=TRUE, prob = c(1-0.622, 0.622))
  A_LL<- sample(c(0,1), size=samplesize, replace=TRUE, prob = c(1-0.139, 0.139))
  A_A<- sample(c(0,1), size=samplesize, replace=TRUE, prob = c(1-0.041, 0.041))
  
  #we will be using the truncnorm library to construct the variables
  Unmet<- rtruncnorm(samplesize, a=0, b=26.347, mean=2.038, sd=3.711)
  Needaid<- rtruncnorm(samplesize, a=0, b=9.186, mean=0.948, sd=1.620)
  Loanaid<- rtruncnorm(samplesize, a=0, b=12.792, mean=1.543, sd=2.234)
  Meritaid<- rtruncnorm(samplesize, a=0, b=6.818, mean=0.168, sd=0.538)
  ACT<- rtruncnorm(samplesize, a=11, b=35, mean=24.657, sd=4.189)
  AP<- rtruncnorm(samplesize, a=0, b=59, mean=3.153, sd=6.671)
  Course<- rtruncnorm(samplesize, a=0, b=100, mean=91.857, sd=20.288)
  Ccount<- rtruncnorm(samplesize, a=0, b=5, mean=0.691, sd=0.898)
  Dcount<- rtruncnorm(samplesize, a=0, b=4, mean=0.13, sd=0.387)
  
  
  

  }