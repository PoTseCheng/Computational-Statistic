
atest<- t(data.matrix(a[2,]))
true_beta1<- c(0.8236, 1.4669, 1.0736, 1.4007, 3.457, 1.0732, 0.9210, 2.9302, 0.8705, 0.7729, 0.7322, 0.5342, 1.0163, 1.0387, 1.0751, 0.5763, 1.0252, 0.9612, 0.9598, 1.4969, 1.9247)
true_beta2<- c(1.4504, 0.9286, 0.6117, 1.051, 1.161, 1.9606, 1.8764, 1.0572, 0.8780, 0.8430, 0.7328, 0.618, 1.0331, 0.9644, 1.0658, 0.6495, 0.9781, 0.976, 0.979, 1.1397, 1.5924)
true_beta3<- c(0.5680, 1.5797, 1.7551, 1.3327, 2.9753, 0.5474, 0.4909, 2.7718, 0.9915, 0.9169, 0.9991, 0.8646, 0.9838, 1.077, 1.0087, 0.8873, 1.0482, 0.9848, 0.9803, 1.3134, 1.2086)

Truedata<- function (samplesize, dataframe){
  #we will generate the probability according to the papers result, then construct 
  #dummy variables to categorise the end effect
  
  true_beta1<- c(0.8236, 1.4669, 1.0736, 1.4007, 3.457, 1.0732, 0.9210, 2.9302, 0.8705, 0.7729, 0.7322, 0.5342, 1.0163, 1.0387, 1.0751, 0.5763, 1.0252, 0.9612, 0.9598, 1.4969, 1.9247)
  #true_beta 1 is for generation of probability comparing graduated with not graduated
  
  true_beta2<- c(1.4504, 0.9286, 0.6117, 1.051, 1.161, 1.9606, 1.8764, 1.0572, 0.8780, 0.8430, 0.7328, 0.618, 1.0331, 0.9644, 1.0658, 0.6495, 0.9781, 0.976, 0.979, 1.1397, 1.5924)
  #true_beta 2 is for generation of probability comparing graduated with graduated(transferred)
  
  true_beta3<- c(0.5680, 1.5797, 1.7551, 1.3327, 2.9753, 0.5474, 0.4909, 2.7718, 0.9915, 0.9169, 0.9991, 0.8646, 0.9838, 1.077, 1.0087, 0.8873, 1.0482, 0.9848, 0.9803, 1.3134, 1.2086)
  #true_beta 3 is for generation of probability comparing graduated with graduated(transferred)
  #short notice: the beta correspond to how I generate the raw data, so sequence is different with the original paper
  
  for (obs in 1:samplesize){
    
    temp1<- t(data.matrix(dataframe[obs,]))
    <- true_beta1%*%temp1-136.4
    
    
    135.24
    -138.11
    
  }
  #plug every observation in to get 
  
  #restore the probability
  
  #construct dummy variable by comparing which probability is highest (then assign it 1)
  #with three catagories: Graduate(Original), Graduate(Transfer), Not_Graduate
  
  
  
  
  return(some random shit)
}

#the use dplyr to split the data into training and testing


missinggen<- function (data){
  
  
  
  
  
  
  
  
}

