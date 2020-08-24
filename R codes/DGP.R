#PackAges for DGP
#for producing a 

#for age generation
library(wakefield)
library(truncnorm)



#DGP 1
Observations<- function (n){

#establish group difference
p1<- 46/262
n1<- round(n*p1)
p2<- 216/262
n2<- round(n*p2)

#group A (severe)#
Severity<- rep(1, n1)

#Age generation

n11<- age(round(n1*(1/46)), x= 1:12)
n12<- age(round(n1*(9/46)), x= 13:44)
n13<- age(round(n1*(16/46)), x= 45:66)
n14<- age(round(n1*(20/46)), x= 65:94)
test <- round(n1*(1/46))+round(n1*(9/46))+ round(n1*(16/46))+ round(n1*(20/46))

if (test< n1){
  addmiss<- age(1, x= 45:66)
  n13<- c(n13, addmiss)
  
  
} else if (test > n1){
  n13 <- n13[-sample(1:length(n13), 1)]
  
} 
Age<- c(n11, n12, n13, n14)

#Gender
temp0<- sample(c(1, 0), size=n1, replace=TRUE, prob = c(26/46, 20/46))
Gender<- factor(temp0, levels = c(1,0), labels = c("Male", "Female"))
#living Address
temp<- sample(c(0, 1, 2), size=n1, replace=TRUE, prob = c(39/46, 5/46, 2/46))
Address<- factor(temp, levels = c(0,1,2), labels = c("Beijin", "Wuhan", "Other"))

#Temperature

n31<- runif(round(n1*(9/46)), 36.5, 37.2)
n32<- runif(round(n1*(16/46)), 37.3, 38.0)
n33<- runif(round(n1*(20/46)), 38.1, 39.0)
n34<- runif(round(n1*(1/46)), 39.1, 41)

test3 <- round(n1*(9/46))+round(n1*(16/46)) + round(n1*(20/46)) + round(n1*(1/46))

if (test3< n1){
  addmiss<- sample(37.3:38.0, size = 1)
  n32<- c(n32, addmiss)
  
  
} else if (test3 > n1){
  n32 <- n32[-sample(1:length(n32), 1)]
  
} 


rawTemperature<- c(n31, n32, n33, n34)
Temperature<- vector()
for (i in rawTemperature){
  Temperature<- c(Temperature, round(i, 2))
  
  
}

Tlabel<- vector()
for (i in Temperature){
  if (i <= 37.5){
    Tlabel<- c(Tlabel, "Normal")
  } else if (i>37.5& i <=40){
    Tlabel<- c(Tlabel, "Fever")
  } else if (i>40){
    Tlabel<- c(Tlabel, "Hyperpyrexia")
  }
  
  
}
#Wuhan Contacts
temp3<- sample(c(0, 1, 2), size=n1, replace=TRUE, prob = c(13/46, 26/46, 7/46))
Contact<- factor(temp3, levels = c(0, 1, 2), labels = c("Wuhan 14 days", "Case 14 days", "Unknown"))
#Symptoms
cough<- sample(c(1, 0), size=n1, replace=TRUE, prob= c(25/46, 21/46))
fatigue<- sample(c(1, 0), size=n1, replace=TRUE, prob= c(15/46, 31/46))
Dyspnea<- sample(c(1, 0), size=n1, replace=TRUE, prob= c(15/46, 31/46))
Headache<- sample(c(1, 0), size=n1, replace=TRUE, prob= c(3/46, 43/46))
all<- data.frame(cough, fatigue, Dyspnea, Headache)
Symptoms<- vector()

for (i in 1:nrow(all))
{
  if (all[i,1]==1&all[i,2]==1&all[i,3]==1&all[i,4]==1){
    Symptoms<-c(Symptoms, "Coughing, Fatigue, Dyspnea, Headache")
  }else if (all[i,1]==1&all[i,2]==1&all[i,3]==1&all[i,4]==0){
    Symptoms<-c(Symptoms, "Coughing, Fatigue, Dyspnea")
  }else if (all[i,1]==1&all[i,2]==1&all[i,3]==0&all[i,4]==0){
    Symptoms<-c(Symptoms, "Coughing, Fatigue")
  }else if (all[i,1]==1&all[i,2]==0&all[i,3]==0&all[i,4]==0){
    Symptoms<-c(Symptoms, "Coughing")
  }else if (all[i,1]==0&all[i,2]==1&all[i,3]==1&all[i,4]==1){
    Symptoms<-c(Symptoms, "Fatigue, Dyspnea, Headache")
  }else if (all[i,1]==0&all[i,2]==1&all[i,3]==1&all[i,4]==0){
    Symptoms<-c(Symptoms, "Fatigue, Dyspnea")
  }else if (all[i,1]==0&all[i,2]==1&all[i,3]==0&all[i,4]==1){
    Symptoms<-c(Symptoms, "Fatigue, Headache")
  }else if (all[i,1]==0&all[i,2]==1&all[i,3]==0&all[i,4]==0){
    Symptoms<-c(Symptoms, "Fatigue")
  }else if (all[i,1]==1&all[i,2]==1&all[i,3]==0&all[i,4]==1){
    Symptoms<-c(Symptoms, "Coughing, Fatigue, Headache")
  }else if (all[i,1]==1&all[i,2]==0&all[i,3]==1&all[i,4]==0){
    Symptoms<-c(Symptoms, "Coughing, Dyspnea")
  }else if (all[i,1]==1&all[i,2]==0&all[i,3]==1&all[i,4]==1){
    Symptoms<-c(Symptoms, "Coughing, Dyspnea, Headache")
  }else if (all[i,1]==1&all[i,2]==0&all[i,3]==0&all[i,4]==1){
    Symptoms<-c(Symptoms, "Coughing, Headache")
  }else if (all[i,1]==0&all[i,2]==0&all[i,3]==1&all[i,4]==1){
    Symptoms<-c(Symptoms, "Dyspnea, Headache")
  }else if (all[i,1]==0&all[i,2]==0&all[i,3]==1&all[i,4]==0){
    Symptoms<-c(Symptoms, "Dyspnea")
  }else if (all[i,1]==0&all[i,2]==0&all[i,3]==0&all[i,4]==1){
    Symptoms<-c(Symptoms, "Headache")
  }else if (all[i,1]==0&all[i,2]==0&all[i,3]==0&all[i,4]==0){
    Symptoms<-c(Symptoms, "No Symptoms")
  }
  
}
#Result
temp4<- sample(c(0, 1, 2), size=n1, replace=TRUE, prob = c(2/46, 41/46, 3/46))
Result<- factor(temp4, levels = c(0, 1, 2), labels = c("Discharge", "Hospitalization", "Death"))

groupa<- data.frame(Severity, Gender, Age, Address, Contact, Temperature, Tlabel, Symptoms, Result)

#group B

Severity<- rep(0, n2)


#Age generation

n21<- age(round(n2*(10/216)), x= 1:12)
n22<- age(round(n2*(103/216)), x= 13:44)
n23<- age(round(n2*(75/216)), x= 45:66)
n24<- age(round(n2*(28/216)), x= 65:94)
test <- round(n2*(10/216))+round(n2*(103/216))+ round(n2*(75/216))+ round(n2*(28/216))

if (test< n2){
  addmiss<- age(1, x= 45:66)
  n23<- c(n23, addmiss)
  
  
} else if (test > n2){
  n23 <- n23[-sample(1:length(n23), 1)]
  
} 
Age<- c(n21, n22, n23, n24)

#Gender
temp0<- sample(c(1,0), size=n2, replace=TRUE, prob = c(101/216, 115/216))
Gender<-factor(temp0, levels = c(1,0), labels = c("Male", "Female"))
#living Address
temp<- sample(c(0, 1, 2), size=n2, replace=TRUE, prob = c(153/216, 48/216, 15/216))
Address<- factor(temp, levels = c(0,1,2), labels = c("Beijin", "Wuhan", "Other"))

#Temperature


n41<- runif(round(n2*(38/216)), 36.5, 37.2)
n42<- runif(round(n2*(94/216)), 37.3, 38.0) 
n43<- runif(round(n2*(76/216)), 38.1, 39.0) 
n44<- runif(round(n2*(8/216)), 39.1, 41) 

test4 <- round(n2*(38/216))+round(n2*(94/216)) + round(n2*(76/216)) + round(n2*(8/216))

if (test4< n2){
  addmiss<- sample(37.3:38.0, size = 1)
  n42<- c(n42, addmiss)
  
  
} else if (test4 > n2){
  n42 <- n42[-sample(1:length(n42), 1)]
  
} 
Temperature<- c(n41, n42, n43, n44)

rawTemperature<- c(n41, n42, n43, n44)
Temperature<- vector()
for (i in rawTemperature){
  Temperature<- c(Temperature, round(i, 2))
  
  
}

Tlabel<- vector()
for (i in Temperature){
  if (i <= 37.5){
    Tlabel<- c(Tlabel, "Normal")
  } else if (i>37.5& i <=40){
    Tlabel<- c(Tlabel, "Fever")
  } else if (i>40){
    Tlabel<- c(Tlabel, "Hyperpyrexia")
  }
  
  
}

#Wuhan Contacts
temp3<- sample(c(0, 1, 2), size=n2, replace=TRUE, prob = c(93/216, 103/216, 20/216))
Contact<- factor(temp3, levels = c(0, 1, 2), labels = c("Wuhan 14 days", "Case 14 days", "Unknown"))
#Symptoms

cough<- sample(c(1, 0), size=n2, replace=TRUE, prob= c(95/216, 121/216))
fatigue<- sample(c(1, 0), size=n2, replace=TRUE, prob= c(54/216, 162/216))
Dyspnea<- sample(c(1, 0), size=n2, replace=TRUE, prob= c(3/216, 213/216))
Headache<- sample(c(1, 0), size=n2, replace=TRUE, prob= c(14/216, 202/216))
all<- data.frame(cough, fatigue, Dyspnea, Headache)

Symptoms<- vector()

for (i in 1:nrow(all))
{
  if (all[i,1]==1&all[i,2]==1&all[i,3]==1&all[i,4]==1){
    Symptoms<-c(Symptoms, "Coughing, Fatigue, Dyspnea, Headache")
  }else if (all[i,1]==1&all[i,2]==1&all[i,3]==1&all[i,4]==0){
    Symptoms<-c(Symptoms, "Coughing, Fatigue, Dyspnea")
  }else if (all[i,1]==1&all[i,2]==1&all[i,3]==0&all[i,4]==0){
    Symptoms<-c(Symptoms, "Coughing, Fatigue")
  }else if (all[i,1]==1&all[i,2]==0&all[i,3]==0&all[i,4]==0){
    Symptoms<-c(Symptoms, "Coughing")
  }else if (all[i,1]==0&all[i,2]==1&all[i,3]==1&all[i,4]==1){
    Symptoms<-c(Symptoms, "Fatigue, Dyspnea, Headache")
  }else if (all[i,1]==0&all[i,2]==1&all[i,3]==1&all[i,4]==0){
    Symptoms<-c(Symptoms, "Fatigue, Dyspnea")
  }else if (all[i,1]==0&all[i,2]==1&all[i,3]==0&all[i,4]==1){
    Symptoms<-c(Symptoms, "Fatigue, Headache")
  }else if (all[i,1]==0&all[i,2]==1&all[i,3]==0&all[i,4]==0){
    Symptoms<-c(Symptoms, "Fatigue")
  }else if (all[i,1]==1&all[i,2]==1&all[i,3]==0&all[i,4]==1){
    Symptoms<-c(Symptoms, "Coughing, Fatigue, Headache")
  }else if (all[i,1]==1&all[i,2]==0&all[i,3]==1&all[i,4]==0){
    Symptoms<-c(Symptoms, "Coughing, Dyspnea")
  }else if (all[i,1]==1&all[i,2]==0&all[i,3]==1&all[i,4]==1){
    Symptoms<-c(Symptoms, "Coughing, Dyspnea, Headache")
  }else if (all[i,1]==1&all[i,2]==0&all[i,3]==0&all[i,4]==1){
    Symptoms<-c(Symptoms, "Coughing, Headache")
  }else if (all[i,1]==0&all[i,2]==0&all[i,3]==1&all[i,4]==1){
    Symptoms<-c(Symptoms, "Dyspnea, Headache")
  }else if (all[i,1]==0&all[i,2]==0&all[i,3]==1&all[i,4]==0){
    Symptoms<-c(Symptoms, "Dyspnea")
  }else if (all[i,1]==0&all[i,2]==0&all[i,3]==0&all[i,4]==1){
    Symptoms<-c(Symptoms, "Headache")
  }else if (all[i,1]==0&all[i,2]==0&all[i,3]==0&all[i,4]==0){
    Symptoms<-c(Symptoms, "No Other Symptoms")
  }
  
}

#Result
temp4<- sample(c(0, 1, 2), size=n2, replace=TRUE, prob = c(2/216, 41/216, 3/216))
Result<- factor(temp4, levels = c(0, 1, 2), labels = c("Discharge", "Hospitalization", "Death"))

groupb<- data.frame(Severity, Gender, Age, Address, Contact, Temperature, Tlabel, Symptoms, Result)


total <- rbind(groupa, groupb)
return (total)
}


#DGP from the paper2

Observations2<- function (n){
  #maybe can expand this function in the future for more testing(for instance adding 0 function to test certain algorithm)
  
  
  #key idea about this process: We will be using the descriptive statistics to
  #to generate the Result
  
  #below are for the dummy variables, which is generated according to the paper, we will follow the
  #suggested mean but not the SD. The reason is because the SD is realated to the sample
  #size while the mean is not. In this experiment, the sample size is a controlable variable.
  D_F<- sample(c(0,1), size=n, replace=TRUE, prob = c(0.457, 0.543))
  Gender<- factor(D_F, levels = c(0,1), labels = c("Female", "Male"))
  D_R<- sample(c(0,1,2), size=n, replace=TRUE, prob = c(0.079, 0.11, 0.811))
  Race<- factor(D_R, levels = c(0, 1, 2), labels = c("Minorities", "Asian", "White"))
  D_G<- sample(c(0,1), size=n, replace=TRUE, prob = c(0.636, 0.364))
  Generation<- factor(D_G, levels = c(0, 1), labels = c("No", "Yes"))
  D_19<- sample(c(0,1), size=n, replace=TRUE, prob = c(0.981, 0.019))
  Status<- factor(D_19, levels = c(0, 1), labels = c("Traditional", "Nontraditional"))
  
  
  G_O<- sample(c(0,1,2), size=n, replace=TRUE, prob = c(0.061, 0.248, 0.691))
  Geographic<- factor(G_O, levels = c(0, 1, 2), labels = c("Out-of-state", "Reciprocity state", "In-state"))
  
  A_R<- sample(c(0,1), size=n, replace=TRUE, prob = c(0.917, 0.083))
  Remedy<- factor(A_R, levels = c(0, 1), labels = c("No", "Yes"))
  
  A_F<- sample(c(0,1), size=n, replace=TRUE, prob = c(1-0.704, 0.704))
  Choice<- factor(A_F, levels = c(0, 1), labels = c("Others", "First Choice"))
  A_LC<- sample(c(0,1), size=n, replace=TRUE, prob = c(1-0.622, 0.622))
  Living<- factor(A_LC, levels = c(0, 1), labels = c("Outside Campus", "On Campus"))
  A_LL<- sample(c(0,1), size=n, replace=TRUE, prob = c(1-0.139, 0.139))
  Community<- factor(A_LL, levels = c(0, 1), labels = c("No", "Yes"))
  A_A<- sample(c(0,1), size=n, replace=TRUE, prob = c(0.959, 0.041))
  Athlete<- factor(A_A, levels = c(0, 1), labels = c("No", "Yes"))
  
  
  #Out of three financial Aid, one could only choose one
  finraw<- sample(c(0,1,2,3), size=n, replace=TRUE, prob = c(0.03, 0.39, 0.46, 0.12))
  Aidtype<- factor(finraw, levels = c(0, 1, 2, 3), labels = c("None", "Need Aid", "Loan Aid", "Merit Aid"))
  
  #get the Sum of True for each aid
  a1<- sum(finraw == 1)
  a2<- sum(finraw == 2)
  a3<- sum(finraw == 3)
  
  #implement aid
  rawNeedaid<- rtruncnorm(a1, a=0, b=9.186, mean=0.948, sd=1.620)
  
  
  rawLoanaid<- rtruncnorm(a2, a=0, b=12.792, mean=1.543, sd=2.234)
  
  
  rawMeritaid<-rtruncnorm(a3, a=0, b=6.818, mean=0.168, sd=0.538) 
  
  
  rawAidamount<- vector()
  
  j<-1
  x<-1
  z<-1
  for (i in finraw){
    if (i==1){
      rawAidamount<- c(rawAidamount, rawNeedaid[j])
      j<- j+1
    }else if (i==2){
      rawAidamount<- c(rawAidamount, rawLoanaid[x])
      x<-x+1
    }else if (i==3){
      rawAidamount<- c(rawAidamount, rawMeritaid[z])
      z<- z+1
    }else if (i==0){
      rawAidamount<- c(rawAidamount, 0)
    }
    
    
  }
  
  
  Aidamount<- vector()
  for (i in rawAidamount){
    Aidamount<- c(Aidamount, round(i, 2))
    
  }
  
  #we will be using the truncnorm library to construct the variables
  

  rawUnmet<- rtruncnorm(n, a=0, b=26.347, mean=2.038, sd=3.711)
  Unmet<- round(rawUnmet, 2)
  
  rawACT<- rtruncnorm(n, a=11, b=35, mean=24.657, sd=4.189)
  ACT<- round(rawACT, 2)
  rawAP<- rtruncnorm(n, a=0, b=59, mean=3.153, sd=6.671)
  AP<- round(rawAP, 2)
  rawCourse<- rtruncnorm(n, a=0, b=100, mean=91.857, sd=20.288)
  Course<- round(rawCourse, 2)
  rawCcount<- rtruncnorm(n, a=0, b=5, mean=0.691, sd=0.898)
  Ccount<- round(rawCcount, 2)
  rawDcount<- rtruncnorm(n, a=0, b=4, mean=0.13, sd=0.387)
  Dcount<- round(rawDcount, 2)
  
  #we will also generate the Result here according to the paper
  
  #need to make some changes here
  
  #preparation for the end Result of the paper
  beta1<- matrix(c(log(1.0163), log(1.0387), log(1.0751), log(0.5763), log(0.8239), log(1.4669), log(1.0736), log(1.4007), log(3.4570), log(1.0732),log(0.9210), log(1.0252), log(0.9612), log(2.9302), log(0.9598), log(1.4969), log(1.9247), log(0.8705), log(0.7729),log(0.7322), log(0.5342)),ncol=21)
  beta2<- matrix(c(log(1.0331), log(0.9644), log(1.0658), log(0.6495), log(1.4504), log(0.9286), log(0.6117), log(1.0510), log(1.161), log(1.9606), log(1.8764), log(0.9781), log(0.976), log(1.0572), log(0.979), log(1.1397), log(1.5924), log(0.878), log(0.843), log(0.7328), log(0.618)), ncol=21)
  beta3<- matrix(c(log(0.9838), log(1.077), log(1.0087), log(0.8873), log(0.568), log(1.5797), log(1.7551), log(1.3327), log(2.9753), log(0.5474), log(0.4909), log(1.0482), log(0.9848), log(2.7718), log(0.9803), log(1.3134), log(1.2086), log(0.9915), log(0.9169), log(0.9991), log(0.8646)), ncol= 21)
  
  z<- 1
  a<- 1
  b<- 1
  c<- 1
  
  #we need to build this part using the already known finraw, by constructing a matrix with n rows 3 columns, with only one column has a value besides 0
  Calaid<- matrix(0, n, 3)
  for (i in finraw){
    if (i==1){
      Calaid[z, 2]<-  0
      Calaid[z, 3]<-  0
      Calaid[z,1]<- Calaid[z,1] + rawNeedaid[a]
      a<- a+1
      z<- z+1
    }else if (i==2){
      Calaid[z, 1]<-  0
      Calaid[z, 3]<-  0
      Calaid[z,2]<- Calaid[z,2] + rawLoanaid[b]
      b<- b+1
      z<- z+1
    }else if (i==3){
      Calaid[z, 1]<-  0
      Calaid[z, 2]<-  0
      Calaid[z,3]<- Calaid[z,3] + rawMeritaid[c]
      c<- c+1
      z<- z+1
    }else if (i==0){
      Calaid[z, 1]<-  0
      Calaid[z, 2]<-  0
      Calaid[z, 3]<-  0
      z<- z+1
      
    }
    
  }
  
  
  #the below two for loops are transfering the already established D_R and G_O (as they got more then 1 values) to dummy variables
  
  D_U<- vector()
  D_A<- vector()
  for (i in D_R){
    if (i==0){
      D_U<- c(D_U, 1)
      D_A<- c(D_A, 0)
      
    }else if (i==1){
      D_U<- c(D_U, 0)
      D_A<- c(D_A, 1)
    }else if (i==2){
      D_U<- c(D_U, 0)
      D_A<- c(D_A, 0)
    }
    
  }
  
  G_S<- vector()
  G_R<- vector()
  for (i in G_O){
    if (i==0){
      G_S<- c(G_S, 1)
      G_R<- c(G_R, 0)
      
    }else if (i==1){
      G_S<- c(G_S, 0)
      G_R<- c(G_R, 0)
    }else if (i==2){
      G_S<- c(G_S, 0)
      G_R<- c(G_R, 0)
    }
  }
  
  #preparation df for calculating the Result
  
  raw1<- data.frame(rawUnmet)
  raw2<- as.data.frame(Calaid)
  raw3<- data.frame(D_F, D_U, D_A, D_G, D_19, G_S, G_R, rawACT, rawAP, A_R, rawCourse, rawCcount, rawDcount, A_F, A_LC, A_LL, A_A)
  
  #the rawdf matches exactly the order of the paper
  rawdf<- cbind(raw1, raw2, raw3)
  
  rawdf<- data.matrix(rawdf)
  
  rawResult<- matrix(0, n, 5)

  
  #we use the papers Result to calculate the odds of getting either Transferred, Not graduated, or Graduated
  for (i in 1:n){
    rawResult[i,1] <- exp(rawdf[i,]%*%t(beta1))**2
    rawResult[i,2] <- exp(rawdf[i,]%*%t(beta1))*exp(rawdf[i,]%*%t(beta2))
    rawResult[i,3] <- exp(rawdf[i,]%*%t(beta2))*exp(rawdf[i,]%*%t(beta3))
    rawResult[i,4] <- 1/(rawResult[i,1]+ rawResult[i,2] + rawResult[i,3])
    rawResult[i,5] <- sample(c(0,1,2), size=1, replace=TRUE, prob = c(rawResult[i,1]*rawResult[i,4], rawResult[i,2]*rawResult[i,4], rawResult[i,3]*rawResult[i,4]))
    
  }
  
  Result<- factor(rawResult[1:n,5], levels = c(0, 1, 2), labels = c("Not Graduated", "Transfer Graduated", "Graduated"))
  
  
  #build the dataframe with final Result
  final <-data.frame(Gender, Race, Generation, Status, Geographic, Remedy, Choice, Living, Community, Athlete, Aidtype, Aidamount, Unmet, ACT, AP, Course, Ccount, Dcount, Result)
  
  return(final)
}
