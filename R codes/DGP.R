install.packages("wakefield")

#Packages for DGP
#for age generation
library(wakefield)
library(dplyr)
library(partykit)
library(truncnorm)

#DGP 1
Observations<- function (n){

#establish group difference
p1<- 46/262
n1<- round(n*p1)
p2<- 216/262
n2<- round(n*p2)

#group A (severe)#
severity<- rep(1, n1)

#age generation

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
age<- c(n11, n12, n13, n14)

#gender
temp0<- sample(c(1, 0), size=n1, replace=TRUE, prob = c(26/46, 20/46))
gender<- factor(temp0, levels = c(1,0), labels = c("Male", "Female"))
#living address
temp<- sample(c(0, 1, 2), size=n1, replace=TRUE, prob = c(39/46, 5/46, 2/46))
address<- factor(temp, levels = c(0,1,2), labels = c("Beijin", "Wuhan", "Other"))

#temperature
#temp2<- sample(c(0, 1, 2, 3), size=n1, replace=TRUE, prob = c(9/46, 16/46, 20/46, 1/46))
#tlabel<- factor(temp2, levels = c(0, 1, 2, 3), labels = c("average", "high", "dangerous", "critical"))



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


rawtemperature<- c(n31, n32, n33, n34)
temperature<- vector()
for (i in rawtemperature){
  temperature<- c(temperature, round(i, 2))
  
  
}

tlabel<- vector()
for (i in temperature){
  if (i <= 37.5){
    tlabel<- c(tlabel, "Normal")
  } else if (i>37.5& i <=40){
    tlabel<- c(tlabel, "Fever")
  } else if (i>40){
    tlabel<- c(tlabel, "Hyperpyrexia")
  }
  
  
}
#Wuhan Contacts
temp3<- sample(c(0, 1, 2), size=n1, replace=TRUE, prob = c(13/46, 26/46, 7/46))
contact<- factor(temp3, levels = c(0, 1, 2), labels = c("Wuhan 14 days", "Case 14 days", "Unknown"))
#symptoms
cough<- sample(c(1, 0), size=n1, replace=TRUE, prob= c(25/46, 21/46))
fatigue<- sample(c(1, 0), size=n1, replace=TRUE, prob= c(15/46, 31/46))
Dyspnea<- sample(c(1, 0), size=n1, replace=TRUE, prob= c(15/46, 31/46))
Headache<- sample(c(1, 0), size=n1, replace=TRUE, prob= c(3/46, 43/46))
all<- data.frame(cough, fatigue, Dyspnea, Headache)
symptoms<- vector()

for (i in 1:nrow(all))
{
  if (all[i,1]==1&all[i,2]==1&all[i,3]==1&all[i,4]==1){
    symptoms<-c(symptoms, "Coughing, Fatigue, Dyspnea, Headache")
  }else if (all[i,1]==1&all[i,2]==1&all[i,3]==1&all[i,4]==0){
    symptoms<-c(symptoms, "Coughing, Fatigue, Dyspnea")
  }else if (all[i,1]==1&all[i,2]==1&all[i,3]==0&all[i,4]==0){
    symptoms<-c(symptoms, "Coughing, Fatigue")
  }else if (all[i,1]==1&all[i,2]==0&all[i,3]==0&all[i,4]==0){
    symptoms<-c(symptoms, "Coughing")
  }else if (all[i,1]==0&all[i,2]==1&all[i,3]==1&all[i,4]==1){
    symptoms<-c(symptoms, "Fatigue, Dyspnea, Headache")
  }else if (all[i,1]==0&all[i,2]==1&all[i,3]==1&all[i,4]==0){
    symptoms<-c(symptoms, "Fatigue, Dyspnea")
  }else if (all[i,1]==0&all[i,2]==1&all[i,3]==0&all[i,4]==1){
    symptoms<-c(symptoms, "Fatigue, Headache")
  }else if (all[i,1]==0&all[i,2]==1&all[i,3]==0&all[i,4]==0){
    symptoms<-c(symptoms, "Fatigue")
  }else if (all[i,1]==1&all[i,2]==1&all[i,3]==0&all[i,4]==1){
    symptoms<-c(symptoms, "Coughing, Fatigue, Headache")
  }else if (all[i,1]==1&all[i,2]==0&all[i,3]==1&all[i,4]==0){
    symptoms<-c(symptoms, "Coughing, Dyspnea")
  }else if (all[i,1]==1&all[i,2]==0&all[i,3]==1&all[i,4]==1){
    symptoms<-c(symptoms, "Coughing, Dyspnea, Headache")
  }else if (all[i,1]==1&all[i,2]==0&all[i,3]==0&all[i,4]==1){
    symptoms<-c(symptoms, "Coughing, Headache")
  }else if (all[i,1]==0&all[i,2]==0&all[i,3]==1&all[i,4]==1){
    symptoms<-c(symptoms, "Dyspnea, Headache")
  }else if (all[i,1]==0&all[i,2]==0&all[i,3]==1&all[i,4]==0){
    symptoms<-c(symptoms, "Dyspnea")
  }else if (all[i,1]==0&all[i,2]==0&all[i,3]==0&all[i,4]==1){
    symptoms<-c(symptoms, "Headache")
  }else if (all[i,1]==0&all[i,2]==0&all[i,3]==0&all[i,4]==0){
    symptoms<-c(symptoms, "No Symptoms")
  }
  
}
#result
temp4<- sample(c(0, 1, 2), size=n1, replace=TRUE, prob = c(2/46, 41/46, 3/46))
result<- factor(temp4, levels = c(0, 1, 2), labels = c("Discharge", "Hospitalization", "Death"))

groupa<- data.frame(severity, gender, age, address, contact, temperature, tlabel, symptoms, result)

#group B

severity<- rep(0, n2)


#age generation

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
age<- c(n21, n22, n23, n24)

#gender
temp0<- sample(c(1,0), size=n2, replace=TRUE, prob = c(101/216, 115/216))
gender<-factor(temp0, levels = c(1,0), labels = c("Male", "Female"))
#living address
temp<- sample(c(0, 1, 2), size=n2, replace=TRUE, prob = c(153/216, 48/216, 15/216))
address<- factor(temp, levels = c(0,1,2), labels = c("Beijin", "Wuhan", "Other"))

#temperature

#temp2<- sample(c(0, 1, 2, 3), size=n2, replace=TRUE, prob = c(38/216, 94/216, 76/216, 8/216))

#tlabel<- factor(temp2, levels = c(0, 1, 2, 3), labels = c("average", "high", "dangerous", "critical"))


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
temperature<- c(n41, n42, n43, n44)

rawtemperature<- c(n41, n42, n43, n44)
temperature<- vector()
for (i in rawtemperature){
  temperature<- c(temperature, round(i, 2))
  
  
}

tlabel<- vector()
for (i in temperature){
  if (i <= 37.5){
    tlabel<- c(tlabel, "Normal")
  } else if (i>37.5& i <=40){
    tlabel<- c(tlabel, "Fever")
  } else if (i>40){
    tlabel<- c(tlabel, "Hyperpyrexia")
  }
  
  
}

#Wuhan Contacts
temp3<- sample(c(0, 1, 2), size=n2, replace=TRUE, prob = c(93/216, 103/216, 20/216))
contact<- factor(temp3, levels = c(0, 1, 2), labels = c("Wuhan 14 days", "Case 14 days", "Unknown"))
#symptoms

cough<- sample(c(1, 0), size=n2, replace=TRUE, prob= c(95/216, 121/216))
fatigue<- sample(c(1, 0), size=n2, replace=TRUE, prob= c(54/216, 162/216))
Dyspnea<- sample(c(1, 0), size=n2, replace=TRUE, prob= c(3/216, 213/216))
Headache<- sample(c(1, 0), size=n2, replace=TRUE, prob= c(14/216, 202/216))
all<- data.frame(cough, fatigue, Dyspnea, Headache)

symptoms<- vector()

for (i in 1:nrow(all))
{
  if (all[i,1]==1&all[i,2]==1&all[i,3]==1&all[i,4]==1){
    symptoms<-c(symptoms, "Coughing, Fatigue, Dyspnea, Headache")
  }else if (all[i,1]==1&all[i,2]==1&all[i,3]==1&all[i,4]==0){
    symptoms<-c(symptoms, "Coughing, Fatigue, Dyspnea")
  }else if (all[i,1]==1&all[i,2]==1&all[i,3]==0&all[i,4]==0){
    symptoms<-c(symptoms, "Coughing, Fatigue")
  }else if (all[i,1]==1&all[i,2]==0&all[i,3]==0&all[i,4]==0){
    symptoms<-c(symptoms, "Coughing")
  }else if (all[i,1]==0&all[i,2]==1&all[i,3]==1&all[i,4]==1){
    symptoms<-c(symptoms, "Fatigue, Dyspnea, Headache")
  }else if (all[i,1]==0&all[i,2]==1&all[i,3]==1&all[i,4]==0){
    symptoms<-c(symptoms, "Fatigue, Dyspnea")
  }else if (all[i,1]==0&all[i,2]==1&all[i,3]==0&all[i,4]==1){
    symptoms<-c(symptoms, "Fatigue, Headache")
  }else if (all[i,1]==0&all[i,2]==1&all[i,3]==0&all[i,4]==0){
    symptoms<-c(symptoms, "Fatigue")
  }else if (all[i,1]==1&all[i,2]==1&all[i,3]==0&all[i,4]==1){
    symptoms<-c(symptoms, "Coughing, Fatigue, Headache")
  }else if (all[i,1]==1&all[i,2]==0&all[i,3]==1&all[i,4]==0){
    symptoms<-c(symptoms, "Coughing, Dyspnea")
  }else if (all[i,1]==1&all[i,2]==0&all[i,3]==1&all[i,4]==1){
    symptoms<-c(symptoms, "Coughing, Dyspnea, Headache")
  }else if (all[i,1]==1&all[i,2]==0&all[i,3]==0&all[i,4]==1){
    symptoms<-c(symptoms, "Coughing, Headache")
  }else if (all[i,1]==0&all[i,2]==0&all[i,3]==1&all[i,4]==1){
    symptoms<-c(symptoms, "Dyspnea, Headache")
  }else if (all[i,1]==0&all[i,2]==0&all[i,3]==1&all[i,4]==0){
    symptoms<-c(symptoms, "Dyspnea")
  }else if (all[i,1]==0&all[i,2]==0&all[i,3]==0&all[i,4]==1){
    symptoms<-c(symptoms, "Headache")
  }else if (all[i,1]==0&all[i,2]==0&all[i,3]==0&all[i,4]==0){
    symptoms<-c(symptoms, "No Symptoms")
  }
  
}

#result
temp4<- sample(c(0, 1, 2), size=n2, replace=TRUE, prob = c(2/216, 41/216, 3/216))
result<- factor(temp4, levels = c(0, 1, 2), labels = c("Discharge", "Hospitalization", "Death"))

groupb<- data.frame(severity, gender, age, address, contact, temperature, tlabel, symptoms, result)


total <- rbind(groupa, groupb)
return (total)
}


#DGP from the paper2

Observations2<- function (n){
  #maybe can expand this function in the future for more testing(for instance adding 0 function to test certain algorithm)
  
  
  #key idea about this process: We will be using the descriptive statistics to
  #to generate the result
  
  #below are for the dummy variables, which is randomly sampled, we will follow the
  #suggested mean but not the SD. The reason is because the SD is realated to the sample
  #size while the mean is not. In this experiment, the sample size is a controlable variable.
  D_F<- sample(c(0,1), size=n, replace=TRUE, prob = c(1-0.543, 0.543))
  Gender<- factor(D_F, levels = c(0,1), labels = c("Female", "Male"))
  D_R<- sample(c(0,1,2), size=n, replace=TRUE, prob = c(0.079, 0.11, 0.811))
  Race<- factor(D_R, levels = c(0, 1, 2), labels = c("Minorities", "Asian", "White"))
  D_G<- sample(c(0,1), size=n, replace=TRUE, prob = c(0.636, 0.364))
  Generation<- factor(D_G, levels = c(0, 1), labels = c("No", "Yes"))
  D_19<- sample(c(0,1), size=n, replace=TRUE, prob = c(1-0.019, 0.019))
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
  Needaid<- round(rtruncnorm(a1, a=0, b=9.186, mean=0.948, sd=1.620), 2)
  Loanaid<- round(rtruncnorm(a2, a=0, b=12.792, mean=1.543, sd=2.234), 2)
  Meritaid<- round(rtruncnorm(a3, a=0, b=6.818, mean=0.168, sd=0.538), 2)
  
  Aidamount<- vector()
  
  j<-1
  x<-1
  z<-1
  for (i in finraw){
    if (i==1){
      Aidamount<- c(Aidamount, Needaid[j])
      j<- j+1
    }else if (i==2){
      Aidamount<- c(Aidamount,Loanaid[x])
      x<-x+1
    }else if (i==3){
      Aidamount<- c(Aidamount,Meritaid[z])
      z<- z+1
    }else if (i==0){
      Aidamount<- c(Aidamount, 0)
    }
    
    
  }

  
  #we will be using the truncnorm library to construct the variables
  

  Unmet<- round(rtruncnorm(n, a=0, b=26.347, mean=2.038, sd=3.711), 2)
  
  ACT<- round(rtruncnorm(n, a=11, b=35, mean=24.657, sd=4.189), 2)
  AP<- round(rtruncnorm(n, a=0, b=59, mean=3.153, sd=6.671), 2)
  Course<- round(rtruncnorm(n, a=0, b=100, mean=91.857, sd=20.288), 2)
  Ccount<- round(rtruncnorm(n, a=0, b=5, mean=0.691, sd=0.898), 2)
  Dcount<- round(rtruncnorm(n, a=0, b=4, mean=0.13, sd=0.387), 2)
  
  
  #we will also generate the result here according to the paper
  
  temp<- sample(c(0, 1, 2), size=n, replace=TRUE, prob = c(0.659, 0.0854, 0.2556))
  
  result <-factor(temp, levels = c(0,1,2), labels = c("Graduated", "Transferred Graduated ", "Not Graduated"))
  
  
  #build the dataframe
  raw <-data.frame(Gender, Race, Generation, Status, Geographic, Remedy, Choice, Living, Community, Athlete, Aidtype, Aidamount, Unmet, ACT, AP, Course, Ccount, Dcount, result)
  
  return(raw)
}
