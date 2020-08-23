

beta1<- matrix(c(log(1.0163), log(1.0387), log(1.0751), log(0.5763), log(0.8239), log(1.4669), log(1.0736), log(1.4007), log(3.4570), log(1.0732),log(0.9210), log(1.0252), log(0.9612), log(2.9302), log(0.9598), log(1.4969), log(1.9247), log(0.8705), log(0.7729),log(0.7322), log(0.5342)),ncol=21)
beta2<- matrix(c(log(1.0331), log(0.9644), log(1.0658), log(0.6495), log(1.4504), log(0.9286), log(0.6117), log(1.0510), log(1.161), log(1.9606), log(1.8764), log(0.9781), log(0.976), log(1.0572), log(0.979), log(1.1397), log(1.5924), log(0.878), log(0.843), log(0.7328), log(0.618)), ncol=21)
beta3<- matrix(c(log(0.9838), log(1.077), log(1.0087), log(0.8873), log(0.568), log(1.5797), log(1.7551), log(1.3327), log(2.9753), log(0.5474), log(0.4909), log(1.0482), log(0.9848), log(2.7718), log(0.9803), log(1.3134), log(1.2086), log(0.9915), log(0.9169), log(0.9991), log(0.8646)), ncol= 21)

n<-100

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

#we will also generate the result here according to the paper

#need to make some changes here

#preparation for the end result of the paper

z<- 1
a<- 1
b<- 1
c<- 1

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



raw1<- data.frame(rawUnmet)
raw2<- as.data.frame(Calaid)
raw3<- data.frame(D_F, D_U, D_A, D_G, D_19, G_S, G_R, rawACT, rawAP, A_R, rawCourse, rawCcount, rawDcount, A_F, A_LC, A_LL, A_A)

rawdf<- cbind(raw1, raw2, raw3)

rawdf<- data.matrix(rawdf)

rawresult<- matrix(0, n, 3)
result<- vector()


for (i in 1:n){
  rawresult[i,1] <-exp(test1[i,]%*%t(beta1))
  rawresult[i,2] <-exp(test1[i,]%*%t(beta2))
  rawresult[i,3] <- exp(test1[i,]%*%t(beta3))
  if(rawresult[i,1]>rawresult[i,2]&rawresult[i,1]>rawresult[i,2]){
    result<- c(result, "Transferred Graduated")
  }else if (rawresult[i,2]>rawresult[i,1]&rawresult[i,2]>rawresult[i,3]){
    result<- c(result, "Not Graduated")
  }else if (rawresult[i,3]>rawresult[i,1]&rawresult[i,3]>rawresult[i,2]){
    result<- c(result, "Graduated")
  }

}


