install.packages("wakefield")

#for age generation
library(wakefield)

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
  n13<- c(n12, addmiss)
  
  
} else if (test > n1){
  n13 <- n13[-sample(1:length(n13), 1)]
  
} 
age<- c(n11, n12, n13, n14)

#gender
gender<- sample(c(1, 0), size=n1, replace=TRUE, prob = c(26/46, 20/46))

#living address
temp<- sample(c(0, 1, 2), size=n1, replace=TRUE, prob = c(39/46, 5/46, 2/46))
address<- factor(temp, levels = c(0,1,2), labels = c("Beijin", "Wuhan", "Other"))

#temperature
temp2<- sample(c(0, 1, 2, 3), size=n1, replace=TRUE, prob = c(9/46, 16/46, 20/46, 1/46))
temperature<- factor(temp2, levels = c(0, 1, 2, 3), labels = c("average", "high", "dangerous", "critical"))

#Wuhan Contacts
temp3<- sample(c(0, 1, 2), size=n1, replace=TRUE, prob = c(13/46, 26/46, 7/46))
contact<- factor(temp3, levels = c(0, 1, 2), labels = c("Wuhan 14 days", "Case 14 days", "Unknown"))
#symptoms
fever<- sample(c(1, 0), size=n1, replace=TRUE, prob= c(37/46, 9/46))
cough<- sample(c(1, 0), size=n1, replace=TRUE, prob= c(25/46, 21/46))
fatigue<- sample(c(1, 0), size=n1, replace=TRUE, prob= c(15/46, 31/46))
Dyspnea<- sample(c(1, 0), size=n1, replace=TRUE, prob= c(15/46, 31/46))
Headache<- sample(c(1, 0), size=n1, replace=TRUE, prob= c(3/46, 43/46))

#result
temp4<- sample(c(0, 1, 2), size=n1, replace=TRUE, prob = c(2/46, 41/46, 3/46))
result<- factor(temp4, levels = c(0, 1, 2), labels = c("Discharge", "Hospitalization", "Death"))

groupa<- data.frame(severity, gender, age, address, contact, temperature, fever, cough, fatigue, Dyspnea, Headache, result)

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
  n13<- c(n22, addmiss)
  
  
} else if (test > n2){
  n23 <- n23[-sample(1:length(n23), 1)]
  
} 
age<- c(n21, n22, n23, n24)

#gender
gender<- sample(c(1,0), size=n2, replace=TRUE, prob = c(101/216, 115/216))

#living address
temp<- sample(c(0, 1, 2), size=n2, replace=TRUE, prob = c(153/216, 48/216, 15/216))
address<- factor(temp, levels = c(0,1,2), labels = c("Beijin", "Wuhan", "Other"))

#temperature
temp2<- sample(c(0, 1, 2, 3), size=n2, replace=TRUE, prob = c(38/216, 94/216, 76/216, 8/216))
temperature<- factor(temp2, levels = c(0, 1, 2, 3), labels = c("average", "high", "dangerous", "critical"))

#Wuhan Contacts
temp3<- sample(c(0, 1, 2), size=n2, replace=TRUE, prob = c(93/216, 103/216, 20/216))
contact<- factor(temp3, levels = c(0, 1, 2), labels = c("Wuhan 14 days", "Case 14 days", "Unknown"))
#symptoms
fever<- sample(c(1, 0), size=n2, replace=TRUE, prob= c(178/216, 38/216))
cough<- sample(c(1, 0), size=n2, replace=TRUE, prob= c(95/216, 121/216))
fatigue<- sample(c(1, 0), size=n2, replace=TRUE, prob= c(54/216, 162/216))
Dyspnea<- sample(c(1, 0), size=n2, replace=TRUE, prob= c(3/216, 213/216))
Headache<- sample(c(1, 0), size=n2, replace=TRUE, prob= c(14/216, 202/216))

#result
temp4<- sample(c(0, 1, 2), size=n2, replace=TRUE, prob = c(2/216, 41/216, 3/216))
result<- factor(temp4, levels = c(0, 1, 2), labels = c("Discharge", "Hospitalization", "Death"))

groupb<- data.frame(severity, gender, age, address, contact, temperature, fever, cough, fatigue, Dyspnea, Headache, result)


total <- rbind(groupa, groupb)
return (total)
}


splitter<- function (data){
  #this function split the training and testing data
  
  
  
}