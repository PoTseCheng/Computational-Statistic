
library(data.table)
library(rpart)
library(RWeka)
library(partykit)
library(rpart.plot)

set.seed(123)
train<- Observations(500)

train2<- Observations2(500)

#cart by rpart

mytree <- rpart(
  Symptoms ~. , 
  data = train, 
  method = "class",
  control=rpart.control( minsplit=3)
  )
#check what is the most reasonable cp for rpart
#, minbucket=1
#severity + gender + age + address + contact + temperature + fever + cough + fatigue + Dyspnea + Headache
plot(mytree)
text(mytree, use.n=TRUE, all=TRUE, cex=.8)

mytree2 <- rpart(
  Result ~. , 
  data = train, 
  method = "class",
  control=rpart.control( minsplit=1, minbucket=1, cp=0.001)
)
#check what is the most reasonable cp for rpart
#, minbucket=1
#severity + gender + age + address + contact + temperature + fever + cough + fatigue + Dyspnea + Headache
plot(mytree2)
text(mytree2, use.n=TRUE, all=TRUE, cex=.8)

printcp(mytree2)
plotcp(mytree2)

mytree3 <- rpart(
  Result ~ ., 
  data = train2, 
  method = "class",
  
 )
#check what is the most reasonable cp for rpart
#, minbucket=1

plot(mytree3)
text(mytree3, use.n=TRUE, all=TRUE, cex=.8)

printcp(mytree3)
plotcp(mytree3)
#weakness, biased towards variables with more variations, which is clear in both 
#data

#almost uninterpretable when the exogenuous categories are huge

#computational expensive



#find a way to make the pictures readable



#c4.5


J1 <- J48(Symptoms ~ .,
          train
          )
plot(J1)
summary(J1)

#key difference, post (CART) and pre pruning(c4.5)
#both rely on information gain formula (with a small twists) 
#according to the J48 algorithm, the node is not worth splitting

J2 <- J48(Result ~.,
          train2
          )
plot(J2)

J3 <- J48(Aidtype ~.,
          train2
          )
plot(J3)
#one of the major difference, c4.5 did not choose.



c1<- ctree(symptoms ~ .,
           train
           )

plot(c1)

c2<- ctree(Result ~ .,
           train
)

plot(c2)

c3<- ctree(Aidtype ~ .,
           train2
           )

plot(c3)

c4<- ctree(Result ~ .,
           train2
           )

plot(c4)







