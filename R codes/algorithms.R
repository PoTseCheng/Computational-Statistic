
library(data.table)
library(rpart)
library(RWeka)
library(partykit)


set.seed(123)
train<- Observations(50)

train2<- Observations2(50)

#cart by rpart

mytree <- rpart(
  symptoms ~. , 
  data = train, 
  method = "class",
  control=rpart.control( minsplit=3, minbucket=1, cp=0.001)
  )
#check what is the most reasonable cp for rpart
#, minbucket=1
#severity + gender + age + address + contact + temperature + fever + cough + fatigue + Dyspnea + Headache
plot(mytree)
text(mytree, use.n=TRUE, all=TRUE, cex=.8)

printcp(mytree)
plotcp(mytree)

mytree3 <- rpart(
  result ~ ., 
  data = train2, 
  method = "class",
  minsplit = 2, 
  minbucket = 1
 )
#check what is the most reasonable cp for rpart
#, minbucket=1

plot(mytree3)
text(mytree3, use.n=TRUE, all=TRUE, cex=.8)

printcp(mytree3)
plotcp(mytree3)
#how to test it?



#find a way to show the strengh
#find a way to make the pictures readable



#c4.5


J1 <- J48(symptoms ~ .,
          train
          )
plot(J1)
summary(J1)
#according to the J48 algorithm, the node is not worth splitting

J2 <- J48(result ~.,
          train2
          )
plot(J2)
summary(J2)
#resonate with the cp=inf

#ct tree by partykit(dont know if we keep it or not)



c1<- ctree(symptoms ~ .,
           train
           )

plot(c1)

c2 <- ctree(result ~.,
          train2,
          )
plot(c2)






