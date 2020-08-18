library(rpart)




set.seed(123)
train<- Observations(100)

train2<- Rawdata(100)

#cart by rpart

mytree <- rpart(
  temperature ~ ., 
  data = train, 
  method = "class",
  control=rpart.control( cp=0.001))
#check what is the most reasonable cp for rpart
#, minbucket=1
#severity + gender + age + address + contact + temperature + fever + cough + fatigue + Dyspnea + Headache
plot(mytree)
text(mytree, use.n=TRUE, all=TRUE, cex=.8)

printcp(mytree)
plotcp(mytree)

mytree2 <- rpart(
  result ~. , 
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
  method = "class"
 )
#check what is the most reasonable cp for rpart
#, minbucket=1
#severity + gender + age + address + contact + temperature + fever + cough + fatigue + Dyspnea + Headache
plot(mytree3)
text(mytree3, use.n=TRUE, all=TRUE, cex=.8)

printcp(mytree3)
plotcp(mytree3)
#how to test it?



#find a way to show the strengh
#find a way to make the pictures readable
#c4.5


#