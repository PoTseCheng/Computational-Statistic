library(rpart)
test<- Observations(100)


#cart by rpart

mytree <- rpart(
  result ~ severity + gender + age + address + contact + temperature + fever + cough + fatigue + Dyspnea + Headache, 
  data = test, 
  method = "class",
  control=rpart.control(minsplit=1, minbucket=1, cp=0.001))
#check what is the most reasonable cp for rpart

plot(mytree)
text(mytree, use.n=TRUE, all=TRUE, cex=.8)

#how to test it?

#c4.5


#