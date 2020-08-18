#prediction
pred<- predict(mytree4, 
               type=c("prob"), 
               newdata=test2
)

pred2<- predict(mytree, 
                type=c("prob"), 
                newdata=test1
)