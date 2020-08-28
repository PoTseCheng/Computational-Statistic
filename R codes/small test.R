library(rpart)
library(rpart.plot)


test<- function(n){
  y<- rnorm(n, 0, 1)
  x1<- c(rnorm(n-1, 0, 1),1)
  x2<- c(rnorm(n-2, 0, 1),1/2,1/2)
  x3<- c(rnorm(n-3, 0, 1),1/3,1/3,1/3)
  end<- data.frame(y, x1, x2, x3)
  
  return(end)
}

set.seed(100)
df<- test(100)

bias<- rpart(y~.,data=df)
prp(bias)

bias<- function(n, df){
  
  raw2<- vector()
  for (i in 1:n){
    
    raw<- rpart(y~.,data=df)
    a<- path.rpart(raw,2)
    b<-a[[1]][2]
    s1 = unlist(strsplit(b, split='>', fixed=TRUE))[1]
    s1 = unlist(strsplit(s1, split='<', fixed=TRUE))[1]
    raw2<-c(raw2, s1)
    
  }
  
  final<-c(sum(raw2=="x1")/n,sum(raw2=="x2")/n,sum(raw2=="x3")/n)
  
  return(final)
}

df<- test(100)
a<- bias(500, df)

a


 