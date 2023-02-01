library(class)
data = wine
data$V1 = factor(data$V1)
res <- c()

for(x in 1:50)
{
  idx = sample(1:nrow(data), as.integer(0.5*nrow(data)))
  train = data[idx,]
  test = data[-idx,]
  trainData = train[,2:14]
  testData = test[,2:14]
  traincl = train[,1]
  testcl = test[,1]
  nn3 <- knn(trainData, testData, cl=traincl, k=3)
  cm3 = table(nn3,testcl)
  s = sum(diag(cm3))/sum(cm3)
  res <- append(res, s)
}

sd(res)
mean(res)
cm3