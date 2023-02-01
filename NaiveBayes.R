library(naivebayes)
data = wine
data$V1 = factor(data$V1)
res <- c()

for(x in 1:50)
{
  idx = sample(2, 178, replace = T, c(0.5,0.5))
  train = data[idx == 1,]
  test = data[idx == 2,]
  model = naive_bayes(V1~., data = train, usekernel = T)
  predict = predict(model, test)
  cm = table(predict, test$V1)
  p = sum(diag(cm))/sum(cm)
  res <- append(res, p)
}

sd(res)
mean(res)
cm