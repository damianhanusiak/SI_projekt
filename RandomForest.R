library(randomForest)
data = wine
data$V1 = factor(data$V1)
res <- c()

for(x in 1:50)
{
  idx = sample(1:nrow(data), 0.5*nrow(data))
  test = data[idx,]
  train = data[-idx,]
  model = randomForest(V1 ~ ., data = train)
  p=predict(model, test)
  cm = table(p, test$V1)
  quality = sum(diag(cm))/sum(cm)
  res = append(res, quality)
}
mean(res)
sd(res)
cm