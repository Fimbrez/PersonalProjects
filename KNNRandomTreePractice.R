
library(tidyverse)
library(tree)
library(plyr)
library(class)
library(rpart)
library(maptree)
library(ROCR)

spam <- read_table2("spambase.tab", guess_max=2000)
spam <- spam %>%
  mutate(y = factor(y, levels=c(0,1), labels=c("good", "spam"))) %>% # label as factors
  mutate_at(.vars=vars(-y), .funs=scale) # scale others

spam


calc_error_rate <- function(predicted.value, true.value){
  return(mean(true.value!=predicted.value))
}

records = matrix(NA, nrow=3, ncol=2)
colnames(records) <- c("train.error","test.error")
rownames(records) <- c("knn","tree","logistic")

set.seed(1)
test.indices = sample(1:nrow(spam), 1000)
spam.train=spam[-test.indices,]
spam.test=spam[test.indices,]


nfold = 10
set.seed(1)
folds = seq.int(nrow(spam.train)) %>% ## sequential obs ids
  cut(breaks = nfold, labels=FALSE) %>% ## sequential fold ids
  sample


do.chunk <- function(chunkid, folddef, Xdat, Ydat, k){
  train = (folddef!=chunkid)
  Xtr = Xdat[train,]
  Ytr = Ydat[train]
  Xvl = Xdat[!train,]
  Yvl = Ydat[!train]
  ## get classifications for current training chunks
  predYtr = knn(train = Xtr, test = Xtr, cl = Ytr, k = k)
  ## get classifications for current test chunk
  predYvl = knn(train = Xtr, test = Xvl, cl = Ytr, k = k)
  data.frame(train.error = calc_error_rate(predYtr, Ytr),
             val.error = calc_error_rate(predYvl, Yvl))
}

YTrain = spam.train$y
XTrain = spam.train %>% select(-y) %>% scale(center = TRUE, scale = TRUE)
YTest = spam.test$y
XTest = spam.test %>% select(-y) %>% scale(center = TRUE, scale = TRUE)

#Want to perform 10-fold cross validation
error.folds = NULL
kvec = c(1, seq(10, 50, length.out=5))
set.seed(1)
for (j in kvec){
  tmp = ldply(1:nfold, do.chunk, # Apply do.chunk() function to each fold
              folddef=folds, Xdat=XTrain, Ydat=YTrain, k=j)
  # Necessary arguments to be passed into do.chunk
  tmp$neighbors = j # Keep track of each value of neighors
  error.folds = rbind(error.folds, tmp) # combine results
}
best.kfold <- error.folds[error.folds$val.error == min(error.folds$val.error),]$neighbors
## this corresponds to k=10

# we already have the test error rate below:

#computing the test error rate:
pred.YTest = knn(train=XTrain, test=XTest, cl=YTrain, k=best.kfold)
conf.matrix = table(predicted=pred.YTest, true=YTest)
# Test accuracy rate
sum(diag(conf.matrix)/sum(conf.matrix))
## [1] 0.64
# Test error rate
1 - sum(diag(conf.matrix)/sum(conf.matrix))

calc_error_rate( error.folds[error.folds$val.error == min(error.folds$val.error),]$train.error, 1 - sum(diag(conf.matrix)/sum(conf.matrix)))

records["knn",]<-c(error.folds[error.folds$val.error == min(error.folds$val.error),]$train.error,1 - sum(diag(conf.matrix)/sum(conf.matrix)))

records


tree.control(minsize = 5, mindev = 1e-5)
dim(spam.train[,])
?tree.control
?tree
spamtree <-tree(y~.,data = spam.train,control = tree.control(nobs = 3601, minsize = 5, mindev = 1e-5))
summary(spamtree)
#there are 149 nodes
#49 were misclassified

pruned <-prune.tree(spamtree, best = 10, method = "misclass")
?draw.tree
draw.tree(pruned, nodeinfo = TRUE,cex = 0.3)


set.seed(1)
cv <- cv.tree(spamtree, rand = folds, FUN = prune.misclass, K = 10 )
best.size.cv <- cv$size[which.min(cv$dev)]
best.size.cv
cv
##plotting now
op = par(mfrow=c(1,1))
plot(cv$size , cv$dev, type="b", 
     xlab = "Number of leaves, \'best\'", ylab = "CV Misclassification Error",
     col = "red", main="CV")
abline(v=best.size.cv, lty=2)
## the dotted line is the best.size.cv

spamtree.pruned <- prune.misclass(spamtree, best = best.size.cv)
pred.spam.prune = predict(spamtree.pruned, spam.test , type="class")
error.spamtree.prune = table(pred.spam.prune, YTest)
error.spamtree.prune
#classification error
true.treevalue <-1-sum(diag(error.spamtree.prune))/sum(error.spamtree.prune)
#finding 
summary(spamtree)
calc_error_rate(pred.spam.prune,true.treevalue)

records["tree",] <-c(pred.spam.prune,true.treevalue)


glm.fit = glm(y ~.,
              data=spam.train, family=binomial)
summary(glm.fit)

prob.training = predict(glm.fit, type="response")
round(prob.training, digits=2)
Dis = spam %>%
  mutate(predHIRING=as.factor(ifelse(prob.training<=0.5, "Spam", "Good")))
# Confusion matrix (training error/accuracy)
table(pred=Dis$y, true=YTest)


