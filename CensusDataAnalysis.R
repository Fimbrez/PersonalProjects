library(tidyverse)
library(tree)
library(randomForest)
library(gbm)
library(ROCR)
library(e1071)
library(imager)
library(cluster)
library(ISLR)
library(maptree)

#reading in county data:
census = read.csv("census.csv") %>% as.tbl
county_data <- read_csv('acs2015_county_data.csv')
census.del <- na.omit(census) %>%
  mutate(Men = Men/TotalPop*100,
         Employed = Employed/TotalPop*100,
         Citizen = Citizen/TotalPop*100,
         Minority = Hispanic+Black+Native+Asian+Pacific) %>%
  select(-Women, -Hispanic, -Native, -Black, -Asian, -Pacific, -Construction,-Walk, -PublicWork)
census.del <- census.del[,c(1:6,29,7:28)] # reordering (want minority next to white)
census.subct <- census.del %>%
  group_by(State, County) %>%
  add_tally(TotalPop) %>%
  mutate(CountyTotal = n) %>%
  mutate(Weight = TotalPop/CountyTotal) %>%
  select(-n)

census.ct <- census.subct %>%
            summarise_at(vars(Men:CountyTotal), funs(weighted.mean(.,Weight)))
census.ct <- data.frame(census.ct)
print(head(census.ct))
print(head(census.subct))
## Run PCA for both county & sub-county level data
pca_county <- prcomp(census.ct[3:28], scale=TRUE, center = TRUE)
pca_sub <- prcomp(census.subct[3:30], scale=TRUE, center = TRUE)

ct.pc <- data.frame(pca_county$rotation)
subct.pc <- data.frame(pca_sub$rotation)
### I chose to scale bin order to make the SD of every observation 1 in order to be able to accurately find
# the observations with the highest SD due to when we scale, we put every variable on the same units.
# of course, center is true by default, but we chose to do so because it allows us to standardize the 
# data, thus not having one or more variabels be weighted more than others, thereby giving us
# less reliable results from our data.
TOP<-order(abs(ct.pc[1]), decreasing = TRUE)[1:3]
c(TOP)
#gives values of 6,9,8
rownames(ct.pc)[which(abs(ct.pc[1]) == abs(ct.pc[1][6,0:1]))]

rownames(ct.pc)[which(abs(ct.pc[1]) == abs(ct.pc[1][9,0:1]))]

rownames(ct.pc)[which(abs(ct.pc[1]) == abs(ct.pc[1][8,0:1]))]

rownames(ct.pc)[which(ct.pc[1] < 0 )]
# The row names of the features
# with negative values are .... ^, 
# which implies that the correlation of these features have a negative correlation 
#for county
county_sdev <-pca_county$sdev
pve <- county_sdev^2/sum(county_sdev^2)
cumulative_pve <- cumsum(pve)
which(cumulative_pve >= 0.9)[1]

#for subcounty
sub_sdev <-pca_sub$sdev
pve_sub <- sub_sdev^2/sum(sub_sdev^2)
cumulative_pve_sub <- cumsum(pve_sub)
which(cumulative_pve_sub >= 0.9)[1]

plot(pve, type="b", xlab="Principal Component",
     ylab="Variance Porportion",
     main="Total Proportion of Variance Explained for County", col="maroon")

plot(cumulative_pve, type="b", xlab="Principal Component",
     ylab="Variance Porportion",
     main="Cumulative PVE Explained for County", col="maroon")

plot(pve_sub, type="b", xlab="Principal Component",
     ylab="Variance Porportion",
     main="Total Proportion of Variance Explained for Subcounty", col="maroon")

plot(cumulative_pve_sub, type="b", xlab="Principal Component",
     ylab="Variance Porportion",
     main="Cumulative PVE Explained for Subcounty", col="maroon")


# using the entire data set
scale.census.ct <- scale(census.ct[3:28])
dista <- dist(scale.census.ct, method="euclidean")
hc.census.ct <- hclust(dista, method="complete")
clustersa <- cutree(hc.census.ct, k=10)
table(clustersa)

# using the first 5 principal components
ct.pc.scores <- data.frame(ct.pca$x[,1:5])
scale.ct.pc <- scale(ct.pc.scores)
distb <- dist(scale.ct.pc, method="euclidean")
hc.ct.pc <- hclust(distb, method="complete")
clustersb <- cutree(hc.ct.pc, k=10)
table(clustersb)

clustersa[which(census.ct$County == "San Mateo")]

clustersb[which(census.ct$County == "San Mateo")]

#When using census.ct, the county San Mateo is placed into cluster 2. But when using the first five principal
#components, San Mateo is placed into cluster 1. Furthermore, when looking at the cluster assignments
#attached to the original data (in the dataframes dataclustersa and dataclustersb) we see that when San
#Mateo is placed in cluster 2, it appears to be more in line with cluster guidelines (we want the elements in
#  the clusters to be as similar as possible); there are less Alabama counties inside cluster 2 with San Mateo
#for example (which we would expect since San Mateo is a county from California). But when San Mateo is
#placed into cluster 1 there are way more differing counties in its cluster (most of Alabama counties are in
#   this cluster for example). This is most likely due to the fact that the first five principal components do not
#describe most of the variance in census.ct, thus there are disagreements in the clustering.


#### Using Clqffication now ####
election.raw = read.csv("election.csv") %>% as.tbl
election <- filter(election.raw, election.raw$fips != "US" &
                     as.character(election.raw$fips) != as.character(election.raw$state))
county_winner <- election %>%
  group_by(fips) %>%
  mutate(total=sum(votes), pct=votes/total) %>%
  top_n(1)
tmpwinner = county_winner %>% ungroup %>%
  mutate(state = state.name[match(state, state.abb)]) %>% # state abbreviations
  mutate_at(vars(state, county), tolower) %>% # to all lowercase
  mutate(county = gsub(" county| columbia| city| parish", "", county)) # remove suffixes
tmpcensus = census.ct %>% mutate_at(vars(State, County), tolower)
election.cl = tmpwinner %>%
  left_join(tmpcensus, by = c("state"="State", "county"="County")) %>%
  na.omit
## saves meta information to attributes
attr(election.cl, "location") = election.cl %>% select(c(county, fips, state, votes, pct))
election.cl = election.cl %>% select(-c(county, fips, state, votes, pct))

set.seed(10)
n = nrow(election.cl)
in.trn= sample.int(n, 0.8*n)
trn.cl = election.cl[ in.trn,]
tst.cl = election.cl[-in.trn,]

set.seed(20)
nfold = 10
folds = sample(cut(1:nrow(trn.cl), breaks=nfold, labels=FALSE))


calc_error_rate = function(predicted.value, true.value){
  return(mean(true.value!=predicted.value))
}
records = matrix(NA, nrow=3, ncol=2)
colnames(records) = c("train.error","test.error")
rownames(records) = c("tree","logistic","lasso")



trn.cl <- trn.cl %>% select(-total) # getting rid of constant variable
tst.cl <- tst.cl %>% select(-total) # getting rid of constant variable

# setting up the X and Y variables
trn.clX <- trn.cl %>% select(-candidate)
trn.clY <- trn.cl$candidate
tst.clX <- tst.cl %>% select(-candidate)
tst.clY <- tst.cl$candidate

election.cl2 <- election.cl %>%
  mutate(candidate=as.factor(ifelse(candidate=="Donald Trump", "Donald Trump",'Hillary Clinton')))
trn.cl2 = election.cl2[ in.trn,]
tst.cl2 = election.cl2[-in.trn,]
trn.cl2 <- trn.cl2 %>% select(-total) # getting rid of constant variable
tst.cl2 <- tst.cl2 %>% select(-total) # getting rid of constant variable

# setting up the X and Y variables
trn.clX2 <- trn.cl2 %>% select(-candidate)
trn.clY2 <- trn.cl2$candidate
tst.clX2 <- tst.cl2 %>% select(-candidate)
tst.clY2 <- tst.cl2$candidate

# creating the original tree
cantree <- tree(candidate~.,trn.cl2)
summary(cantree)


# using cross validation to find best size
cvtree <- cv.tree(cantree, rand=folds, FUN=prune.misclass)
best.size.cv <- min(cvtree$size[which(cvtree$dev==min(cvtree$dev))])
best.size.cv

# pruning the tree based on cv size
prunedtree <- prune.tree(cantree, best=best.size.cv, method="misclass")
#drawing unpruned tree
draw.tree(cantree, nodeinfo=TRUE, cex=0.6)
title("Unpruned Tree")

#drawing pruned tree
draw.tree(prunedtree, nodeinfo=TRUE, cex=0.6)
title("Pruned Tree")
# training error
pred.cantree.train <- predict(prunedtree, trn.clX2, type="class")
train.errort <- calc_error_rate(pred.cantree.train, trn.clY2)
# test error
pred.cantree.test <- predict(prunedtree, tst.clX2, type="class")
test.errort <- calc_error_rate(pred.cantree.test, tst.clY2)
# putting errors into records
records[1,1] <- train.errort
records[1,2] <- test.errort
records
##### Now to perform analysis #####


election.cl2 <- election.cl %>%
  mutate(candidate=as.factor(ifelse(candidate=="Donald Trump", "Donald Trump",'Hillary Clinton')))
trn.cl2 = election.cl2[ in.trn,]
tst.cl2 = election.cl2[-in.trn,]
trn.cl2 <- trn.cl2 %>% select(-total) # getting rid of constant variable
tst.cl2 <- tst.cl2 %>% select(-total) # getting rid of constant variable

# setting up the X and Y variables
trn.clX2 <- trn.cl2 %>% select(-candidate)
trn.clY2 <- trn.cl2$candidate
tst.clX2 <- tst.cl2 %>% select(-candidate)
tst.clY2 <- tst.cl2$candidate

log.fit <- glm(candidate~., family = binomial, data = trn.cl2)
log.response<- predict(log.fit, type="response")
log.pred <- prediction(log.response, trn.cl2$candidate)
log.perf <- performance(log.pred, measure="tpr", x.measure="fpr")

plot(log.perf, col=2, lwd=3, main="ROC curve for logistic")
abline(0,1)


prob.training <-round(predict(log.fit, type="response"))
log.train <- trn.cl2 %>%  mutate(candidate = as.factor(ifelse(prob.training == 0, 
                                                             "Donald Trump",'Hillary Clinton')))



prob.test <- round(predict(log.fit, tst.cl2, type = 'response'))
log.test <- tst.cl2 %>%  mutate(candidate = as.factor(ifelse(prob.test == 0, 
                                                             "Donald Trump",'Hillary Clinton')))
records[2, 1] <- calc_error_rate(log.train$candidate, trn.clY2)
records[2, 2] <- calc_error_rate(log.test$candidate, tst.clY2)
records


library(glmnet)
?cv.glmnet
trn.cl2 %>% select(-candidate) 
?subset
cv.lasso <-cv.glmnet( x = as.matrix(trn.clX2),y = as.factor(trn.clY2),alpha = 1, 
                    lambda = c(1, 5, 10, 50) * 1e-4,
                    family = "binomial")

plot(cv.lasso)
summary(cv.lasso)
##optimal lambda is 1
# trying with optimal lambda:
lassoOptimal <-glmnet( x = as.matrix(trn.clX2),y = as.factor(trn.clY2),alpha = 1, 
                      lambda = cv.lasso$lambda.1se,
                      family = "binomial")
row.names(coef(lassoOptimal) != 0)

x.trn <- model.matrix(candidate ~., trn.cl2)[,-1]
lassoTrn.probs <-round(predict(lassoOptimal, type="response", newx = x.trn))
lasso.train <- trn.cl2 %>%  mutate(candidate = as.factor(ifelse(lassoTrn.probs == 0, 
                                                              "Donald Trump",'Hillary Clinton')))
records[3, 1] <-calc_error_rate(lasso.train$candidate, trn.clY2)

x.tst <- model.matrix(candidate ~., tst.cl2)[,-1]
lassoTst.probs <-round(predict(lassoOptimal, type="response", newx = x.tst))
lasso.test <- tst.cl2 %>%  mutate(candidate = as.factor(ifelse(lassoTst.probs == 0, 
                                                                "Donald Trump",'Hillary Clinton')))
records[3, 2] <-calc_error_rate(lasso.test$candidate, tst.clY2)
records
#### overall, the training and test errors are larger for lasso regression


pred.cantree.test <- predict(prunedtree, tst.clX2, type="class")
tree.pred <- prediction(as.numeric(pred.cantree.test), tst.clY2)
tree.perf <- performance(tree.pred, measure = "tpr",x.measure = "fpr")

log.pred <- prediction(prob.test, tst.clY2)
lasso.pred <- prediction(lassoTst.probs, tst.clY2)

lasso.perf<-performance(lasso.pred, measure="tpr", x.measure="fpr")
log.perf<- performance(log.pred, measure="tpr", x.measure="fpr")


plot(tree.perf, col=1, lwd=3, main="ROC curve for tree")
abline(0,1)
plot(log.perf, col=2, lwd=3, main="ROC curve for logistic", add = TRUE)
abline(0,1)
plot(lasso.perf, col=3, lwd=3, main="ROC curve for lasso", add = TRUE)
abline(0,1)

##logistic has best ROC curve, while tree has second best and lasso has the worst
## logistic is the best for classifying if one will vote for Clinton or Trump
# but tree would be a good compromise between the predictability of logistic
# regression and interpretability as given by the pruned tree visual
# lasso would be good for the case to prevent overfitting, but in this case
# logistic regression seems to be performing better with the potential of overfitting
# hence, there is a lesser need for lasso regression





library(e1071)
tune.out<-tune(svm,candidate~.,data=trn.cl2,kernel="linear",
              ranges=list(cost=c(0.001, 0.01, 0.1,1,5,10,100)))
summary(tune.out)
#min error is cost of 0.1
bestmod<-tune.out$best.model
summary(bestmod)

ypred<-predict(bestmod,tst.clX2, type = "class")
#table of predictions
table(predict=ypred, truth=tst.clY2)
svm.pred <- prediction(as.numeric(ypred), tst.clY2)
svm.perf<- performance(svm.pred, measure="tpr", x.measure="fpr")

#comparing svm with previously used models
plot(tree.perf, col=1, lwd=3, main="ROC curve for tree")
abline(0,1)
plot(log.perf, col=2, lwd=3, main="ROC curve for logistic", add = TRUE)
abline(0,1)
plot(lasso.perf, col=3, lwd=3, main="ROC curve for lasso", add = TRUE)
abline(0,1)
plot(svm.perf, col=4, lwd=3, main="ROC curve for SVM", add = TRUE)
abline(0,1)

# we can see that svm performs slightly worse than logistic and the tree models, but still better than
# the lasso method. 
