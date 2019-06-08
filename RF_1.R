url = "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

d <- read.csv(url, header = FALSE)
head(d)

colnames(d) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","hd")

head(d)
str(d)

d[d== "?"] = NA

d[d$sex ==0,]$sex = "FEMALE"

head(d$sex)
d[d$sex ==1,]$sex = "MALE"

d$sex = factor(d$sex)
d$cp = factor(d$cp)
d$fbs = factor(d$fbs)
d$restecg = factor(d$restecg)
d$exang =factor(d$exang)
d$slope = factor(d$slope)
d$ca = as.integer(d$ca)
d$ca = factor(d$ca)
d$thal = as.integer(d$thal)
d$thal = factor(d$thal)
d$hd = ifelse(d$hd == 0,"yes","NO")
d$hd = factor(d$hd)
d$trestbps = integer(d$trestbps)

str(d)

set.seed(199)

library(randomForest)


# imputing values in place of missing values
  
data.imputed <- rfImpute(hd~.,data = d,iter=6) # prints out oob(out of the bag) error rate

# making random forest model.

model = randomForest(hd~.,data = data.imputed,proximity = TRUE) # proximity = TRUE will return proximity matrix
importance(model,type = 2) #cp,ca,thalach,thal are the important factors for deciding if some one is having heart desease or not.

model
head(model$err.rate) # to  oob error

trees =rep(1:nrow(model$err.rate),times = 3)  
head(trees)
Type=rep(c("OOB","NO","YES"),each=nrow(model$err.rate))
type
Error=c(model$err.rate[,"OOB"],model$err.rate[,"NO"],model$err.rate[,"yes"])

oob.error.rate <- data.frame(trees,Type,Error)
tail(oob.error.rate)
head(oob.error.rate)

library(ggplot2)

ggplot(oob.error.rate,aes(x = trees,y=Error,color=Type)) + geom_line() # after 200 trees error rate stabilized. The error rate was decresing till then
  
# tring change no of variables tried at each split and check if there is any improvement in the model.

oob.values <- vector(length = 10)
oob.values

for( i in 1:10) {
  temp.model <- randomForest(hd~.,data = data.imputed,mtry=i, ntree=1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1] # select the oob error when all 1000 trees have been made 
  
}
oob.values # 3rd value for mtry which is the default value has the lowest error rate. 
