
dt = read.csv("C:/Users/VIJAY/Desktop/ML/logistic regression/diabetes.csv")
head(dt)
str(dt)
dt$Outcome <- factor(dt$Outcome, levels = c(0,1),labels = c("NO","YES") )
dt$Outcome
dim(dt)
80*768
dt$Glucose
set.seed(1000)
s <- sample(768,610)
train <- dt[s,]
test <- dt[-s,]
dim(train)

dim(test)

library(randomForest)
library(ggplot2)
library(caret)

colSums(is.na(dt))

fit = randomForest(Outcome~.,data = train, ntree=500)
fit
summary(fit)
importance(fit) # important factor for testing if the patient is diabetic or not is Glucose & BMI

head(fit$err.rate) # to get oob error

tree.no = rep(1:nrow(fit$err.rate),time=3)
head(tree.no)
type.error = rep(c("OOB","NO","YES"), each= nrow(fit$err.rate))
nrow(fit$err.rate)
nrow(cbind(type.error))
head(type.error)
error.rate = c(fit$err.rate[,"OOB"],fit$err.rate[,"NO"],fit$err.rate[,"YES"])
head(cbind(error.rate))

ndt = data.frame(tree.no,type.error,error.rate)
head(ndt)

ggplot(ndt,aes(x=tree.no,y=error.rate,color=type.error)) + geom_line()

p <- predict(fit,test, type = "class")
nrow(cbind(p))
nrow(test)

cm  = table(test$Outcome,p)
cm

confusionMatrix(cm) #accuracy = 77%, SENSITIVITY = PRECISION = 80%, RECALL = POS PRED VALUE = 86%


# TRYING DECISION TREE FOR THE SAME DATA SET

library(rpart)
library(rpart.plot)

fit1 <- rpart(Outcome~.,data = train, method = "class")
summary(fit1)

rpart.plot(fit1, cex = .7, extra = 4)

p1 <- predict(fit1,test,type = "class")
p1

cm2 =table(test[,9],p1)
cm2
confusionMatrix(cm2) # accuracy = 75%, precision = 79%, recall =83%
printcp(fit1) # xerror less for cp=.019
#pruning tree

pr <- prune(fit1,cp = .019)


rpart.plot(pr, cex = .7, extra = 101)
pr
summary(pr)
p2 <- predict(pr,test,type = "class")

cm3 =  table
cm3
confusionMatrix # accuracy = 74%, precision =73%, recall = 91%
