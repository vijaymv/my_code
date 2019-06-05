# CART on car set
library(ISLR)
Carseats
head(Carseats)

library(rpart)
library(rpart.plot)

#selecting data

data = Carseats
data
dim(data)

sam  = sample(400,300)

train =data[sam,]
test = data[-sam,]
dim(test)
dim(train)

# modelling using training data and method = "class" implies that we want to do classification
ifgood = ifelse(data$Sales > 8,"high","low")
#adding ifgood column to data
data = data.frame(data,ifgood)
head(data)

dtm = rpart(ifgood ~.-Sales,data = train, method = "class")

rpart.plot(dtm,cex =.7,type =4,extra = 101)

p = predict(dtm,test,type ="class")

cm3 = table(test[,12],p)
cm3
head(test)
confusionMatrix(cm3)

#checking column names of data
names(data)
#plotting the histogram
hist(data$Sales)
#setting value of ifgood variable as high is sales >8 else value will be low
ifgood = ifelse(data$Sales > 8,"high","low")
#adding ifgood column to data
data = data.frame(data,ifgood)
head(data)
# model
tree = rpart(ifgood ~.-Sales,data = data)
#plotting
rpart.plot(tree,cex=.7)
summary(tree)
#tryinh to find out best cp value & cp value with least xerror(cross validation error) is selected
printcp(tree)
plotcp(tree)
summary(tree)
#pruning using cp value with least xerror
prun = prune(tree,cp = .01)
#plotting pruned tree using prp
prp(prunetree,5)
#improving the plot
rpart_1 = rpart.plot(prun, nn=T, cex=.7, type=4,extra =101,main = "classification")

# create training set for testing
 set.seed(100)
 dim(data)
# making test data
 train =  sample(data)


tree_1 = rpart(ifgood ~.-Sales,data=data,subset = train)

tree_pred = predict(tree_1,newdata = train,type = 'class')
cm =table(train[,9],tree_pred)
#evaluating the error
library(caret)

confusionMatrix(cm)

# checking using a sample
train1 = sample(1:nrow(data),250)

tree_2 = rpart(ifgood~.-Sales,data,subset = train1)

tree2_pred = predict(tree_2,data= train1,type = 'class')

cm1 = table(train[,9],tree2_pred)
confusionMatrix(cm)


