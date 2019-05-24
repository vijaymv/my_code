# logistic regression

url="https://docs.google.com/spreadsheets/d/1Md_ro2t3M7nA9JMH1DsE12jfeX7qq-UPw6p8WQd6A2Y/edit#gid=120271978"

library(gsheet)

data = as.data.frame(gsheet2tbl(url))
View(data)
head(data)

tail(data)
data$gender= factor(data$gender)

data$gender

library(caTools)
set.seed(1000)
split = sample.split(data$purchased,SplitRatio = .75)
train = subset(data, split == TRUE) 
test = subset(data, split == FALSE)
nrow(data)

model = glm(purchased ~ age + salary, family =  binomial, data = train)
summary(model)

testset = data.frame(age=c(40,50),gender= c("Male","Female"),salary= c(70000,25000))
testset
prediction1 = predict(model,type = 'response', newdata = testset)                     
cbind(testset,prediction1)

prediction2 = predict(model,type = 'response', newdata = test)
head(cbind(test,prediction2))

summary(prediction2)

prediction3 = ifelse(prediction2 > .5,1,0)
cbind(prediction2,prediction3)
head(cbind(data$purchased,prediction3))

# comfusion matrix

cm = table(test[,5],prediction3)
cm

library(caret)
caret :: confusionMatrix(cm)
