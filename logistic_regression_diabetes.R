
#read data
data = read.csv(file.choose())

data = read.csv(file ="C:/Users/VIJAY/Desktop/ML/logistic regression/diabetes.csv")
                
head(data)

library(caTools)
set.seed(1000) #seetin seed

split = sample.split(data$Outcome, SplitRatio = .75) # splitting data as training and testing

train = subset(data,split = TRUE) #trainig data

test = subset(data,split = FALSE) #testing data

nrow(data) # finding out no of rows in dataset

model = glm(Outcome ~ Pregnancies + Glucose +BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, family = binomial, data = train) # modelling on train data

prediction = predict(model,type = 'response',newdata = test) # tesing the model on test data

head(cbind(data$Outcome,prediction)) 

prediction1 = ifelse(prediction >.5,1,0) # if the value of prediction variabel is >.5 make it as 1 otherwise make it as 0

head(cbind(prediction,prediction1))

cm =table(test[,9],predition1) # making the confusion matrix
cm

library(caret)

caret::confusionMatrix(cm)# gives the accuracy and other detail of the model through confusion matrix
