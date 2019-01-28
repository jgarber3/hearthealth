library(fastDummies)
library(caret)


#Setup work including reading the source data
setwd("/Users/jgarber/git/datascience200/month2week4project")
dataSet <- read.csv("./HDdata.csv")

#about 3% of the observations have no age so remove them since filling seems unreliable and its only 3%
dataSet <- na.omit(dataSet)

#change the categorical values to factors
dataSet$cp <- as.factor(dataSet$cp)
dataSet$restecg <- as.factor(dataSet$restecg)
dataSet$sex <- as.factor(dataSet$sex)
dataSet$num <- as.factor(dataSet$num)

#make dummy variables for the categorical features
#it should be noted that I'm slightly concerned using an identifier called sex_dummies in my code
sex_dummies <- dummy_cols(dataSet$sex)
dataSet$male = sex_dummies[,2]
#dataSet$female = sex_dummies[,3]

cp_dummies <- dummy_cols(dataSet$cp)
dataSet$typical_angina <- cp_dummies[,2]
dataSet$atypical_angina <- cp_dummies[,3]
dataSet$non_angina <- cp_dummies[,4]
#dataSet$asymptomatic <- cp_dummies[,5]

bs_dummies <- dummy_cols(dataSet$fbs)
dataSet$high_sugar <- bs_dummies[,2]
#dataSet$low_sugar <- bs_dummies[,3]

ecg_dummies <- dummy_cols(dataSet$restecg)
dataSet$normal_ecg <- ecg_dummies[,2]
dataSet$wave_ecg <- ecg_dummies[,3]
#dataSet$estes_ecg <- ecg_dummies[,4]

#drop out the original categorical columns
dataSet <- subset(dataSet, select = -c(sex,cp,fbs,restecg))

#Cut into training and test sets
train <- dataSet[1:203,]
test <- dataSet[204:293,]

#fit both types of models
logistic_fit <- glm(num ~., family=binomial, data=train)

#get predictions out of the logistic model
#note: When using predictions() i get a warning that says my data is rank-deficient. Stack overflow seems to suggest that
#predictions() removes features and if you get less observations than features this warning is issued. This does not seem
#to be the case here and i dont know if this warning is pointing out a problem I'm missing. <---- JOE LOOK HERE
predictions <- predict(logistic_fit, test, type="response")

#set a prediction threshold
y_or_n <- ifelse(predictions > .5, 1, 0)
#set the factory between the predictions the same as to the response variable
p_class <- factor(y_or_n, levels = levels(test$num))
confusionMatrix(p_class,test$num)
