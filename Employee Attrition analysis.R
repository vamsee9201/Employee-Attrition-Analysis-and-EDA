
library(dplyr)
employeeAttrition <- read.csv("employeeAttrition.csv",header = TRUE, stringsAsFactors = FALSE)
head(employeeAttrition)
ncol(employeeAttrition)
nrow(employeeAttrition)

summary(employeeAttrition)

head(employeeAttrition$Department)

class(employeeAttrition$EnvironmentSatisfaction)

#convert categorical data to numeric
employeeAttrition$AttritionCat <- 0
employeeAttrition$AttritionCat[employeeAttrition$Attrition == "Yes"] <- 1
employeeAttrition$AttritionCat[employeeAttrition$Attrition == "No"] <- 0
head(employeeAttrition)
employeeAttrition$Over18Cat <- 0
employeeAttrition$Over18Cat[employeeAttrition$Over18 == "Y"] <- 1
employeeAttrition$Over18Cat[employeeAttrition$Over18 == "N"] <- 0

head(employeeAttrition)

numericVals <- select_if(employeeAttrition, is.numeric)
head(numericVals)

head(numericVals)
#removing outliers from the data

data <- numericVals

library(reshape)
meltData <- melt(data)
boxplot(data=meltData, value~variable)


#removing the outlliers with boxplot method

outlier_removal <- numericVals
#removal of outliers

boxplot(outlier_removal)$out

ggbetweenstats(outlier_removal,
               wool, breaks, outlier.tagging = TRUE)


#removing unnecessary features

"drop <- ('EmployeeCount','EmployeeNumber','Over18','StandardHours')
drop <- ('Age',
         'DailyRate',
         'DistanceFromHome',
         'Education',
         'EnvironmentSatisfaction',
         'HourlyRate',
         'JobInvolvement',
         'JobLevel',
         'JobSatisfaction',
         'MonthlyIncome',
         'MonthlyRate',
         'NumCompaniesWorked',
         'PercentSalaryHike',
         'PerformanceRating',
         'RelationshipSatisfaction',
         'StockOptionLevel',
         'TotalWorkingYears',
         'TrainingTimesLastYear',
         'WorkLifeBalance',
         'YearsAtCompany',
         'YearsInCurrentRole',
         'YearsSinceLastPromotion',
         'YearsWithCurrManager')"

vector <- c('EnvironmentSatisfaction','JobSatisfaction','RelationshipSatisfaction','JobInvolvement','PerformanceRating',
            'StockOptionLevel','WorkLifeBalance','EmployeeCount','EmployeeNumber','Over18','StandardHours','AttritionCat', 'Over18Cat'
)

"
drop <- ('Attrition',
         'BusinessTravel',
         'Department',
         'EducationField',
         'Gender',
         'JobRole',
         'MaritalStatus',
         'OverTime')"

#correlation matrix



library(Hmisc)

data1 <- na.omit(numericVals)
data1 <- data[is.finite(rowSums(numericVals)),]
corr_matrix <- rcorr(as.matrix(numericVals))
print(corr_matrix)


#heat map of correlation matrix
library(reshape2)
corr_mat <- round(cor(corr_matrix),2)

#cleanEmployeeAttritionDataset


employeeAttritionClean <- employeeAttrition[,!(names(employeeAttrition)%in%vector)]

#summary of the data
summary(employeeAttritionClean)

#decision tree
head(employeeAttritionClean)
set.seed(345)
train = sample(1:nrow(employeeAttritionClean), nrow(employeeAttritionClean)*(2/3))
Attrition.train = employeeAttritionClean[train,]
Attrition.test = employeeAttritionClean[-train,]
nrow(Attrition.train)
nrow(Attrition.test)

fit = rpart(Attrition ~ ., data=Attrition.train,
            method="class", control=rpart.control(xval=0, minsplit=50),
            # formula
            # dataframe used
            # treat survived as a categorical variable, default
            # xval: num of cross validation
            # minsplit=100: stop splitting if node has 100 or fewer observations
            parms=list(split="gini"))
fit
library(rpart.plot)
rpart.plot(fit, type = 1, extra = 1)


#accuracy on training and testing data

# Accuracy on the Training Data
Attrition.pred <- predict(fit, Attrition.train, type="class") 
Attrition.actual <- Attrition.train$Attrition
confusion.matrix <- table(Attrition.pred, Attrition.actual) 
pt <- prop.table(confusion.matrix)
#accuracy
pt[1,1] + pt[2,2]
# Accuracy on the Testing data
Attrition.pred <- predict(fit, Attrition.test, type="class")
Attrition.actual <- Attrition.test$Attrition
confusion.matrix <- table(Attrition.pred, Attrition.actual) 
addmargins(confusion.matrix)
pt <- prop.table(confusion.matrix)
#accuracy
pt[1,1] + pt[2,2]




#logistic regression

attritionDf <- employeeAttritionClean
attritionDf$Attrition <- as.factor(attritionDf$Attrition)
set.seed(2)
train <- sample(1:nrow(attritionDf), (0.6)*nrow(attritionDf)) 
train.df <- attritionDf[train,]
test.df <- attritionDf[-train,]
nrow(train.df)
nrow(test.df)


logit.reg <- glm(Attrition ~ Age + DailyRate + Education + HourlyRate + MaritalStatus + NumCompaniesWorked
                 + TotalWorkingYears + YearsInCurrentRole + Department + EducationField + JobLevel + MonthlyIncome + OverTime + TrainingTimesLastYear + YearsSinceLastPromotion +BusinessTravel + DistanceFromHome+Gender +JobRole +MonthlyRate +PercentSalaryHike + YearsAtCompany+ YearsWithCurrManager, data = train.df, family = "binomial")
print(logit.reg)
summary(logit.reg)


colnames(attritionDf)

logitPredict <- predict(logit.reg, train.df, type = "response")
logitPredictClass <- ifelse(logitPredict > 0.5, 1, 0)
actual <- train.df$Attrition
predict <- logitPredictClass
cm <- table(predict, actual)
tp <- cm[2,2] 
tn <- cm[1,1] 
fp <- cm[2,1] 
fn <- cm[1,2]
#accuracy 
accuracy <- (tp + tn)/(tp + tn + fp + fn);accuracy
#TPR
tpr <- tp/(fn+tp);tpr
#TNR
tnr <- tn/(fp+tn);tnr
#FPR
fpr <- fp/(fp+tn);fpr
#FNR
fnr <- fn/(fn+tp);fnr



logitPredict <- predict(logit.reg, test.df, type = "response")
logitPredictClass <- ifelse(logitPredict > 0.5, 1, 0)
actual <- test.df$Attrition
predict <- logitPredictClass
cm <- table(predict, actual)
tp <- cm[2,2] 
tn <- cm[1,1] 
fp <- cm[2,1] 
fn <- cm[1,2]
#accuracy
accuracy <- (tp + tn)/(tp + tn + fp + fn);accuracy
#TPR
tpr <- tp/(fn+tp);tpr
#TNR
tnr <- tn/(fp+tn);tnr
#FPR
fpr <- fp/(fp+tn);fpr
#FNR
fnr <- fn/(fn+tp);fnr


