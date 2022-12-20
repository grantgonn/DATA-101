##reading csv
sp = read.csv(file = 'StudentsPerformance.csv')

##factor dummy variables
sp$gender <- factor(sp$gender)
sp$parental.level.of.education <- factor(sp$parental.level.of.education)
sp$lunch <- factor(sp$lunch)
sp$test.preparation.course <- factor(sp$test.preparation.course)

##drop race.ethnicity variable
sp <- subset (sp, select = -race.ethnicity)
str(sp)

##boxplots of test scores vs. variables
boxplot(math.score ~ gender, data = sp)
boxplot(math.score ~ parental.level.of.education, data = sp)
boxplot(math.score ~ lunch, data = sp)
boxplot(math.score ~ test.preparation.course, data = sp)

boxplot(reading.score ~ gender, data = sp)
boxplot(reading.score ~ parental.level.of.education, data = sp)
boxplot(reading.score ~ lunch, data = sp)
boxplot(reading.score ~ test.preparation.course, data = sp)

boxplot(writing.score ~ gender, data = sp)
boxplot(writing.score ~ parental.level.of.education, data = sp)
boxplot(writing.score ~ lunch, data = sp)
boxplot(writing.score ~ test.preparation.course, data = sp)

##histograms of test scores with 5 number summaries 
hist(sp$math.score)
fivenum(sp$math.score)

hist(sp$reading.score)
fivenum(sp$reading.score)

hist(sp$writing.score)
fivenum(sp$writing.score)

##scatter plots of test scores with correlation coefficients
plot(sp$math.score, sp$reading.score)
cor(sp$math.score, sp$reading.score)

plot(sp$math.score, sp$writing.score)
cor(sp$math.score, sp$writing.score)

plot(sp$reading.score, sp$writing.score)
cor(sp$reading.score, sp$writing.score)

##test scores with all variables 
lm_math <- lm(math.score  ~ ., data = sp)
summary(lm_math)

lm_writing <- lm(writing.score  ~ ., data = sp)
summary(lm_writing)

lm_reading <- lm(reading.score  ~ ., data = sp)
summary(lm_reading)

##forward selection for each score to find best model of each score
Naive <- lm(math.score ~ 1, data = sp)
Initial <- lm(math.score ~ gender, data = sp)
Complex <- lm(math.score ~ ., data = sp)
result <- step(Initial, scope = list(lower = Naive, upper = Complex), direction = "forward")

Naive <- lm(writing.score ~ 1, data = sp)
Initial <- lm(writing.score ~ gender, data = sp)
Complex <- lm(writing.score ~ ., data = sp)
result <- step(Initial, scope = list(lower = Naive, upper = Complex), direction = "forward")

Naive <- lm(reading.score ~ 1, data = sp)
Initial <- lm(reading.score ~ gender, data = sp)
Complex <- lm(reading.score ~ ., data = sp)
result <- step(Initial, scope = list(lower = Naive, upper = Complex), direction = "forward")

##summaries of best model for each score 
lm_math <- lm(math.score  ~ gender + writing.score + test.preparation.course + lunch + reading.score + parental.level.of.education, data = sp)
summary(lm_math)

lm_writing <- lm(writing.score  ~ gender + reading.score + math.score + test.preparation.course + parental.level.of.education, data = sp)
summary(lm_writing)

lm_reading <- lm(reading.score  ~ gender + writing.score + math.score + test.preparation.course + parental.level.of.education + lunch, data = sp)
summary(lm_reading)

##What role does gender have in the outcome of exam scores?
lm_math <- lm(math.score  ~ gender, data = sp)
summary(lm_math)

lm_writing <- lm(writing.score  ~ gender, data = sp)
summary(lm_writing)

lm_reading <- lm(reading.score  ~ gender, data = sp)
summary(lm_reading)

##Does having a reduced lunch affect the test score?
lm_math <- lm(math.score  ~ lunch, data = sp)
summary(lm_math)

lm_writing <- lm(writing.score  ~ lunch, data = sp)
summary(lm_writing)

lm_reading <- lm(reading.score  ~ lunch, data = sp)
summary(lm_reading)

##Does having compleated the test preparation course affect the test score?
lm_math <- lm(math.score  ~ test.preparation.course, data = sp)
summary(lm_math)

lm_writing <- lm(writing.score  ~ test.preparation.course, data = sp)
summary(lm_writing)

lm_reading <- lm(reading.score  ~ test.preparation.course, data = sp)
summary(lm_reading)

##Predicting likelihood of compleating the test preparation course  
##building the logistic reg model
logit_md = glm(test.preparation.course ~ math.score, data = sp, family = 'binomial')
newdata = data.frame('math.score'= 50)
predict(logit_md, newdata, type = 'response')

logit_md = glm(test.preparation.course ~ writing.score, data = sp, family = 'binomial')
newdata = data.frame('writing.score'= 50)
predict(logit_md, newdata, type = 'response')

logit_md = glm(test.preparation.course ~ reading.score, data = sp, family = 'binomial')
newdata = data.frame('reading.score'= 50)
predict(logit_md, newdata, type = 'response')

##spliting data to training and testing
n = dim(sp[1])
train_index = sample(1:n, round(.8*n))
train = sp[train_index, ]
test = sp[-train_index, ]

library(caret)
knn_md = knnreg(writing.score  ~ gender + reading.score + math.score 
                + test.preparation.course + parental.level.of.education, data = train, k=5)

##predicting on test data
knn_pred = predict(knn_md, test)
knn_rmse = sqrt(mean((knn_pred - test$math.score)^2))
knn_rmse
knn_mae = mean(abs(knn_pred - test$math.score))
knn_mae

##random forest
library(randomForest)
RF_md = randomForest(writing.score  ~ gender + reading.score + math.score + test.preparation.course 
                     + parental.level.of.education, data = train, ntree = 500)

RF_pred = predict(RF_md, test)
RF_rmse = sqrt(mean((RF_pred - test$math.score)^2))
RF_rmse
RF_mae = mean(abs(RF_pred - test$math.score))
RF_mae

##gradient boosting
library(gbm)
gbm_md = gbm(writing.score  ~ gender + reading.score + math.score + test.preparation.course 
             + parental.level.of.education, data = train, distribution = "gaussian", n.trees = 500, interaction.depth = 4)

gbm_pred = predict(gbm_md, test, n.trees = 500 )
gbm_rmse = sqrt(mean((gbm_pred - test$math.score)^2))
gbm_rmse
gbm_mae = mean(abs(gbm_pred - test$math.score))
gbm_mae

##presenting values for each model 
model <- c("KNN", "RF", "GBM")
RMSE <- c(knn_rmse, RF_rmse, gbm_rmse)
MAE <- c(knn_mae, RF_mae, gbm_mae)
df <- data.frame(model, RMSE, MAE)
print(df)









