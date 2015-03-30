setwd('E:/Analytics/kaggle/titanic')

train <- read.csv('train.csv', header = T)
test <- read.csv('test.csv', header = T)
str(train)
table(train$Survived)
prop.table(table(train$Survived))

test$survived <- rep(0, 418)

submit <- data.frame(PassengerID = test$PassengerId, Survived = test$survived)
write.csv(submit, file = 'theyalldie.csv', row.names = F)

summary(train$Sex)
prop.table(table(train$Sex, train$Survived))

prop.table(table(train$Sex, train$Survived),1)

test$survived <- 0
test$survived[test$Sex == 'female'] <- 1


submit <- data.frame(PassengerID = test$PassengerId, Survived = test$survived)
write.csv(submit, file = 'onlyfemalesurvived.csv', row.names = F)

#deep dive to study child

summary(train$Age)

train$Child <- 0
train$Child[train$Age < 18] <- 1

aggregate(Survived ~ Child + Sex, train = train, FUN = sum)
aggregate(Survived ~ Child + Sex, train = train, FUN = length)
aggregate(Survived ~ Child + Sex, train = train, FUN = function(x) {sum(x)/length(x)})


#deepdive into fares and passenger class

str(train$Pclass)

train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

aggregate (Survived ~ Fare2 + Pclass + Sex, train = train, FUN = function(x) {sum(x)/length(x)})

test$survived <- 0
test$survived[test$Sex == 'female'] <- 1
test$survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

submit <- data.frame(PassengerID = test$PassengerId, Survived = test$survived)
write.csv(submit, file = 'analysisonfareandclass.csv', row.names = F)

#decision trees

library(rpart)
fit <- rpart(train$Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, train, method="class")
plot(fit)
text(fit)

install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)


fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

#playing with the default values, overfitting example
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, train,
             method="class", control=rpart.control(minsplit=5, cp=0))
fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myseconddtree.csv", row.names = FALSE)

#interactive decision tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control( your controls ))
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)

Prediction <- predict(new.fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "interactivetree.csv", row.names = FALSE)

#featured engineering

train <- read.csv('train.csv', header = T)
test <- read.csv('test.csv', header = T)

test$Survived <- NA
combi <- rbind(train, test)

str(train)
combi$Name <- as.character(combi$Name)
combi$Name[1]

strsplit(combi$Name[1], '[,.]')[[1]]
strsplit(combi$Name[1], split='[,.]')[[1]][2]

combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})

combi$Title <- sub(' ', '', combi$Title)

table(combi$Title)

 #combining likewise titles
combi$Title[combi$Title  %in% c('Mlle', 'Mme')] <- 'Mlle'
combi$Title[combi$Title  %in% c('Don','Sir','Jonkheer','Dr')] <- 'Sir'
combi$Title[combi$Title  %in% c('Capt','Col','Major')] <- 'Armymen'
combi$Title[combi$Title  %in% c('Dona', 'Lady','the Countess')] <- 'Lady'

combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1

#playing with surname and family size
combi$Surname <- sapply(combi$Name, FUN = function(x) {strsplit((x), split = '[,.]')[[1]][1]})

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep = "")

combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)

famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

# splitting test and training set to learn the decision tree
train <- combi[1:891,]
test <- combi[892:1309,]


fit <- rpart(Survived ~ Pclass + Sex + Age + Fare + Title, train, method="class")
fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "featuredddtree.csv", row.names = FALSE)

#random forest

sample(1:10, replace = T)
summary(combi$Age)

#replacing NA in age with decsion tree support

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data = combi[!is.na(combi$Age),], method = "anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

summary(combi)

#there are 2 values in Embarked as blank, lets replace them with any of the values

which(combi$Embarked == "")
combi$Embarked[c(62, 830)] <- "S"
combi$Embarked <- factor(combi$Embarked)

#in the fare variable there is one NA, lets replace it with median value
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm = T)

#reducing the factors for family ID as random forest in R can take upto 32 factors

summary(combi$FamilyID)
nlevels(combi$FamilyID)

combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <=3] <- "Small"
combi$FamilyID2 <- factor(combi$FamilyID2)
nlevels(combi$FamilyID2)

train <- combi[1:891,]
test <- combi[892:1309,]

#we are ready to create our random forest

install.packages('randomForest')
library(randomForest)

#do not alter the number
set.seed(415)

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
                      FamilyID2, data=train, importance=TRUE, ntree=2000)

#imporatnce argument will tell us the imprtance of the variables
varImpPlot(fit)

Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)

#conditionl inference trees
#lets try another ensamble model where decision is made based on statistical significance rather on purity measure
install.packages('party')
library(party)

set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "secondforest.csv", row.names = FALSE)
