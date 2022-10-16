setwd("D:/STUDIES/BA with R/PROJECT")

#Read the data file
hmeq_read<- read.csv("hmeq.csv")

#Delete rows having too many missing values (5 or more missing values)
data_new1 <- hmeq_read                               # Duplicate data frame
data_new1[hmeq_read == ""] <- NA                     # Replace blank by NA

hmeq <- data_new1[rowSums(is.na(data_new1)) <=5, ]   # Cleaned dataset

#Imputation
summary(hmeq)
hmeq$MORTDUE[is.na(hmeq$MORTDUE)] <-  median(hmeq$MORTDUE, na.rm = TRUE)
hmeq$VALUE[is.na(hmeq$VALUE)] <-  median(hmeq$VALUE, na.rm = TRUE)
hmeq$REASON[is.na(hmeq$REASON)] <- 'HomeImp'
hmeq$JOB[is.na(hmeq$JOB)] <- 'Other'
hmeq$YOJ[is.na(hmeq$YOJ)] <-  median(hmeq$YOJ, na.rm = TRUE)
hmeq$DEROG[is.na(hmeq$DEROG)] <-  median(hmeq$DEROG, na.rm = TRUE)
hmeq$DELINQ[is.na(hmeq$DELINQ)] <-  median(hmeq$DELINQ, na.rm = TRUE)
hmeq$CLAGE[is.na(hmeq$CLAGE)] <-  mean(hmeq$CLAGE, na.rm = TRUE)
hmeq$NINQ[is.na(hmeq$NINQ)] <-  median(hmeq$NINQ, na.rm = TRUE)
hmeq$CLNO[is.na(hmeq$CLNO)] <-  median(hmeq$CLNO, na.rm = TRUE)
hmeq$DEBTINC[is.na(hmeq$DEBTINC)] <-  median(hmeq$DEBTINC, na.rm = TRUE)

#Creating training & validation data sets (70% Training & 30% Validation)
set.seed(10)
hmeq_data <- sort(sample(nrow(hmeq), nrow(hmeq)*.7))
train.df <- hmeq[hmeq_data,]
valid.df <- hmeq[-hmeq_data,]

#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

#Plotting the decision tree

#Without pruning
default.ct <- rpart(BAD ~ ., data = train.df, method = "class")
prp(default.ct,type = 1, extra=1, varlen=10, under = TRUE)

#With maxdepth of 2
default.ct <- rpart(BAD ~ ., data = train.df,maxdepth = 2, method = "class")
prp(default.ct,type = 1, extra=1, varlen=10, under = TRUE)

#Confusion matrix for Training data
library(caret)
default.ct.pred.train <- predict(default.ct,train.df,type = "class")
confusionMatrix(default.ct.pred.train, as.factor(train.df$BAD), positive = "1")

#Confusion matrix for Validation data

default.ct.pred.valid <- predict(default.ct,valid.df,type = "class")
confusionMatrix(default.ct.pred.valid, as.factor(valid.df$BAD), positive = "1")

#ROC for Decision Tree
library(pROC)
default.ct <- rpart(BAD ~ ., data = train.df,maxdepth = 2, method = "class")
roc.default.ct.pred.valid <- predict(default.ct, valid.df,type = "prob")
r <- roc(valid.df$BAD,roc.default.ct.pred.valid[,2])

plot.roc(r, percent= TRUE, main = "ROC Curve", print.auc = TRUE, col = "blue")
auc(r)

#LOGIT METHOD

#Data for the logit model
hmeq_logit_data<- hmeq_data

#Training & validation dataset for the logit model
logittrain.df <- train.df
logitvalid.df <- valid.df

#General linear model (glm) with family = "binomial" to fit logistic regression
logit.reg <- glm(BAD ~ ., data = logittrain.df, family = "binomial") 
options(scipen=999)
summary(logit.reg)

#Confusion matrix for training data
logit.reg.pred.train <- predict(logit.reg, logittrain.df, type = "response")
confusionMatrix(as.factor(ifelse(logit.reg.pred.train > 0.4, 1, 0)), as.factor(train.df$BAD), positive = "1")

#Confusion matrix for validation data
logit.reg.pred.valid <- predict(logit.reg, logitvalid.df, type = "response")
confusionMatrix(as.factor(ifelse(logit.reg.pred.valid > 0.4, 1, 0)), as.factor(valid.df$BAD), positive = "1")

#ROC for Logistic Regression
r1 <- roc(logitvalid.df$BAD, logit.reg.pred.valid)
plot.roc(r1, percent= TRUE, main = "ROC Curve", print.auc = TRUE, col = "blue")
auc(r1)

#Plot Lift chart
prob <- predict(logit.reg, newdata = logitvalid.df, type = "response")
#install.packages("gains")
library(gains)
gains <- gains(logitvalid.df$BAD, prob, groups = 100)

y<- gains$cume.pct.of.total*sum(logitvalid.df$BAD)
x<- gains$cume.obs
y<-c(0,y)
x<-c(0,x)
xy.df <- data.frame(x,y)

x<- c(0, nrow(logitvalid.df))
y<- c(0, sum(logitvalid.df$BAD))
b.df<- data.frame(x,y)

ggplot(data= NULL, mapping = aes(x,y)) + geom_line(data = xy.df) + geom_line(data = b.df, linetype = "dashed") +
  ggtitle("Lift Chart") + xlab("Number of Applicants") + ylab("Cumulative Positives") 

#Decile-wise Lift Chart
gains <- gains(logitvalid.df$BAD, prob, groups = 10)

#Decile-wise Lift chart using ggplot
lift.df<- data.frame(gains$lift/100, as.factor(gains$depth))
names(lift.df) <- c("Lift", "Depth")

ggplot(data = lift.df, mapping = aes(x= Depth, y = Lift)) + geom_bar(stat = "identity") +
  geom_text(aes(label= Lift), vjust= -0.25) + xlab("Depth") + ylab("Lift")