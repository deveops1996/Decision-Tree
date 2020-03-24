library(party)
library(plyr)
library(readr)
library(datasets)
library(ggplot2)
library(GGally)
library(moments)
library(party)
library(caret)


company_data <- read.csv(file.choose())
View(company_data)
attach(company_data)
summary(company_data)
plot(company_data)
ggpairs(company_data)
ggplot(data = company_data, mapping = aes(x = Sales, y = Price)) + 
  geom_point(aes(color = ShelveLoc ),size = 2, alpha = 0.5, position = "jitter") +
  xlab("Sales") +
  ylab("Price")
ggplot(data = company_data, mapping = aes(x = Sales, y = Age)) + 
  geom_point(aes(color = ShelveLoc),size = 2, alpha = 0.5, position = "jitter") +
  xlab("Sales") +
  ylab("Age")

base_plot <- ggplot(data = company_data, mapping = aes(x = Sales, y = Price)) + 
  geom_point(aes(color = ShelveLoc), size = 3, alpha = 0.5, position = "jitter") +
  xlab("Sales") +
  ylab("Price") +
  
base_plot
base_plot + theme_light()


base_plot <- ggplot(data = company_data, mapping = aes(x = Sales, y = Age)) + 
  geom_point(aes(color = ShelveLoc), size = 3, alpha = 0.5, position = "jitter") +
  xlab("Sales") +
  ylab("Price") +
  
base_plot
base_plot + theme_light()



# Low Sales less than 9, medium in between 9 & 11, High sales above 11
company_data$Sales <- cut(company_data$Sales,
                     breaks=c(-Inf, 9, 11, Inf),
                     labels=c("low","medium","high"))
summary(Sales)
summary(company_data)
View(company_data)

tree1<-ctree(Sales~., data=company_data)  
plot(tree1)

tree2<-ctree(Sales~CompPrice, data=company_data)
plot(tree2)

tree3<-ctree(Sales~Income+Price, data=company_data)
plot(tree3)

tree4<-ctree(Sales~Income+Price+Urban, data=company_data)
plot(tree4)

tree5<-ctree(Sales~Income+Education+Price+Urban, data=iris)
plot(tree5)

tree6<-ctree(Sales~ShelveLoc, data=company_data)
plot(tree6)

tree7<-ctree(Sales~Income+ShelveLoc, data=company_data)
plot(tree7)

tree8<-ctree(Sales~ShelveLoc+CompPrice, data=company_data)
plot(tree8)

tree9<-ctree(Sales~ShelveLoc+CompPrice+Income+Price+Age, data=company_data)
plot(tree9)


tree10<-ctree(Sales~ShelveLoc+CompPrice+Income+Price+Age+US, data=company_data)
plot(tree10)


tree11<-ctree(Sales~ShelveLoc+CompPrice+Income+Price+Age+US+Urban, data=company_data)
plot(tree11)


set.seed(1234) 
  ind <- sample(2, nrow(company_data), replace=TRUE, prob=c(0.7, 0.3))
  trainData <- company_data[ind==1,]
  testData <- company_data[ind==2,]
  sales_ctree_1 <- ctree(Sales~., data=trainData)
  sales_ctree_2 <- ctree(Sales~CompPrice, data=trainData)
  sales_ctree_3 <- ctree(Sales~Income+Price, data=trainData)
  sales_ctree_4 <- ctree(Sales~Income+Price+Urban, data=trainData)
  sales_ctree_5 <- ctree(Sales~Income+Education+Price+Urban, data=trainData)
  sales_ctree_6 <- ctree(Sales~ShelveLoc, data=trainData)
  sales_ctree_7 <- ctree(Sales~Income+ShelveLoc, data=trainData)
  sales_ctree_8 <- ctree(Sales~ShelveLoc+CompPrice, data=trainData)
  sales_ctree_9 <- ctree(Sales~ShelveLoc+CompPrice+Income+Price+Age, data=trainData)
  sales_ctree_10 <- ctree(Sales~ShelveLoc+CompPrice+Income+Price+Age+US, data=trainData)
  sales_ctree_11 <- ctree(Sales~ShelveLoc+CompPrice+Income+Price+Age+US+Urban, data=trainData)
  
  table(predict(sales_ctree_1), trainData$Sales)
  plot(sales_ctree_1)
  
  testPred <- predict(sales_ctree_1, newdata = testData)
  plot(sales_ctree_2)
  
  testPred <- predict(sales_ctree_2, newdata = testData)
  plot(sales_ctree_2)
  
  testPred <- predict(sales_ctree_3, newdata = testData)
  plot(sales_ctree_3)
  
  plot(sales_ctree_4)
  testPred <- predict(sales_ctree_4, newdata = testData)
  
  plot(sales_ctree_5)
  testPred <- predict(sales_ctree_5, newdata = testData)
  
  plot(sales_ctree_6)
  testPred <- predict(sales_ctree_6, newdata = testData)
  
  table(predict(sales_ctree_7), trainData$Sales)
  plot(sales_ctree_7)
  testPred <- predict(sales_ctree_7, newdata = testData)
  
  plot(sales_ctree_8)
  testPred <- predict(sales_ctree_8, newdata = testData)
  
  plot(sales_ctree_9)
  testPred <- predict(sales_ctree_9, newdata = testData)
  
  plot(sales_ctree_10)
  testPred <- predict(sales_ctree_10, newdata = testData)
  
  
  plot(sales_ctree_11)
  testPred <- predict(sales_ctree_11, newdata = testData)
  
  mean(testPred==testData$Sales)
  table(testPred, testData$Sales)
  confusionMatrix(testPred,as.factor(testData$Sales))

cm <-   confusionMatrix(testPred,as.factor(testData$Sales))
cm$table

draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Class1', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Class2', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Class1', cex=1.2, srt=90)
  text(140, 335, 'Class2', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  
draw_confusion_matrix(cm)
