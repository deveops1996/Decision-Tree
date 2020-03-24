install.packages("C50",repos = "http://cran.us.r-project.org")
install.packages("tree",repos = "http://cran.us.r-project.org")
install.packages("caret",repos = "http://cran.us.r-project.org")
install.packages("gmodels",repos = "http://cran.us.r-project.org")
install.packages("party",repos = "http://cran.us.r-project.org")
install.packages("knitr",repos = "http://cran.us.r-project.org")
install.packages("png",repos = "http://cran.us.r-project.org")
library(party)
library(caret)
library(C50)
library(tree)
library(gmodels)
library(knitr)
library(png)
FraudCheck <- read.csv(file.choose())
View(FraudCheck)
str(FraudCheck)



hist(FraudCheck$Taxable.Income)

# Splitting data into training and testing.
# splitting the data based on Sales
Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")

FC = data.frame(FraudCheck,Risky_Good)
View(FC)
FC_train <- FC[1:300,]

View(FC_train)
FC_test <- FC[301:600,]

View(FC_test)

attach(FraudCheck)

tree1<-ctree(Taxable.Income~., data=FC)  
plot(tree1) #view the decision tree

tree2<-ctree(Taxable.Income~Risky_Good, data=FC)
plot(tree2)

tree3 <- ctree(Risky_Good~ Taxable.Income, data = FC)
plot(tree3)

tree4 = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                  Work.Experience + Urban, data = FC_train)

plot(tree4)
summary(tree4)

pred_tree <- as.data.frame(predict(tree4,newdata=FC_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(tree4,newdata=FC_test)
mean(pred_test_df==FC_test$Risky_Good)

CrossTable(FC_test$Risky_Good,pred_test_df)
confusionMatrix(FC_test$Risky_Good,pred_test_df)

cm <- confusionMatrix(FC_test$Risky_Good,pred_test_df)
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

















