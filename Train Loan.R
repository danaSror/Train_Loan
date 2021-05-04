
library(Rcpp)
library(RColorBrewer)
library(rpart)
library(rattle)
library(caret)
#install.packages("rpart.plot")
library(rpart.plot)
library(lattice)
library(ggplot2)
library(arules)

####QS1.1####

#reading the csv and replacing the empty strings with NA values
dataset<-read.csv("C:\\Users\\97252\\Desktop\\train_Loan.csv", header=T, na.strings=c("","NA"))

dataset<- as.data.frame(dataset)
#displays the values to complete missing values
str(dataset)

#counts rows with NA in each column
loan_ID_sum_NA <-sum(is.na(dataset$Loan_ID))
gender_sum_NA <-sum(is.na(dataset$Gender))
married_sum_NA <-sum(is.na(dataset$Married))
Dependents_sum_NA <-sum(is.na(dataset$Dependents))
Education_sum_NA <-sum(is.na(dataset$Education))
Self_Employed_sum_NA <-sum(is.na(dataset$Self_Employed))
ApplicantIncome_sum_NA <-sum(is.na(dataset$ApplicantIncome))
CoapplicantIncome_sum_NA <-sum(is.na(dataset$CoapplicantIncome))
LoanAmount_sum_NA <-sum(is.na(dataset$LoanAmount))
Loan_Amount_Term_sum_NA <-sum(is.na(dataset$Loan_Amount_Term))
Credit_History_sum_NA <-sum(is.na(dataset$Credit_History))
Property_Area_sum_NA <-sum(is.na(dataset$Property_Area))
Loan_Status_sum_NA <-sum(is.na(dataset$Loan_Status))




####QS1.3####

#this function return the value that has highest number of occurrences in a set of data.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#Complete missing values for categorical values
gender_mode<-getmode(dataset$Gender)
dataset$Gender[is.na(dataset$Gender)]<-gender_mode

married_mode<-getmode(dataset$Married)
dataset$Married[is.na(dataset$Married)]<-married_mode

dependents_mode<-getmode(dataset$Dependents)
dataset$Dependents[is.na(dataset$Dependents)]<-dependents_mode

education_mode<-getmode(dataset$Education)
dataset$Education[is.na(dataset$Education)]<-education_mode

employees_mode<-getmode(dataset$Self_Employed)
dataset$Self_Employed[is.na(dataset$Self_Employed)]<-employees_mode

dataset$Credit_History<-as.factor(dataset$Credit_History)
credit_mean<-getmode(dataset$Credit_History)
dataset$Credit_History[is.na(dataset$Credit_History)]<-credit_mean

property_area_mode<-getmode(dataset$Property_Area)
dataset$Property_Area[is.na(dataset$Property_Area)]<-property_area_mode



#this function that calculates the average of every numeric column
getmean <- function(column) {
  mean(column, na.rm=TRUE)
}

#Complete missing values for numeric values

applicantIncome_mean<-getmean(dataset$ApplicantIncome)
dataset$ApplicantIncome[is.na(dataset$ApplicantIncome)]<-applicantIncome_mean

coapplicantIncome_mean<-getmean(dataset$CoapplicantIncome)
dataset$CoapplicantIncome[is.na(dataset$CoapplicantIncome)]<-coapplicantIncome_mean

loan_mean<-getmean(dataset$LoanAmount)
dataset$LoanAmount[is.na(dataset$LoanAmount)]<-loan_mean

loan_amount_term_mean<-getmean(dataset$Loan_Amount_Term)
dataset$Loan_Amount_Term[is.na(dataset$Loan_Amount_Term)]<-loan_amount_term_mean



####QS1.4####

summary(dataset$ApplicantIncome)
#Discretization

dataset$ApplicantIncome<-discretize(dataset$ApplicantIncome, include.lowest
                                        =TRUE ,method="frequency",right=TRUE ,breaks=4,labels= c("Low","Medium","High","Higher"))

summary(dataset$CoapplicantIncome)
#Discretization
dataset$CoapplicantIncome<-discretize(dataset$CoapplicantIncome, 
                                       method="interval" ,breaks=3,labels=c("Low","Medium","High"))

summary(dataset$LoanAmount)
#Discretization
dataset$LoanAmount<-discretize(dataset$LoanAmount, 
                                     method="frequency" ,breaks=3 ,labels=c("Low","Medium","High"))


####QS1.2####


inTrain<-createDataPartition(y=dataset$Loan_Status,p=0.8 ,list=FALSE)
#training set
training<-dataset[inTrain,]
#testing set
testing<-dataset[-inTrain,]


####QS 2####

#without coulmn id 
testing<- subset(testing, select= -c(Loan_ID))
training<- subset(training, select= -c(Loan_ID))


gini_tree_minsplit_10 <- rpart(data=training,
                                   training$Loan_Status~., method="class", minsplit=10, cp=0.002, parms=list(split="gini"))
fancyRpartPlot(gini_tree_minsplit_10)


gini_tree_minsplit_25 <- rpart(data=training,
                                   training$Loan_Status~., method="class", minsplit=25, cp=0.002, parms=list(split="gini"))
fancyRpartPlot(gini_tree_minsplit_25)


IG_tree_minsplit_10 <- rpart(data=training,
                                   training$Loan_Status~., method="class", minsplit=10, cp=0.002, parms=list(split="information"))
fancyRpartPlot(IG_tree_minsplit_10)


IG_tree_minsplit_25 <- rpart(data=training,
                                   training$Loan_Status~., method="class", minsplit=25, cp=0.002, parms=list(split="information"))
fancyRpartPlot(IG_tree_minsplit_25)




#####QS 3####

gini_predict_10 <<- predict(gini_tree_minsplit_10, testing, type="class")
gini_table_10 <<- table(testing$Loan_Status, gini_predict_10)
gini_accuracy_10 <<- sum(diag(gini_table_10))/sum(gini_table_10)

ig_predict_10 <<- predict(IG_tree_minsplit_10, testing, type="class")
ig_table_10 <<- table(testing$Loan_Status, ig_predict_10)
ig_accuracy_10 <<- sum(diag(ig_table_10))/sum(ig_table_10)

gini_predict_25 <<- predict(gini_tree_minsplit_25, testing, type="class")
gini_table_25 <<- table(testing$Loan_Status, gini_predict_25)
gini_accuracy_25 <<- sum(diag(gini_table_25))/sum(gini_table_25)

ig_predict_25 <<- predict(IG_tree_minsplit_25, testing, type="class")
ig_table_25 <<- table(testing$Loan_Status, ig_predict_25)
ig_accuracy_25 <<- sum(diag(ig_table_25))/sum(ig_table_25)

gini_accuracy_10
ig_accuracy_10
gini_accuracy_25
ig_accuracy_25 




