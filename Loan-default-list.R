install.packages("gains")
install.packages("irr")
install.packages("caret")
install.packages("car")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("eeptools")

library(dplyr)
library(ggplot2)
library(car)
library(gains)
library(irr)
library(caret)
library(eeptools)
library(lubridate)

setwd("C:\\Users\\hp\\Desktop\\New folder (2)")
ldt<-read.csv("loan_details_train.csv" , stringsAsFactors=FALSE, na.strings = "")

dim(ldt)
str(ldt)

summary(ldt)

colSums(is.na(ldt))


#Data transformation

#epmoyment_type to factor conv
class(ldt$Employment.Type)
ldt$Employment.Type<-ifelse(is.na(ldt$Employment.Type), "Missing", as.character(ldt$Employment.Type))

View(ldt$Employment.Type)
colSums(is.na(ldt))

summary(ldt$Employment.Type)
ldt$Employment.Type<-as.factor(ldt$Employment.Type)
class(ldt$Employment.Type)
View(ldt)

#DOB transformation

ldt$Date.of.Birth<- as.Date(ldt$Date.of.Birth, format= "%d-%m-%Y")

View(ldt$Date.of.Birth)
ldt$Age=round(age_calc(ldt$Date.of.Birth, enddate=Sys.Date(),
                       units="years"),0)
class(ldt$Age)
View(ldt$Age)

#region into factor conv
ldt$region<-as.factor(ldt$region)
class(ldt$region)

#disbursal date
ldt$DisbursalDate<- as.Date(ldt$DisbursalDate, format= "%d-%m-%Y")
class(ldt$DisbursalDate)
View(ldt)

#flag types to factor cov
ldt$MobileNo_Avl_Flag <- as.numeric(ldt$MobileNo_Avl_Flag)
ldt$Passport_flag <- as.factor(ldt$Passport_flag)
ldt$VoterID_flag <- as.factor(ldt$VoterID_flag)
ldt$PAN_flag <- as.factor(ldt$PAN_flag)
ldt$Aadhar_flag <- as.factor(ldt$Aadhar_flag)
ldt$Driving_flag <- as.factor(ldt$Driving_flag)



#credit_hist_lngth
ldt<-tidyr::separate(
  data= ldt,
  col= CREDIT.HISTORY.LENGTH,
  sep= " ",
  into= c("year", "months"),
  remove= FALSE
)
install.packages("readr")
library(readr)
ldt$CREDIT.HISTORY.LENGTH1<-readr::parse_number(ldt$CREDIT.HISTORY.LENGTH)
ldt$CREDIT.HISTORY.LENGTH2<-readr::parse_number(ldt$months)
ldt$CREDIT.HISTORY.LENGTH3<-(12*ldt$CREDIT.HISTORY.LENGTH1 +
                               ldt$CREDIT.HISTORY.LENGTH2)/12
head(ldt$CREDIT.HISTORY.LENGTH3)
class(ldt$CREDIT.HISTORY.LENGTH3)


summary(ldt)
View(ldt)



#delinquent to factor
ldt$DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS<- as.numeric(ldt$DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS)
class(ldt$DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS)



#visualizations & outliers

#disbursed_data
summary(ldt$disbursed_amount)
M1<-mean(ldt$disbursed_amount)
IQR(ldt$disbursed_amount)
boxplot(ldt$disbursed_amount)

Min1<- 47049-(1.5*13288)
Max1<- 60337+(1.5*13288)

ldt$disbursed_amount<- replace(ldt$disbursed_amount, ldt$disbursed_amount<Min1, M1)
ldt$disbursed_amount<- replace(ldt$disbursed_amount, ldt$disbursed_amount>Max1, M1)


-----------------------------------------------
  #ltv
  summary(ldt$ltv)
M2<- mean(ldt$ltv)
IQR(ldt$ltv)
boxplot(ldt$ltv)

Min2<- 68.76-(1.5*14.84)
Max2<-83.60+(1.5*14.84)

ldt$ltv<-replace(ldt$ltv, ldt$ltv< Min2, M2)


-------------------------------------------
  #asset cost 
  summary(ldt$asset_cost)
M3<- mean(ldt$asset_cost)
IQR(ldt$asset_cost)
boxplot(ldt$asset_cost)


Min3<- 65629-(1.5*13755.5)
Max3<-79385+(1.5*13755)

ldt$asset_cost<- replace(ldt$asset_cost, ldt$asset_cost>Max3, M3)
ldt$asset_cost<- replace(ldt$asset_cost, ldt$asset_cost<Min3, M3)

-----------------------------------------------------------------
  
  #credit hist length  
  boxplot(ldt$CREDIT.HISTORY.LENGTH3)



#Correlation

cor(ldt$default, ldt$disbursed_amount)
cor(ldt$default, ldt$asset_cost)
cor(ldt$default, ldt$ltv)
cor(ldt$default, ldt$branch_id)
cor(ldt$default, ldt$PERFORM_CNS.SCORE)
cor(ldt$default, ldt$DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS)
cor(ldt$default, ldt$NO.OF_INQUIRIES)
cor(ldt$default, ldt$Age)
cor(ldt$default, ldt$CREDIT.HISTORY.LENGTH3)


-------------------------------------------------------
  
  #Model
  
  model<-glm(default~., family="binomial", data= ldt)

summary(model)
------------------------------------------------------------
  
  install.packages("MASS")
library(MASS)
library(pROC)

aov1= aov(model, data = train)


step(model, direction= "both")


-------------------------------------------------------------------
  
  model1<-glm(default ~., family="binomial", data= ldt[,c(-1,-3,-8,-10,-12,
                                                          -13,-14,-15,-19,-20)])

summary(model1)
--------------------------------------------------
  
  model2<-glm(default ~., family="binomial", data= ldt[,c(-1,-3,-8,-10,-12,
                                                          -13,-14,-15,-18,-19,-20,-24,-25)])

summary(model2)
-----------------------------------------------
  
  Test<-read.csv("loan_details_test.csv" , stringsAsFactors=FALSE, na.strings = "")


#Prediction
library(caret)

# existing spread of default vs no default
table(ldt$default)/nrow(ldt)

# always predict 0 (didn't survive)
pred<-predict(model2, type="response")
head(pred)
summary(pred)
ldt$prob <- pred
--------------------------------------------------------------
  
  # Using probability cutoff of 50%.
  
  pred_default <- factor(ifelse(pred >= 0.50, "Yes", "No"))
actual_default <- factor(ifelse(ldt$default==1,"Yes","No"))
table(actual_default,pred_default)


# Let's find the Accuracy, Sensitivity, Specificity using 50% cutoff

cutoff_default <- factor(ifelse(pred >=0.50, "Yes", "No"))
conf_final <- confusionMatrix(cutoff_default, actual_default, positive = "Yes")
accuracy <- conf_final$overall[1]
sensitivity <- conf_final$byClass[1]
specificity <- conf_final$byClass[2]
accuracy
sensitivity
specificity

----------------------------------------------------------------
  
  perform_fn <- function(cutoff) 
  {
    predicted_default <- factor(ifelse(pred >= cutoff, "Yes", "No"))
    conf <- confusionMatrix(predicted_default, actual_default, positive = "Yes")
    accuray <- conf$overall[1]
    sensitivity <- conf$byClass[1]
    specificity <- conf$byClass[2]
    out <- t(as.matrix(c(sensitivity, specificity, accuray))) 
    colnames(out) <- c("sensitivity", "specificity", "accuracy")
    return(out)
  }

options(repr.plot.width =8, repr.plot.height =6)
summary(pred)
s = seq(0.01,0.80,length=100)
OUT = matrix(0,100,3)
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
     type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),text.font =3,inset = 0.02,
       box.lty=0,cex = 0.8, 
       lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
abline(v = 0.225, col="red", lwd=1, lty=2)
axis(1, at = seq(0.1, 1, by = 0.1))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
#Let's choose a cutoff value of 0.22 for final model, where the three curves for accuracy, specificty and sensitivity meet
cutoff


cutoff_default <- factor(ifelse(pred >=0.2254545, "Yes", "No"))
conf_final <- confusionMatrix(cutoff_default, actual_default, positive = "Yes")
accuracy <- conf_final$overall[1]
sensitivity <- conf_final$byClass[1]
specificity <- conf_final$byClass[2]
accuracy
sensitivity
specificity

#Logistic Regression with a cutoff probability value of 0.22 gives us better values of accuracy, sensitivity and specificity in the validation data.


#Checking the AUC

library(pROC)
options(repr.plot.width =10, repr.plot.height = 8)
glm.roc <- roc(response = ldt$default, predictor = as.numeric(pred))
par("mar")
par(mar=c(1,1,1,1))
plot(glm.roc,      legacy.axes = TRUE, print.auc.y = 1.0, print.auc = TRUE)
legend("bottom", "Logistic")
Test$pred_prob <- predict(model2, type = "response", newdata = Test)
summary(Test)
Test$default <- round(Test$pred_prob, digits = 2) # round off to 2 decimal digits
summary(Test)

----------------------------------------------------------------------
  
names(Test)
nrow(Test)
submission <- Test[,c('ID','default')]
nrow(submission)
head(submission)
colSums(is.na(submission))
nrow(submission)
submission$default <- round(submission$default, digits = 2) # round off to 2 decimal digits
write.csv(submission,"Solution.csv1", row.names = FALSE)