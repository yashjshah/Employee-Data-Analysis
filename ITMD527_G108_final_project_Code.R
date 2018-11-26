#ITMD 527-01 DATA ANALYTICS FINAL PROJECT
#GROUP 108 EMPLOYEE DATA ANALYSIS
#TEAM MEMBERS
#1. JAYON NIRAVEL
#2. YASH JITENDAR SHAH
#3. NEERAJ RAJAGOPALAN IYER


# LOAD LIBRARIES

library(ggplot2)
library(pROC)
library(corrplot)
library(caTools)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(ROCR)
library(reshape2)
library(dplyr)
library(tidyr)
library(magrittr)
library(BSDA)



# IMPORTING THE DATA FILE
mydata=read.csv("HR_comma_sep.csv",header=T)

# renaming the "sales" column in the csv as "department"
colnames(mydata)[9]="department"

# printing the initial data entries

head(mydata)
str(mydata)

# Summary of the dataset

summary(mydata)

# Overview of the Summary

cor_vars<-mydata[,c("satisfaction_level","last_evaluation","number_project","average_montly_hours","time_spend_company","Work_accident","left","promotion_last_5years")]
aggregate(cor_vars[,c("satisfaction_level","last_evaluation","number_project","average_montly_hours","time_spend_company","Work_accident","promotion_last_5years")],by=list(category=cor_vars$left),FUN=mean)


# Checking the dependent variable "left" 0:1 ratio

round(prop.table(table(mydata$left))*100)

# INFERENTIAL STATISTICS

# 1.Does low average satisfaction level make the employee leave the company?

Employee_left=subset(mydata,mydata$left==1)
print(mean(Employee_left$satisfaction_level))       # printing the mean of the satisfaction level of the left employees
print(median(Employee_left$satisfaction_level))    # printing the median of the satisfaction level of the left employees

# Visualization of the satisfaction level of the left Employees

tr1<- function(a) { ggplot(data=Employee_left,aes(x=a,y=..density..))+geom_histogram(fill="green",color="blue",alpha=0.5,bins=100)+geom_density()}
tr1(Employee_left$satisfaction_level)


# 2. How last evaluation is related to our research study?

print(mean(Employee_left$last_evaluation))     # printing the mean of the last evaluation of the left employees
print(median(Employee_left$last_evaluation))         # printing the median of the last evaluation of the left employees
 
# Visualization of the last evaluation of the left employees
tr1(Employee_left$last_evaluation)   

# 3. Is salary having any direct impact?


# Visualization to see the Salary levels impact
ggplot(subset(mydata,left==1),aes(x=factor('salary'),fill=factor(salary)))+geom_bar(width=1,position="fill",color="black") + coord_polar(theta="y")+theme_bw()+labs(title="Salary")


# 4. Is there a Certain time period after which the Employees change the company?


# Visualization to see the time spend by an employee in the company

ggplot(subset(mydata,left==1),aes(time_spend_company))+geom_histogram(binwidth=0.5,fill='red')+labs(x="Time spent company",title="TIme Spend in company")+theme_bw()


# 5. Is there any issues in particular department?

# Visualization to see if there are any issues in any particular department

ggplot(subset(mydata,left==1),aes(department))+geom_bar(fill='blue',width=0.5)+labs(x="Department",title="Department")+theme_bw()


# Digging deeper to find the department with issues

round(prop.table(table(mydata$department))*100)

# From which department have the employees left the most

left_dept=subset(mydata,mydata$left==1)
(table(left_dept$department))/(table(mydata$department))


#Histogram Evaluation


hist(mydata$satisfaction_level, col="orange")
hist(mydata$last_evaluation, col="red")
hist(mydata$average_montly_hours, col="lightgreen")

# Boxplot of time spent in company and the number of project
boxplot(mydata$time_spend_company,mydata$number_project)

# Number of Projects (vs) Average Monthly Hours

p<-ggplot(mydata,aes(x=factor(number_project),y=average_montly_hours,fill=factor(left)))+geom_boxplot()+scale_fill_manual(values=c("yellow","orange"))
print(p)

# checking number of projects(y) with time spend in company(x) per department
p<-ggplot(mydata,aes(y=number_project,x=time_spend_company,color=left))+geom_point()+facet_grid( ~department)
print(p)

# Validation by splitting left/grids

p<-ggplot(mydata,aes(y=number_project,x=time_spend_company))+geom_point()+facet_grid( ~left)
print(p)

# checking number of projects(x) with time spend in company(y) per department
p<-ggplot(mydata,aes(x=number_project,y=time_spend_company,color=left))+geom_point()+facet_grid( ~department)
print(p)

# checking number of projects(x) with time spend in company(y)
p<-ggplot(mydata,aes(x=number_project,y=time_spend_company,color=left))+geom_point()
print(p)

# Number of projects (vs) last Evalaution

p<-ggplot(mydata,aes(x=factor(number_project),y=last_evaluation,fill=factor(left)))+geom_boxplot()+scale_fill_manual(values=c("yellow","orange"))
print(p)

# Satisfaction Level (vs) Last Evalaution

ggplot(mydata,aes(satisfaction_level,last_evaluation,color=left))+geom_point(shape=16,size=5,show.legend=FALSE)+theme_minimal()+scale_color_gradient(low="red",high="black")

# No of people who left when they were not promoted in last 5 years

left_Employee=mydata[,c("promotion_last_5years","left")] %>% group_by(promotion_last_5years) %>% summarize(left_Employee=n())
ggplot(left_Employee,aes(x=promotion_last_5years,y=left_Employee,fill=left_Employee,color=promotion_last_5years)) + geom_bar(stat='identity')

#ANOVA number of projects (vs) average monthly hours

p<-ggplot(mydata,aes(x=factor(number_project),y=average_montly_hours))+geom_boxplot()
print(p)

numberproject=mydata$number_project
averagemonthlyhours=mydata$average_montly_hours
anova1=lm(averagemonthlyhours~as.factor(numberproject))
summary(anova1)


#Hypothesis Testing

# 1. Employees with high satisfaction levels of greater than 0.8 leave the company
hypothesis=read.table('HR_comma_sep.csv',header=T,sep=',',nrows=100)
h1=hypothesis[,1]
z.test(h1,NULL,alternative="less",mu=0.8,sigma.x=sd(h1),conf.level=0.95)

# 2. Employees with low performance evaluation of less than 0.5 leave the company
h2=hypothesis[,2]
z.test(h2,NULL,alternative="greater",mu=0.5,sigma.x=sd(h2),conf.level=0.95)

# 3. Employees with less than 4 projects leave the company

h3=hypothesis[,3]
z.test(h3,NULL,alternative="greater",mu=4,sigma.x=sd(h3),conf.level=0.95)


# PREDICTIVE MODELLING

# Correlation Matrix

cor_vars<-mydata[,c("satisfaction_level","last_evaluation","number_project","average_montly_hours","time_spend_company","Work_accident","left","promotion_last_5years")]
cor(cor_vars)



# Salary (vs) left

relvis1<-table(mydata$salary,mydata$left)
d_relvis1<-as.data.frame(relvis1)
 p<-ggplot(d_relvis1,aes(x=Var1,y=Freq,fill=Var2))+geom_bar(position="dodge",stat="identity")+coord_flip()+labs(x="Var1(salary)",y="Var2(left)",title="salary vs left")
 print(p)

# Department (vs) left

relvis2<-table(mydata$department,mydata$left)
d_relvis2<-as.data.frame(relvis2)
d_relvis2<-subset(d_relvis2,Var2==1)
d_relvis2$Var1<-factor(d_relvis2$Var1,levels=d_relvis2$Var1[order(-d_relvis2$Freq)])
 p<-ggplot(d_relvis2,aes(x=Var1,y=Freq,fill=Var1))+ geom_bar(position="dodge",stat='identity')+theme(axis.text.x=element_text(angle=90,hjust=1))+labs(x="Var1(department)",y="Var2(left freq)",title="department vs left")
print(p)

#  left (vs) number of projects

relvis3<-table(mydata$number_project,mydata$left)
d_relvis3<-as.data.frame(relvis3)

library(ggplot2)
p<-ggplot(d_relvis3, aes(x=Var1,y=Freq,fill=Var2)) +
 geom_bar(position="dodge",stat='identity') + coord_flip()+labs(x="Var1(number of projects)",y="Var2(left freq)",title="number of projects vs left")

print(p)

# left (vs) Last Evaluation

left_data<-subset(mydata,left==1)
stay_data<-subset(mydata,left==0)
ggplot()+geom_density(aes(x=last_evaluation),colour="red",data=left_data)+geom_density(aes(x=last_evaluation),colour="blue",data=stay_data)


# left (vs) Average Monthly Hours

ggplot()+geom_density(aes(x=average_montly_hours),colour="red",data=left_data)+geom_density(aes(x=average_montly_hours),colour="blue",data=stay_data)


# left (vs) Satisfaction Level

ggplot()+geom_density(aes(x=satisfaction_level),colour="red",data=left_data)+geom_density(aes(x=satisfaction_level),colour="blue",data=stay_data)


# Splitting the data set 

split=sample.split(mydata$left,SplitRatio=0.8)
train.data=subset(mydata,split==TRUE)
test.data=subset(mydata,split==FALSE)
table(train.data$left)

# 1. LOGISTIC REGRESSION MODEL

# Simply creating a rough model

m1=glm(as.factor(train.data$left)~.,data=train.data,family='binomial')
summary(m1)

# Discard "m1" as we will build models using the stepwise backward and forward selection

# Creating a model using the stepwise backward selection

m2=step(m1,direction="backward",trace=T)
summary(m2)

#Adj-R2 value for model m2
mod1=glm(as.factor(train.data$left)~satisfaction_level+last_evaluation+number_project+average_montly_hours+time_spend_company+Work_accident+promotion_last_5years+department+salary,data=train.data,family="binomial")
nullmod1=glm(as.factor(train.data$left)~1,family="binomial")
1-logLik(mod1)/logLik(nullmod1)

#Residual Analysis 
res1=residuals(m2,type="deviance")
plot(res1)
abline(a=0,b=0,col='red')



# Creating the model using the stepwise Forward Selection

base=glm(as.factor(train.data$left)~satisfaction_level,data=train.data,family="binomial")
m3=step(base,scope=list(upper=m1,lower=~1),direction="forward",trace=F)
summary(m3)

#Adj-R2 value for model m3
mod2=glm(as.factor(train.data$left)~satisfaction_level+last_evaluation+number_project+average_montly_hours+time_spend_company+Work_accident+promotion_last_5years+department+salary,data=train.data,family="binomial")
nullmod2=glm(as.factor(train.data$left)~1,family="binomial")
1-logLik(mod2)/logLik(nullmod2)

#Residual Analysis
res2=residuals(m3,type="deviance")
plot(res2)
abline(a=0,b=0,col='red')

# Use m2 or m3 as both have the same AIC value

# Model Evaluation (Finding the best cutoff value)

# Cutoff value = 0.5

prediction1=predict(m2,newdata=test.data)
prediction1.glm=ifelse(predict(m2,newdata=test.data,type='response')>0.5,1,0)
table(as.factor(test.data$left),prediction1.glm)

# Cutoff value = 0.55

prediction2=predict(m2,newdata=test.data)
prediction2.glm=ifelse(predict(m2,newdata=test.data,type='response')>0.55,1,0)
table(as.factor(test.data$left),prediction2.glm)


# Cutoff value =0.6

prediction3=predict(m2,newdata=test.data)
prediction3.glm=ifelse(predict(m2,newdata=test.data,type='response')>0.6,1,0)
table(as.factor(test.data$left),prediction3.glm)

# Cutoff value =0.4

prediction4=predict(m2,newdata=test.data)
prediction4.glm=ifelse(predict(m2,newdata=test.data,type='response')>0.4,1,0)
table(as.factor(test.data$left),prediction4.glm)

# Cutoff value =0.45

prediction5=predict(m2,newdata=test.data)
prediction5.glm=ifelse(predict(m2,newdata=test.data,type='response')>0.45,1,0)
table(as.factor(test.data$left),prediction5.glm)

# Cutoff value =0.39

prediction6=predict(m2,newdata=test.data)
prediction6.glm=ifelse(predict(m2,newdata=test.data,type='response')>0.39,1,0)
table(as.factor(test.data$left),prediction6.glm)

# Cutoff value =0.38

prediction7=predict(m2,newdata=test.data)
prediction7.glm=ifelse(predict(m2,newdata=test.data,type='response')>0.38,1,0)
table(as.factor(test.data$left),prediction7.glm)


# Employees On the verge of leaving the company

Employee_stay<-mydata[mydata$left==0,]
Employee_stay[,c(1,2,4)]<-scale(Employee_stay[,c(1,2,4)])
Employee_stay$prediction<-predict(m2,Employee_stay)
Employee_stay<-Employee_stay %>% arrange(desc(prediction))
nrow(Employee_stay[Employee_stay$prediction>0.4,])



glm_response_scores <- predict(m2, test.data, type="response")
pred_glm<-prediction(glm_response_scores,test.data$left)
Logistic_AUC=auc(test.data$left,glm_response_scores)
print(Logistic_AUC)


# AUC (Area under the Curve) value  
glm.response.scores.th<-predict(m2,test.data,type='response')
pred2.glm.th<-prediction(glm_response_scores,test.data$left)
pred2.glm.th<-prediction(glm_response_scores,test.data$left)
AUC=auc(test.data$left,glm.response.scores.th)
print(AUC)
# Visulaization of AUC
perf_glm<-performance(pred2.glm.th,"tpr","fpr")

# 2. DECISION TREES


# Building the model using CART and finding the cp,the minimum cross validation error
model1_dt<-rpart(left~.,data=train.data,method="class",minbucket=25)
printcp(model1_dt)


# Prediction Using DT

predict1_dt_ROC<-predict(model1_dt,test.data)
pred1_dt<-prediction(predict1_dt_ROC[,2],test.data$left)
pref_dt<-performance(pred1_dt,"tpr","fpr")
auc=performance(pred1_dt,'auc')
CART_AUC<-slot(auc,'y.values')

# finding the best cp value

bestcp<-model1_dt$cptable[which.min(model1_dt$cptable[,"xerror"]),"CP"]
bestcp

# Tree Pruning

model1_dt.pruned<-prune(model1_dt,cp=bestcp)
plotcp(model1_dt.pruned)
rpart.plot(model1_dt.pruned)


# Finding the importance of each variable 

important<-varImp(model1_dt.pruned) 
print(important)



# Predictions using the decision Tree

prediction_dt<-predict(model1_dt,test.data,type="class")
table(test.data$left,prediction_dt)
mean(prediction_dt==test.data$left)


# BORUTA ALGORITHM

library(Boruta)
mydata$left<-as.factor(mydata$left)
boruta.train <- Boruta(left~., data = mydata, doTrace = 2)
print(boruta.train)
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
+ boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)


# SVM

# Building model using SVM

model_svm<-svm(left~.,data=train.data,gamma=0.25,cost=10)
prediction1_svm<-predict(model_svm,test.data)
prediction1_svm<-ifelse(prediction1_svm>0.5,1,0)
table(test.data$left,prediction1_svm)
mean(prediction1_svm==test.data$left)

# Predictions for LOGISTICAL REGRESSION MODEL, SVM, DECISION TREES

prediction_glm_ROC<-predict(m2,test.data,type="response")
pred_glm<-prediction(prediction_glm_ROC,test.data$left)
perf_glm<-performance(pred_glm,"tpr","fpr")
prediction_dt_ROC<-predict(model1_dt,test.data)
pred_dt<-prediction(prediction_dt_ROC[,2],test.data$left)
perf_dt<-performance(pred_dt,"tpr","fpr")
predict_svm_ROC<-predict(model_svm,test.data,type="response")
pred_svm<-prediction(predict_svm_ROC,test.data$left)
perf_svm<-performance<-performance(pred_svm,"tpr","fpr")


# AUC Values for LOGISTICAL REGRESSION MODEL, SVM, DECISION TREES

auc_glm <- performance(pred_glm,"auc")
auc_glm <- round(as.numeric(auc_glm@y.values),3)
auc_dt <- performance(pred_dt,"auc")
auc_dt <- round(as.numeric(auc_dt@y.values),3)
auc_svm <- performance(pred_svm,"auc")
auc_svm <- round(as.numeric(auc_svm@y.values),3)
print(paste('AUC of Logistic Regression:',auc_glm))
print(paste('AUC of Decision Tree:',auc_dt))
print(paste('AUC of Support Vector Machine:',auc_svm))


# Comparing the performance of  LOGISTICAL REGRESSION MODEL, SVM and DECISION TREES using the ROC curve

plot(perf_glm, main = "ROC curves for the models", col='blue')
plot(perf_dt,add=TRUE, col='red')
plot(perf_svm, add=TRUE, col='darkmagenta')
legend('bottom', c("Logistic Regression", "Decision Tree","Support Vector Machine"), fill = c('blue','red','darkmagenta'), bty='n')





 




