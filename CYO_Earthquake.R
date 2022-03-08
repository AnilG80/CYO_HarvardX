##########################################################
   #Install/Load libraries 
##########################################################

# Note: this process could take few minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(lightgbm)) install.packages("lightgbm", repos = "http://cran.us.r-project.org")
if(!require(R6)) install.packages("R6", repos = "http://cran.us.r-project.org")
if(!require(tictoc)) install.packages("tictoc", repos = "http://cran.us.r-project.org")
if(!require(smotefamily)) install.packages("smotefamily", repos = "http://cran.us.r-project.org")
if(!require(xgboost))install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(mlbench))install.packages("mlbench", repos = "http://cran.us.r-project.org")
if(!require(bookdown))install.packages("bookdown", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(caret)
library(data.table)
library(rpart)
library(rpart.plot)
library(randomForest)
library(lightgbm)
library(tictoc)
library(smotefamily)
library(xgboost)
library(mlbench)

##########################################################
     # Download raw data files  
##########################################################

#three data sets were provided by drivendata org for the challenge,
#(Source: https://www.drivendata.org/competitions/57/nepal-earthquake/), these data sets were uploaded
#to github for subsequent easier access. The data consist of building Id, features that may be important for predicting
#the level of damage after an earthquake.  

#1. This data consists of building Ids and features, we are to predict damage_grade values
#based on this data for the challenge. damage grade is not given for this data set and 
#and hence are not useful for tuning algorithms in this project. 

test<-read.csv('https://raw.githubusercontent.com/AnilG80/CYO_HarvardX/main/test_values.csv')

#2. this is the data given to us for training purpose, it has building ID and features 

df_equake<-read.csv("https://raw.githubusercontent.com/AnilG80/CYO_HarvardX/main/train_values.csv")

#3. this is the label data that has building ID and damage_grade for all the building in the training set, 

label <-read.csv("https://raw.githubusercontent.com/AnilG80/CYO_HarvardX/main/train_labels.csv")

#4. Let's combine data sets 2 and 3 and make a complete data set for this project 

dfqk<-inner_join(df_equake,label,by="building_id")
dfqk <- dfqk %>% mutate_if(is.character,as.factor)
dfqk<-dfqk%>%mutate(damage_grade=as.factor(damage_grade))
str(dfqk)

#5. check if the data set has any missing values

sum(is.na(dfqk)==TRUE)

#############################################
#Use visuals to explore the data 
############################################

#How bad were the buildings damaged by the earthquake in 1 to 3 scale (minor, medium, heavy)
##as a bar chart
dfqk %>% 
  ggplot(aes(damage_grade,fill=damage_grade)) + 
  geom_bar(stat = "count") + 
  theme(legend.position = "none")

##as a pie chart with numbers 
dfqk %>%group_by(damage_grade)%>%summarise(N=n())%>%
  mutate(perc=N/sum(N), lab=scales::percent(perc))%>%
  ggplot(aes(x = "", y = perc, fill = damage_grade)) +
  geom_col() +
  geom_text(aes(label = lab),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  guides(fill = guide_legend(title = "Damage Grade")) +
  theme_void()

#quick look at how many unique values in each features 
#as most features appear discreet numerical, binary or categorical variable
l<-dfqk%>% lapply(unique)%>%lengths()
df<-tibble(features=colnames(dfqk),entries=l)
df[-c(1:4,40),]%>%ggplot(aes(x=reorder(features,entries),y=entries))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90))+
  coord_flip()

#let's split data into variable types for future use 
cat_col<-dfqk[,c(9:15,27)]
num_col<-dfqk[,c(2:5,6:8,28)]
binary_str<-dfqk[,c(16:26)]
binary_landuse<-dfqk[,c(29:39)]

#Lets explore the variables, let's start with categorical variables (nominal)
dfqk %>% 
  pivot_longer(cols = c(9:15,27)) %>% 
  ggplot(aes(x=fct_infreq(value),fill=damage_grade)) + 
  geom_bar(stat = "count", position = "fill")+ 
  facet_wrap(~name, scales = "free",nrow = 2,ncol = 4)+
  theme(legend.position = "top")


#For the numerical data, let's use correlation plot to see any interdependence

dfqk [,c(2:8)]%>% 
  cor()%>%
  corrplot::corrplot(type = "upper", order = "hclust", 
                     tl.col = "black", tl.srt = 45)

#how good are features like age,geographical location, area and height 
#of a building in predicting the intensity of damage 
dfqk %>% filter(age<700)%>%
  pivot_longer(cols = c(2:4,6:8)) %>% 
  ggplot(aes(value,fill=damage_grade)) + 
  geom_histogram(position = "fill")+ 
  facet_wrap(~name, scales = "free",nrow = 2,ncol = 4)+
  theme(legend.position = "top")

#age of 995 is likely an error ( 999 as a NA, encoding error), we can't be sure 
#so we will park this for now,  
dfqk %>% filter(age>700)%>%
  ggplot(aes(age,fill=damage_grade)) + 
  geom_histogram(position = "dodge")+ 
  theme(legend.position = "top")

#binary aka one-hot-encoded data sets 
#is building material (superstructure of the building is made of what material)
#important in predicting building damage 
dfqk_clean<-dfqk
colnames(dfqk_clean)<-gsub("has_superstructure_","",colnames(dfqk_clean))
dfqk_clean %>% 
  pivot_longer(cols = c(16:26))%>% 
  ggplot(aes(value, fill=damage_grade)) + 
  geom_bar(stat="count",position="fill") + 
  facet_wrap(~name, scales = "free",nrow = 3,ncol = 4)+
  theme(legend.position = "none")


#what about its usage, i.e some residential buildings have secondary use i.e schools, police 
colnames(dfqk_clean)<-gsub("has_secondary_use_","",colnames(dfqk_clean))
dfqk_clean %>% 
  pivot_longer(cols = c(29:38)) %>% 
  ggplot(aes(value, fill=damage_grade)) + 
  geom_bar(stat="count",position="fill") + 
  facet_wrap(~name, scales = "free",nrow = 3,ncol = 4)+
  theme(legend.position = "none")

#discreet numerical number of families and number of floors 
dfqk %>% 
  pivot_longer(cols = c(5,28)) %>% 
  ggplot(aes(value, fill=damage_grade)) + 
  geom_bar(stat="count",position="fill") + 
  facet_wrap(~name, scales = "free",nrow = 2,ncol = 4)+
  theme(legend.position = "none")

#####################################################
      #Create validation and training sets 
####################################################

#Getting the data ready

##one-hot encoding to change categorical variables into 1's and 0's
dummy <- dummyVars(" ~ .", data=cat_col)
cat_df <- data.frame(predict(dummy, newdata=cat_col))
head(cat_df,n=10)
dfqkm<-data.frame(dfqk[,-c(9:15,27)],cat_df)

## Validation set will be 10% of earthquake data 
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = dfqkm$damage_grade, times = 1, p = 0.1, list = FALSE)
trainqk<- dfqkm[-test_index,]
testqk <- dfqkm[test_index,]
##check they have been created right 
str(trainqk)
str(testqk)

#covert data into x (predictors) and y (to be predicted aka damage_grade)
trainqk_x <- trainqk[,-which(colnames(trainqk) =="building_id")]
trainqk_y<-trainqk[,which(colnames(trainqk) =="damage_grade")]
testqk_x<-testqk[,-which(colnames(trainqk) =="building_id")]
testqk_y<-testqk[,which(colnames(trainqk) =="damage_grade")]
str(testqk_y)
str(trainqk_x)

#####################################################
    #Model 1: Decision Tree Algorithm
####################################################

dt_fit <- rpart(damage_grade ~ ., data=trainqk_x, method="class",cp=-1)
plotcp(dt_fit)

#based on the cp plot, we choose 0.00005
tic()
dt_fit <- rpart(damage_grade ~ ., data=trainqk_x, method="class",cp=0.00005)
exectime_tree <- toc()
exectime_tree <- exectime_tree$toc - exectime_tree$tic

#Predict the test data set 
dt_pred <- predict(dt_fit, testqk_x, type = "class")

#How good is the prediction (Using Confusion Matrix)
rpart_acc<-confusionMatrix(dt_pred, reference=testqk_y,mode = "everything")
rpart_acc
#What variable are most important in damage prediction###
imp<-data.frame(dt_fit$variable.importance)
colnames(imp)<-c("impRank")
qkvar <- rownames(imp)
rownames(imp) <- NULL
imp <- cbind(qkvar,imp)
imp%>%filter(impRank>1000)%>%
  ggplot(aes(x=reorder(qkvar,impRank),y=impRank,fill=qkvar))+
  geom_bar(stat="identity")+
  coord_flip()+
  theme(legend.position = "none")+
  ylab("Variable Importance")+
  xlab("Top Features")

#Table outlining the performance of difference models 
options(pillar.sigfig = 5)
ptable<-cbind(t(tibble("Decision Tree Model" =rpart_acc[["byClass"]][,"F1"])),OverallAccuracy=rpart_acc$overall[1],Timetaken=exectime_tree)
ptable<-data.frame(ptable)
colnames(ptable)[1:3]<-c("F1_Class1","F1_Class2","F1_Class2")
ptable

#####################################################
     #Model 2: Random Forest 
####################################################

##run the model 

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
tic()
rf_fit <- randomForest(damage_grade ~ ., data=trainqk_x,
                       ntree=50,
                       sampsize=50000,
                       mtry=25,
                       importance = TRUE)
exectime_rf <- toc()
exectime_rf <- exectime_rf$toc - exectime_rf$tic

##see how many ntrees optimal, n=50 looked good went back and reran with ntree 50 

rf_fit
plot(rf_fit)

## try tuning mtry 
tic() 
tuneRF(trainqk_x[,-31],trainqk_y,
       mtryStart = 2,
       stepFactor = 2,
       plot = TRUE,ntreeTry = 50,
       trace = TRUE,
       improve = 0.05)
exectime_rftune <- toc()
exectime_rftune <- exectime_rftune$toc - exectime_rftune$tic

#predict test data using the fit 
rf_pred<-predict(rf_fit,testqk_x)
#check the performance of the model 
rf_acc<-confusionMatrix(rf_pred, reference=testqk_y, mode = "everything")
rf_acc
#find What variable are most important in damage prediction
varImpPlot(rf_fit)
varImpPlot(rf_fit,
           sort = TRUE,
           n.var=10,
           main="Top 10 - Variable Importance")
imp_rf<-data.frame(importance(rf_fit))
rname_rf <- rownames(imp_rf)
rownames(imp_rf) <- NULL
imp_rf <- cbind(rname_rf,imp_rf)
imp_rf
imp_rf%>%filter(X1>10)%>%
  pivot_longer(!rname_rf)%>%
  ggplot(aes(x=reorder(rname_rf,value),y=value,fill=name))+
  geom_bar(stat="identity")+
  coord_flip()+
  theme(legend.position = "none")+
  ylab("Variable Importance")+
  xlab("Top Features")

#update table outlining the performance of difference models 
options(pillar.sigfig = 5)
ptable1<-cbind(t(tibble(RandomForestF1=rf_acc[["byClass"]][,"F1"])),OverallAccuracy=rf_acc$overall[1],Timetaken=exectime_rf)
ptable2<-rbind(ptable,ptable1)
ptable2
########################################################
#Model 3: Random Forest using only Important variables
#######################################################

#Find the important variable, emphasize variables that are important for class 1,
#i.e. damage grade 1 

imp_var<-imp_rf%>%filter(X1>10)%>%arrange(desc(MeanDecreaseAccuracy))
imp_var_names<-imp_var$rname_rf
imp_var_names<-append(imp_var_names,"damage_grade")
imp_var_names
#subset train and test data, y's are the same 
train_ImpVar<-trainqk_x[,imp_var_names]
test_ImpVar<-testqk_x[,imp_var_names]


# run tuning for mtry 
tic()
tuneRF(train_ImpVar[,-20],trainqk_y,
       mtryStart = 1,
       stepFactor = 2,
       plot = TRUE,ntreeTry = 50,
       trace = TRUE,
       improve = 0.001)
exectime_rftune1 <- toc()
exectime_rftune1 <- exectime_rftune1$toc - exectime_rftune1$tic

#run model with mtry 8 from tuning 

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
tic()
rf_fit1 <- randomForest(damage_grade ~ ., data=train_ImpVar,
                       ntree=50,
                       sampsize=70000,
                       mtry=8,
                       importance = TRUE)
exectime_rf1 <- toc()
exectime_rf1 <- exectime_rf1$toc - exectime_rf1$tic

# get  model info and use it for prediction 

rf_fit1
plot(rf_fit1)
rf_pred1<-predict(rf_fit1,testqk_x)
rf_acc1<-confusionMatrix(rf_pred1, reference=testqk_y, mode = "everything")
rf_acc1

#update table of model performances 
options(pillar.sigfig = 5)
ptable3<-cbind(t(tibble(RandomForestSelectFeatureF1=rf_acc1[["byClass"]][,"F1"])),OverallAccuracy=rf_acc1$overall[1],Timetaken=exectime_rf1)
ptable4<-rbind(ptable2,ptable3)
ptable4


########################################################
#Model 4: Gradient boost using lightgbm 
#######################################################

#getting data in a suitable format 

## We must convert damage_grade to factors starting from number 0 to use multiclass
# For instance: 0, 1, 2, 3, 4, 5...
train_label <- as.numeric(as.factor(trainqk_x$damage_grade)) - 1
test_label <- as.numeric(as.factor(testqk_x$damage_grade)) - 1

#get the data as matrix for lgb dataset 
train_gb <- as.matrix(trainqk_x[,-31])
test_gb <- as.matrix(testqk_x[,-31])

dtrainqk <- lgb.Dataset(data = train_gb, label = train_label)
dtestqk <- lgb.Dataset(data = test_gb, label = test_label)
#validation data set 
valids <- list(test = dtestqk)

#tuning the model
tic()

params <- list(
  learning_rate = 0.1 # multiplication performed on each boosting iteration.
  , num_iterations = 1000 # how many total iterations if not early stopped 
  , objective = "multiclass" # classification more than 2 
  , metric = "multi_error" # metric of choice for evaluation on the test set 
  , num_class = 3 # How many levels to classify 
  , early_stopping_rounds = 50 # Number of maximum iterations without improvements.
  , min_sum_hessian_in_leaf = 0.1 # Prune by minimum hessian requirement  
  , num_threads = 4 # computing 
  , max_depth = -1 # Maximum depth of each trained tree, -1 is unlimited 
  , boosting = "gbdt" # boosting method gradient boosting decision tree 
  , max_bin = 16320 #Number of maximum unique values per feature.
  , num_leaves = 127 # Maximum leaves for each trained tree
)


fit_gb <- lgb.train(
  params
  , data=dtrainqk
  , valids=valids
  , verbose = 0
)

exectime_gb <- toc()
exectime_gb <- exectime_gb$toc - exectime_gb$tic

gb_raw <- data.frame(predict(fit_gb, test_gb, reshape = TRUE))
gb_index<-apply(gb_raw,1,function(x) colnames(gb_raw)[which.max(x)])
gb_pred<-if_else(gb_index=="X1",1, ifelse(gb_index=="X2",2,3))
gb_acc<-confusionMatrix(as.factor(gb_pred),as.factor(as.numeric(test_label)+1),mode = "everything")
gb_acc

#update table of model performances 
options(pillar.sigfig = 5)
ptable5<-cbind(t(tibble(GradientBoostLightGBMF1=gb_acc[["byClass"]][,"F1"])),OverallAccuracy=gb_acc$overall[1],Timetaken=exectime_gb)
ptable6<-rbind(ptable4,ptable5)
ptable6


########################################################
#Model 5: Xtreme Gradient boost using xboost 
#######################################################

xgb_train<-xgb.DMatrix(data=train_gb,label=train_label)
xgb_test<-xgb.DMatrix(data=test_gb,label=test_label)

num_class<- length(unique(train_label))

tic()
xgb_fit<-xgboost(
  data=xgb_train,
  eta=0.2,
  max_depth=15,
  nrounds=50,
  eval_metric="merror",
  objective="multi:softprob",
  num_class = 3,
  nthread = 4
)

exectime_xgb <- toc()
exectime_xgb <- exectime_xgb$toc - exectime_xgb$tic

# Review the final model and results
xgb_fit
xgb_raw<-data.frame(predict(xgb_fit,xgb_test,reshape = TRUE))
xgb_index<-apply(xgb_raw,1,function(x) colnames(xgb_raw)[which.max(x)])
xgb_pred<-if_else(xgb_index=="X1",1, ifelse(xgb_index=="X2",2,3))
xgb_acc<-confusionMatrix(as.factor(xgb_pred),as.factor(as.numeric(test_label)+1),mode = "everything")
xgb_acc

#update table of model performances 
options(pillar.sigfig = 5)
ptable7<-cbind(t(tibble(ExtremeBoosXGBOOSTF1=xgb_acc[["byClass"]][,"F1"])),OverallAccuracy=xgb_acc$overall[1],Timetaken=exectime_xgb)
ptable8<-rbind(ptable6,ptable7)
ptable8


########################################################
#Model 6: Gradient boost lightgbm with weighing  
#######################################################

#getting data in a suitable format 

round(100*prop.table(table(trainqk_x$damage_grade)))

weights_gb <-ifelse(trainqk_x$damage_grade==1,1,if_else(trainqk_x$damage_grade==2,0.4,0.7))
length(weights_gb)
length(trainqk_y)
#get the data as matrix for lgb dataset 
dtrainqk_w <- lgb.Dataset(data = train_gb, label = train_label,weight =weights_gb )
dtestqk_w <- lgb.Dataset(data = test_gb, label = test_label)
#validation data set 
valids_w <- list(test = dtestqk_w)

#parameter for the model
tic()
params_gbw <- list(
  min_data = 300
  , num_leaves = 60
  , num_iterations = 1000
  , early_stopping_rounds = 50
  , learning_rate = 0.4
  , objective = "multiclass"
  , metric = "multi_error"
  , num_class = 3
)
#fit the model 
fit_gbw <- lgb.train(
  params_gbw
  , data=dtrainqk_w
  , valids = valids_w
  , verbose = 0
)

exectime_gbw <- toc()
exectime_gbw <- exectime_gbw$toc - exectime_gbw$tic

gbw_raw <- data.frame(predict(fit_gbw, test_gb, reshape = TRUE))
gbw_index<-apply(gbw_raw,1,function(x) colnames(gbw_raw)[which.max(x)])
gbw_pred<-if_else(gbw_index=="X1",1, ifelse(gbw_index=="X2",2,3))
gbw_acc<-confusionMatrix(as.factor(gbw_pred),as.factor(as.numeric(test_label)+1),mode = "everything")

#update table of model performances 
#update table of model performances 
options(pillar.sigfig = 5)
ptable9<-cbind(t(tibble(GradientBoostwithWeightF1=gbw_acc[["byClass"]][,"F1"])),OverallAccuracy=gbw_acc$overall[1],Timetaken=exectime_gbw)
ptable10<-rbind(ptable8,ptable9)
ptable10


########################################################
#Model 7: Xtreme Gradient boost with weighted samples
#######################################################

xgb_train<-xgb.DMatrix(data=train_gb,label=train_label,weight =weights_gb)
xgb_test<-xgb.DMatrix(data=test_gb,label=test_label)

num_class<- length(unique(train_label))

tic()
xgb_fit1<-xgboost(
  data=xgb_train,
  eta=0.2,
  max_depth=15,
  nrounds=50,
  eval_metric="merror",
  objective="multi:softprob",
  num_class = 3,
  nthread = 4
)

exectime_xgb1 <- toc()
exectime_xgb1 <- exectime_xgb1$toc - exectime_xgb1$tic

# Review the final model and results
xgb_fit1
xgb_raw1<-data.frame(predict(xgb_fit1,xgb_test,reshape = TRUE))
xgb_index1<-apply(xgb_raw1,1,function(x) colnames(xgb_raw1)[which.max(x)])
xgb_pred1<-if_else(xgb_index1=="X1",1, ifelse(xgb_index1=="X2",2,3))
xgb_acc1<-confusionMatrix(as.factor(xgb_pred1),as.factor(as.numeric(test_label)+1),mode = "everything")
xgb_acc1

#update table of model performances 
options(pillar.sigfig = 5)
ptable11<-cbind(t(tibble(ExtremeBoosXGBOOSTWeightF1=xgb_acc1[["byClass"]][,"F1"])),OverallAccuracy=xgb_acc1$overall[1],Timetaken=exectime_xgb1)
ptable12<-rbind(ptable10,ptable11)
ptable12

