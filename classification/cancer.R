
##Loading libraries
library(ggplot2)
library(caTools)
library(tidyr)
library(mice)
library(ROCR)
library(randomForest)
library(adabag)

####Loading data
bc_data <- read.table("cancerdataset.txt", 
                      header = FALSE, 
                      sep = ",")

#giving appropriate column names
colnames(bc_data) <- c("sample_code_number", 
                       "clump_thickness", 
                       "uniformity_of_cell_size", 
                       "uniformity_of_cell_shape", 
                       "marginal_adhesion", 
                       "single_epithelial_cell_size", 
                       "bare_nuclei", 
                       "bland_chromatin", 
                       "normal_nucleoli", 
                       "mitosis", 
                       "classes")


bc_data$classes <- ifelse(bc_data$classes == "2", "benign",
                          ifelse(bc_data$classes == "4", "malignant", NA))
#converting all "?" to NA
bc_data[bc_data == "?"] <- NA

### how many NAs are in the data
length(which(is.na(bc_data)))

# how many samples would we loose, if we removed them?
nrow(bc_data)

nrow(bc_data[is.na(bc_data), ])


# impute missing data
bc_data[,2:10] <- apply(bc_data[, 2:10], 2, function(x) as.numeric(as.character(x)))
dataset_impute <- mice(bc_data[, 2:10],  print = FALSE)
bc_data <- cbind(bc_data[, 11, drop = FALSE], mice::complete(dataset_impute, 1))


#converting target variable to factor
bc_data$classes <- as.factor(bc_data$classes)


#########Exploring the target variable
summary(bc_data$classes)      

ggplot(bc_data, aes(x = classes, fill = classes)) +
  geom_bar()

########bivariate analysis
#classes vs clump_thickness
ggplot(bc_data,aes(classes,clump_thickness))+geom_boxplot()

#classes vs uniformity of cell sizes
ggplot(bc_data,aes(classes,uniformity_of_cell_size))+geom_boxplot()

#classes vs bare_nuclei
ggplot(bc_data,aes(classes,bare_nuclei))+geom_boxplot()

#classes vs bland_chromatin
ggplot(bc_data,aes(classes,bland_chromatin))+geom_boxplot()

#classes vs marginal_adhesion 
ggplot(bc_data,aes(classes,marginal_adhesion ))+geom_boxplot()

#classes vs mitosis
ggplot(bc_data,aes(classes,mitosis))+geom_boxplot()

#classes vs normal_nucleoli
ggplot(bc_data,aes(classes,normal_nucleoli))+geom_boxplot()


#classes vs single_epitheliai_cell_size
ggplot(bc_data,aes(classes,single_epithelial_cell_size))+geom_boxplot()

#classes vs uniformity_of_cell_shape
ggplot(bc_data,aes(classes, uniformity_of_cell_shape))+geom_boxplot()


gather(bc_data, x, y, clump_thickness:mitosis) %>%
  ggplot(aes(x = y, color = classes, fill = classes)) +
  geom_density(alpha = 0.3) +
  facet_wrap( ~ x, scales = "free", ncol = 3)

############Splitting the data
set.seed(123)
ids = sample( nrow(bc_data), nrow(bc_data)*0.7)

train = bc_data[ids,]
test =  bc_data[-ids,]


##############Logistic Regression
#model 1
set.seed(123)
model_glm <- glm(classes ~ .,data = train,family="binomial")
summary(model_glm) 

### Predict the test observations using model 1
test$pred = predict( model_glm , newdata = test, type="response")
#first we take cut off as 0.5
test$pred_class = ifelse( test$pred >= 0.5, 1 , 0)

#confusion matrix
table(test$pred_class, test$classes )

#   benign malignant
#0    129         6
#1      5        70


#Precision 
#70/(70+6)= 0.92
#Recall
#70/(70+5)= 0.93
#F1 score #
#(2*0.92*0.93)/(0.92+0.93)= 0.92

#####ROC graphs 

### add the ROC graph to model1  
pred = prediction(test$pred , test$classes)
perf= performance(pred, "tpr","fpr")
plot(perf)

#from the ROC plot we take 0.95 as cutoff
test$pred_class = ifelse( test$pred >= 0.95, 1 , 0)

#confusion matrix
table(test$pred_class, test$classes )
#benign malignant
#0    133        15
#1      1        61
#Precision: 61/(61+15)=0.8
#Recall: 61/(61+1) = 0.98
#F1 score: 2*0.8*0.98/(0.8+0.98) = 0.88

#AUC for model1
AUC_1 = performance(pred, measure = 'auc')@y.values[[1]]
AUC_1
##AUC is 99.3


##########implementing Random Forest
set.seed(123)
classifier = randomForest(x = train[,-1],
                          y = train$classes,
                          ntree = 500)
plot(classifier)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test[,-1])

# Making the Confusion Matrix
cm = table(test[,1], y_pred)
cm

#Precision
#75/(75+4)=0.949

#Recall
#75/(75+1)=0.986

#F1 score
#(2*0.949*0.986)/(0.949+0.986)=0.96


##############Implementing Boosting 

adaboost = boosting(classes ~ ., data = train)
adaboost$importance
y_pred_adaboost = predict(adaboost, newdata = test)
y_pred_adaboost
cm_adaboost = table(test[, 1], y_pred_adaboost$class)
cm_adaboost

#Precision
#74/(74+5)=0.936

#Recall
#74/(74+2)= 0.973

#F1 score
#(2*0.936*0.973)/(0.936+0.973)= 0.954


####################################
#on comparing all the three models, Random forest is giving best results with an F1 score of 0.96

