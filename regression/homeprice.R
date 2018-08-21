#loading packages
library(car) 
library(MASS) 
library(DAAG) # for cross validation of model
library(lmtest) # for checking homoskedasticity/heteroskedasticity
library(ggplot2) 
library(ggfortify)
library(GGally)
library(e1071)
library(caTools)
library(corrplot)

#####################################
##Loading the data
df <- read.csv("homeprice.csv",header = T,sep=",")

####################################
#understanding the structure of the data frame
summary(df)
str(df)

#The variables CHAS and RAD appears to be a factor so we turn them in to factor using as.factor()
df$chas <- as.factor(df$chas)
df$rad<- as.factor(df$rad)

######################################################
#EDA(Exploratory Data Analysis)
str(df)

summary(df)

######Bi varaite analysis (relation btw dependent and each of independent variables)
#using ggplot
##mv v/s crim

ggplot(df,aes(crim,(mv))+geom_point()+geom_smooth(method = 'lm',color = "red",se =FALSE)
#some decreasing trend and outliers

##mv vs zn
ggplot(df,aes(zn,mv))+geom_point()+geom_smooth(method = 'lm',color = "red",se =FALSE)
#some increasing trend

##mv vs indus
ggplot(df,aes(crim,indus))+geom_point()+geom_smooth(method = "lm",se = FALSE,color="red")
#leverage point

##mv vs chas
ggplot(df,aes(chas,mv))+geom_boxplot()

##mv vs nox
ggplot(df,aes(nox,mv))+geom_point()+geom_smooth(method = "lm",se = FALSE,color="red")
#some decreasing trend  

##mv vs rm
ggplot(df,aes(rm,(mv))+geom_point()+geom_smooth(method = "lm",se = FALSE,color="red")
#increasing trend and some outliers

##mv vs age
ggplot(df,aes(age,mv))+geom_point()+geom_smooth(method = "lm",se = FALSE,color="red")
#we can see some heteroscadesity 

##mv vs dis
ggplot(df,aes(dis,mv))+geom_point()+geom_smooth(method = "lm",se = FALSE,color="red")
#here we cans see some heteroscadesity and outliers

##mv vs rad
ggplot(df,aes(rad,mv))+geom_boxplot()
#outliers

##mv vs tax
ggplot(df,aes(nox,mv))+geom_point()+geom_smooth(method = "lm",se = FALSE,color="red")
#decreasing  non linear trend and leverage points

##mv vs pt
ggplot(df,aes(pt,mv))+geom_point()+geom_smooth(method = "lm",se = FALSE,color="red")
#outliers and leverage points

##mv vs b
ggplot(df,aes(b,mv))+geom_point()+geom_smooth(method = "lm",se = FALSE,color="red")

##mv vs lstat
ggplot(df,aes(lstat,mv))+geom_point()+geom_smooth(method = "lm",se = FALSE,color="red")
#decreasing trend 

##################################################################################
#Objective bivariate analysis(correlation)

str(df)
# check which are non numerical
names(df)
# subset and extract only numeric variables
sub_df =df[,c("crim","zn","indus","nox", "rm" , "age", "dis","tax", "pt" ,"b" ,"lstat","mv")]
# this sub dataset contains only numeric data
# first create correlation matrix and then correlation plot
###first pair plot
ggpairs(sub_df)
# Correlation plot
corrmat = cor(sub_df)

corrplot.mixed(corrmat)

##############################################
###Distribution of mv
summary(df$mv)
mvHist <- hist(df$mv)
skewness(df$mv)
kurtosis(df$mv)
##From the analysis and histogram we can see that the mv has a right skewed distribution
#Thus we conclude that a log transformation will be needed

#######################################################
####Data transformation
#Log transformation of mv variable
logmv <- data.frame(log(df$mv))
colnames(logmv) <- c("logmv")
hist(logmv$logmv)
skewness(logmv$logmv)
kurtosis(logmv$logmv)
#here the distribution is significantly better so now we can proceed to building our linear model

#######################################################
##Fitting the model1
fit1 <- lm(log(mv) ~ ., data = df)
summary(fit1)
#Here the R square value is 0.8109

#checking the heteroscadesity using Breusch-Pagan test
bptest(fit1)
#the p value is very less which says that our data is homoskedastic

#checking multicollinearity using vif()
vif(fit1)
#VIF values higher than 5 are considered to be problematic
#so we drop them from the model
#tax has the highest VIF value at 9.0085, so we drop it first

##################################
#Model 2: Removing Collinear Variables (tax)

fit2 <- update(fit1, ~ . - tax)
vif(fit2)
summary(fit2)
#from the summary we can see some of the variables are non significant and now we remove them

###################################
#model 3: Removing Non-Significant Variables (age, indus, rad)
fit3 <- update(fit2, ~ . - age - indus - rad)
summary(fit3)

#########################################
#Diagnosing for Outliers and High-Leverage Points
outlierTest(fit3, cutoff = Inf, n.max = 15)
# Observations with an absolute value greater than 3 for their studentized residuals are considered problematic, so we remove them from the model.

###########################################
#Model 4:Removing Outliers and High-Leverage Points

df <- df[-c(413, 372, 369, 373, 402, 375, 490, 506,215, 401),]
View(df)
fit4 <- lm(log(mv) ~ . - tax - age - indus - rad, data = df)
summary(fit4)

plot(fit4)

ggplot(data=fit4, aes(residuals(fit4))) + 
  geom_histogram(aes(y =..density..), 
                 col="black", fill="white")
#Here we see that the residuals of our model display a normal distribution

####################################################
#Cross Validation
par(mfrow = c(1,1))
fit4_CV <- suppressWarnings(CVlm(data = df, 
                                 form.lm = formula(fit4), 
                                 m = 10, 
                                 dots = FALSE, 
                                 seed = 1, 
                                 plotit = c("Observed", "Residual"),
                                 main = "Cross-validation for fit4", 
                                 legend.pos="topleft"))

attr(fit4_CV, 'ms')
#0.0284
#With an R-squared value of 0.829 and a Mean Squared Error of 0.0284 model 4 is considered as perfect
