### Attached is a dataset. We estimate the best model for the data provided by using a use a simple OLS regression, a polynomial regression, or a random forest
### These data are by country and represent how well political parties do in terms of securing cabinet seats in a coalition government. If a party receives 0 seats you can assume they are not in the coalition government; more than 0 indicates they are members. All parties prefer more seats to fewer. See: https://en.m.wikipedia.org/wiki/Coalition_government
### Target variable (Y) is cabinet_proportion – the percentage of cabinet seats a party gets.

#Loading libraries
library(tidyr)
library(dplyr)
library(plotly)
library(corrr)
library(car)
library(ridge)
library(caret)
library(MASS)
library(leaps)
library(randomForest)
library(corrplot)
library(party)
library(tree)
library(rpart)
library(multcomp)
library(glmnet)


#Get working directory
#getwd()
#setwd("")

#Import data
data<- read.csv("dataset.csv", header=T)

#Have a look at data
glimpse(data)

# Checking Unique no. of Party_names and Country
length(unique(data$party_name)); length(unique(data$country))

#Count missing values across all Variables
na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

#Checking frequency of D.V. and Selection_Year
table(data$cabinet_proportion); table(data$election_year)

#Checking data type of all variables
str(data)
#
#Eliminating factor variables for further analysis. 
#Character variables, Variables with missing values, duplicated varioables(variables contributing same information) are removed here.
drops <- c("country","cabinet_name","party_name_english", "party", "sq_cabinet", "sq_pm", "left_rightx", "left_righty", "seats_share", "cabinet_seats",
           "country_dummy1", "country_dummy2", "country_dummy3", "country_dummy4", "country_dummy5", "country_dummy6", "country_dummy7", "country_dummy8", "country_dummy9", "country_dummy10", "country_dummy11","country_dummy12", "country_dummy13")

data_num <- data[ , !(names(data) %in% drops)]

#Rechecking data type
str(data_num)

#Missing value re-check in data
sum(is.na(data_num))

#Correlation table and Correlation graph between Numeric variables
cor<-data_num %>% correlate() %>% rearrange() %>% fashion()
data_num %>% correlate() %>% rearrange()   %>% rplot()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### A Dummy column to check how "Cabinet proportion" was derived 
# data_num$calculated_cabprop <- data_num$cabinet_seats/data_num$total_cabinet_size
# #The above variable is equivalent to "Cabinet Proportion". From this we understand how "Cabinet proportion" variable was derived.

########################################## TRAIN/TEST - 70/30 split ########################################

# Split the data into training and test set
set.seed(123)

# Shuffle the dataset, call the result shuffled
n <- nrow(data_num)
shuffled <- data_num[sample(n),]

# Split the data in train and test
train <- 1: round(.7*n)
test <- (round(.3*n)+1):n
train <- shuffled[train, ]
test <- shuffled[test, ]

str(train); str(test)
dim(train); dim(test)

################# Model 1- MLR with Train/Test split #################

model1<-lm(formula=cabinet_proportion~.,data=train)
summary(model1)
coef(model1)

#Check Multicollinearity. If VIF>5 Multicollinearity exists.
vif(model1) 

pred<-predict(model1,test)
res<-(test$cabinet_proportion-pred)

RMSE_lm<-sqrt(mean(res**2))  #Root mean square error
RMSE_lm


################ Model 2- STEPWISE REGRESSION with Train/Test split ################

step.model <- model1 %>% stepAIC(trace = FALSE)
coef(step.model)
summary(step.model)

# Make predictions FULL model
test$pred<-predict(model1,test)
test$res<-(test$cabinet_proportion-test$pred)
RMSEtest<-sqrt(mean(test$res**2))
RMSEtest

# Make predictions STEPWISE model
test$pred<-predict(step.model,test)
test$res<-(test$cabinet_proportion-test$pred)
RMSEtest<-sqrt(mean(test$res**2))
RMSEtest

# Above, the stepwise regression model has selected a reduced number of predictor variables resulting to a final model, which performance was similar to the one of the full model.
# So, the stepwise selection reduced the complexity of the model without compromising its accuracy. Note that, all things equal, we should always choose the simpler model, here the final model returned by the stepwise regression.



################ Model 3- DECISION TREE with Train/Test split ######################

# Train the rpart algorithm 
model_DT <- rpart(cabinet_proportion~., data = train)
summary(model_DT)
library(rpart.plot)

# Predict the fraud probabilities of the test cases
test_DT <- predict(model_DT, newdata = test, method="prob")

#Accuracy RMSE
mean((test_DT-test$cabinet_proportion)^2) #MSE
sqrt(mean((test_DT-test$cabinet_proportion)^2)) #RMSE




################## Model 4- RANDOM FOREST with Train/Test split ###################
# Reference link - http://rstudio-pubs-static.s3.amazonaws.com/156481_80ee6ee3a0414fd38f5d3ad33d14c771.html
# Link: https://www.kaggle.com/grosvenpaul/beginners-guide-to-eda-and-random-forest-using-r


tree.cabinet<- tree(cabinet_proportion~.,train)
summary(tree.cabinet)

#Plot the tree.
plot(tree.cabinet)
text(tree.cabinet,pretty=0)
#Inference- The splus variable measures the different power measure of party The tree shows that higher values of shapley correspond to higher seats.

# # We now use 10-fold cross validation ( using cv.tree() function ) in order to determine the optimal level of tree complexity. This will help us decide whether pruning the tree will improve performance.
# cv.cabinet <- cv.tree(tree.cabinet)
# cv.cabinet  #dev corresponds to cross validation error
# plot(cv.cabinet$size,cv.cabinet$dev,type='b')
# #Inference- The lowest dev corresponds to the tree with 6 leaves.
# 
# #Code if we decide to prune the tree
# prune.cabinet=prune.tree(tree.cabinet,best=5)
# summary(prune.cabinet)
# plot(prune.cabinet)
# text(prune.cabinet,pretty=0)
# 
# #But ultimately, we go with the cross-validation results and use the unpruned tree to make predictions on the test set.
# yhat<-predict(tree.cabinet,newdata=test)
# cabinet.test<-test$cabinet_proportion
# plot(yhat,cabinet.test)
# #abline(0,1)
# mean((yhat-cabinet.test)^2); sqrt(mean((yhat-cabinet.test)^2))
# #Inference- So the test set MSE for the regression tree is 0.0055, with its square root around 0.0744, meaning that this model gives predictions that are within around 0.0744 of the true cabinet proportion

# RF Model
model_rf <- randomForest(cabinet_proportion~., data = train, importance = TRUE, mtry=18, ntree=35)
model_rf

print(model_rf)

#Predicting values on test
yhat.bag <- predict(model_rf,newdata=test)
cabinet.test<-test$cabinet_proportion

#Accuracy RMSE
mean((yhat.bag-cabinet.test)^2) #MSE
sqrt(mean((yhat.bag-cabinet.test)^2)) #RMSE
#Inference- So, the test set MSE for the regression tree is 0.0055, with its square root around 0.0744, meaning that this model gives predictions that are within around 0.0744 of the true cabinet proportion


# Get importance of variables  (Not mandatory)
importance    <- importance(model_rf)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'IncNodePurity'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_classic()

# Aha! The results of our Random Forest Model coincides nicely with our previous observation from the regression analysis that cabinet party and prime minister are the most discriminating factor of Cabinet proportion.




###################### Model 5- LASSO Regression with Train/Test split ######################

# In lasso the coefficients which are responsible for large variance are converted to zero. Lasso regression analysis is also used for variable selection as the model imposes coefficients of some variables to shrink towards zero.
# Important Note-
# The LASSO is not very good at handling variables which show correlation between them and thus can sometimes show very wild behaviors.
# Which is our case with data.

#Reference link- http://www.science.smith.edu/~jcrouser/SDS293/labs/lab10-r.html

# In order to fit a lasso model, we once again use the glmnet() function; however, this time we use the argument alpha<-1. 
# We now split the samples into a training set and a test set in order to estimate the test error of Lasso regression.
# Split the data into training and test set
set.seed(123)

# Shuffle the dataset, call the result shuffled
n <- nrow(data_num)
shuffled <- data_num[sample(n),]

#Splitting into train and test
train <- 1: round(.7*n)
test <- (round(.7*n)+1):n
train <- shuffled[train, ]
test <- shuffled[test, ]

x_train <- model.matrix(cabinet_proportion~., train)[,-1]
x_test <- model.matrix(cabinet_proportion~., test)[,-1]

y_train <-  train[,("cabinet_proportion")] %>%
  unlist() %>%
  as.numeric()

y_test <- test[,("cabinet_proportion")] %>%
  unlist() %>%
  as.numeric()

lasso_mod <-glmnet(x_train,  y_train,  alpha = 1) # Fit lasso model on training data
plot(lasso_mod)    # Draw plot of coefficients

cv.out = cv.glmnet(x_train, y_train, alpha = 1) # Fit lasso model on training data
plot(cv.out) # Draw plot of training MSE as a function of lambda
bestlam = cv.out$lambda.min # Select lamda that minimizes training MSE
lasso_pred = predict(lasso_mod, s = bestlam, newx = x_test) # Use best lambda to predict test data
mean((lasso_pred - y_test)^2) # Calculate test MSE
sqrt(mean((lasso_pred - y_test)^2)) # Calculate test RMSE

#Inference- This is little higher than the test set MSE of the RF model and Decision tree, and very similar to the test MSE of ridge regression with  λ chosen by cross-validation.



###################### Model 6- Principal Component Analysis (PCA) ####################

#Check Multicollinearity. If VIF>5 Multicollinearity exists.
vif(model1)

## Why PCA ? ###
# When there is high multicollinearity in the data one of the method to deal with severe multicollinearity is 'Principal Component Regression'.
# PCA uses correlation structure of original variables and derives p linear combinations which are uncorrelated.

# Because PCA works best with numerical data, you'll exclude the two categorical variables 
#Eliminating Dependent variables for further analysis.Note- Dependent variable should not be taken into account during PCA
drops <- ("cabinet_proportion")
vars <- data_num[ , !(names(data_num) %in% drops)]

# run pca and scale vars in same function call for a 2 latent var model
vars.pca <- prcomp(vars, center = TRUE, scale. = TRUE)

# look at summary of results of pca -- most salient thing is % variance explained by each latent variable
summary(vars.pca)
#Inference- First Principal Component explains 22% of the variation.

# now look at loadings of raw variables in vars on the latent vars -- note it separates into pass vs run as expected
vars.pca
# First Principal Component can be interpreted as "power variable of a party‟ since all variables have similar loadings.

# list data in the object generated by the pca -- this is purely informational
attributes(vars.pca)

# show data in pca -- we have two latent variables 
vars.pca$x

# add data from pca to original set
# and of course, by hand, you should go through several rows starting with data, then inc, then the pca additions and check manually that there's no mistakes anywhere
# and instead of doing this with the $ operator, one can use inc["pc1"] = vars.pca$x[,"PC1"]... instead
inc$pc1 = vars.pca$x[,"PC1"]
inc$pc2 = vars.pca$x[,"PC2"]



############################## Model 7- BOOTSTRAP on RF ###############################

#Referral Link - https://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/

#Load caret library 

# define training control
train_control <- trainControl(method="boot", number=100)

# train the model
model <- train(cabinet_proportion~., data=train, trControl=train_control, method="rf")

# summarize results
print(model)
summary(model)

#Predicting values on test
yhat.boot <- predict(model,newdata=test)
y.test<-test$cabinet_proportion

#Accuracy RMSE
mean((yhat.boot-y.test)^2) #MSE
sqrt(mean((yhat.boot-y.test)^2)) #RMSE
#Inference- So, the test set MSE for the regression tree is 0.059, with its square root around 0.0599, which is better than accuracy given by just RF model above.

# Referral link for OOB ERROR : https://stats.stackexchange.com/questions/369134/random-forest-out-of-bag-rmse
