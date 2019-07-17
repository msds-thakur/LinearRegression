#setwd("C:/Users/THAKPR8/Documents/MSDS/410_Sec56/Assignment1/")

##################

mydata <- read.csv(file="ames_housing_data.csv",head=TRUE,sep=",")
str(mydata)
head(mydata)
names(mydata)

#### Create a new data frame with limited variables

subdat <- subset(mydata, select = c('Zoning','LotShape','LotArea','HouseStyle','BldgType','Neighborhood','OverallQual','OverallCond','YrSold','YearBuilt',
                                    'FullBath','BsmtFullBath','BsmtHalfBath','HalfBath','GrLivArea',
                                    'BsmtFinSF1','BsmtFinSF2','TotalBsmtSF','KitchenQual','TotRmsAbvGrd','SalePrice',
                                    'ScreenPorch','OpenPorchSF','WoodDeckSF', 'GarageArea' ))

str(subdat)

####Remove outliers:
ggplot(subdat, aes(x=SalePrice)) + 
  geom_histogram(color="black", binwidth= 10000) +
  labs(title="Distribution of SalePrice") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))  

nrow(subdat) - length (which(subdat$SalePrice <= 500000))
subdat <- subset(subdat, subdat$SalePrice <= 500000)
nrow(subdat)
#-------
ggplot(subdat, aes(x=GrLivArea)) + 
  geom_histogram(color="black", binwidth= 100) +
  labs(title="Distribution of GrLivArea") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))  

nrow(subdat) - length (which(subdat$GrLivArea <= 4000))
subdat <- subset(subdat, subdat$GrLivArea <= 4000)
nrow(subdat)
#-------------------

nrow(subdat) -  length(which(subdat$Zoning != "C (all)" & subdat$Zoning != "I (all)" & subdat$Zoning != "A (agr)" ) )
subdat <- subset(subdat, subdat$Zoning != "C (all)" & subdat$Zoning != "I (all)" & subdat$Zoning != "A (agr)")
nrow(subdat)
#-------------------
ggplot(subdat, aes(x=TotalBsmtSF)) + 
  geom_histogram(color="black", binwidth= 100) +
  labs(title="Distribution of TotalBsmtSF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))  

nrow(subdat) - length (which(subdat$TotalBsmtSF <= 3000))
subdat <- subset(subdat, subdat$TotalBsmtSF <= 3000)
nrow(subdat)
#---------------------
ggplot(subdat, aes(x=LotArea)) + 
  geom_histogram(color="black", binwidth= 1000) +
  labs(title="Distribution of TotalBsmtSF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))  

nrow(subdat) - length (which(subdat$LotArea <= 50000))
subdat <- subset(subdat, subdat$LotArea <= 50000)
nrow(subdat)

#---------------------
nrow(subdat) / nrow(mydata)


###Construct New Variabels:
subdat$HouseAge <- subdat$YrSold - subdat$YearBuilt
subdat$QualityIndex <- subdat$OverallQual * subdat$OverallCond
subdat$TotalSqFtCalc <- subdat$BsmtFinSF1 + subdat$BsmtFinSF2 + subdat$GrLivArea
subdat$TotalBathRooms <- subdat$FullBath + subdat$BsmtFullBath + 0.5*(subdat$BsmtHalfBath + subdat$BsmtHalfBath)
subdat$TotalPorchSF <- subdat$ScreenPorch + subdat$OpenPorchSF + subdat$WoodDeckSF
subdat$price_sqft <- subdat$SalePrice/subdat$TotalSqFtCalc
summary(subdat$price_sqft)
hist(subdat$price_sqft)

# We will also  want to create dummy variables here for categorical vars in our data
# Dummy1: BldgType
# Break into three groups, Single Fam, Townhomes and 2Fam/Duplex
subdat$BldgTypeGrp <-
  ifelse(subdat$BldgType == '1Fam', "grp1", 
         ifelse(subdat$BldgType == 'TwnhsE' | subdat$BldgType == 'TwnhsI', "grp2",
                "grp3"))


## Dummy2: Neighborhood-------------------------------------
library(plyr)

subdat2 <- ddply(subdat, .(Neighborhood), summarise, 
                 MeanPrice = mean(SalePrice))
subdat3 <- ddply(subdat, .(Neighborhood), summarise, 
                 TotalPrice = sum(SalePrice))
subdat4 <- ddply(subdat, .(Neighborhood), summarise, 
                 TotalNgbSqft = sum(TotalSqFtCalc))
subdat34 <- cbind(subdat3,subdat4)
subdat34$AvgPr_Sqft <- subdat34$TotalPrice/subdat34$TotalNgbSqft

subdatall <- subdat2 
subdatall$AvgPr_Sqft <- subdat34$AvgPr_Sqft

ggplot(subdatall, aes(x=AvgPr_Sqft, y=MeanPrice)) + 
  geom_point(color="blue", shape=1,size=3) +
  ggtitle("Scatter Plot") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))+
  geom_vline(xintercept = c(80, 90, 105))

#### Clean up of the Neighborhood varaible  ########
subdat$NbhdGrp <-
  ifelse(subdat$price_sqft<=80, "grp1", 
         ifelse(subdat$price_sqft<=90, "grp2",
                ifelse(subdat$price_sqft<=105, "grp3",
                          "grp4"))) 

#############################
str(subdat)

###############################################
### Task 2 - Predictive Modeling Framework ###
##############################################
# Set the seed on the random number generator so you get the same split every time that
# you run the code.
my.data <- subdat
set.seed(123)
my.data$u <- runif(n=dim(my.data)[1],min=0,max=1)

# Create train/test split;
train.df <- subset(my.data, u<0.70);
test.df  <- subset(my.data, u>=0.70);
names(train.df)

# Let's check the data split, sum of each should total the whole dataset
dim(my.data)[1]
dim(train.df)[1]
dim(test.df)[1]
dim(train.df)[1]+dim(test.df)[1]

##########################################################
### Task 3 - Model ID by Automated Variable Selection ###
#########################################################
names(subdat)
length(subdat)

train.clean <- subset(train.df, select=c("LotArea","TotalBsmtSF","KitchenQual","TotRmsAbvGrd",
                                         "GarageArea","HouseAge","QualityIndex","TotalSqFtCalc","TotalBathRooms",
                                         "TotalPorchSF","BldgTypeGrp","NbhdGrp","price_sqft","SalePrice"))

test.clean <- subset(test.df, select=c("LotArea","TotalBsmtSF","KitchenQual","TotRmsAbvGrd",
                                       "GarageArea","HouseAge","QualityIndex","TotalSqFtCalc","TotalBathRooms",
                                       "TotalPorchSF","BldgTypeGrp","NbhdGrp","price_sqft","SalePrice"))

train.clean <- na.omit(train.clean)
test.clean <- na.omit(test.clean)

dim(train.clean)[1]
dim(train.clean)[1] + dim(test.clean)[1]

#------------------------------------
# Now, define the models
full.lm <- lm(SalePrice ~ .,data=train.clean)
summary(full.lm)

# Step #2
# Define the upper model as the FULL model
upper.lm <- lm(SalePrice ~ .,data=train.clean)
summary(upper.lm)

# Define the lower model as the Intercept model
lower.lm <- lm(SalePrice ~ 1,data=train.clean)
summary(lower.lm)

# Need a SLR to initialize stepwise selection
sqft.lm <- lm(SalePrice ~ TotalSqFtCalc,data=train.clean)
summary(sqft.lm)

#abc.lm <- lm(SalePrice ~  KitchenQual+TotRmsAbvGrd+GarageArea+HouseAge+QualityIndex+TotalSqFtCalc+NbhdGrp ,data=train.clean)
#summary(abc.lm)

# Step #3
# Model Identification
library(MASS)

# Call stepAIC() for variable selection
forward.lm <- stepAIC(object=lower.lm,scope=list(upper=upper.lm,lower=lower.lm),
                      direction=c('forward'))
summary(forward.lm)
AIC(forward.lm)
BIC(forward.lm)

backward.lm <- stepAIC(object=upper.lm,direction=c('backward'))
summary(backward.lm)
AIC(backward.lm)
BIC(backward.lm)

stepwise.lm <- stepAIC(object=sqft.lm,scope=list(upper=formula(upper.lm),lower=~1),
                       direction=c('both'));
summary(stepwise.lm)
AIC(stepwise.lm)
BIC(stepwise.lm)



# Good Model 1
model1 <- lm(formula = SalePrice ~ ., data = train.clean)
summary(model1)
anova(model1)
AIC(model1)
BIC(model1)
mean(residuals(model1)^2) #mse for good1
mean(abs(residuals(model1))) #mae for good1

# Good Model 2 - from fwd lm, same for stepwise
model2 <- lm(formula = SalePrice ~ TotalSqFtCalc + price_sqft + KitchenQual + 
               NbhdGrp + HouseAge + QualityIndex + GarageArea + TotalBathRooms + 
               TotRmsAbvGrd, data = train.clean)
summary(model2)
anova(model2)
AIC(model2)
BIC(model2)
mean(residuals(model2)^2) #mse for good2
mean(abs(residuals(model2))) #mae for good2

# Good Model 3 - from backward
model3 <- lm(formula = SalePrice ~ LotArea + KitchenQual + TotRmsAbvGrd + 
               GarageArea + HouseAge + QualityIndex + TotalSqFtCalc + TotalBathRooms + 
               BldgTypeGrp + NbhdGrp + price_sqft, data = train.clean)
summary(model3)
anova(model3)
AIC(model3)
BIC(model3)
mean(residuals(model3)^2) #mse for good3, also mean(good3$residuals^2) works
mean(abs(residuals(model3))) #mae for good3

#model 4 Junk model.
model4 <- lm(SalePrice ~ OverallQual + OverallCond + QualityIndex + GrLivArea + TotalSqFtCalc, data=train.df)
summary(model4)
anova(model4)
AIC(model4)
BIC(model4)
mean(residuals(model4)^2) #mse for good3, also mean(good3$residuals^2) works
mean(abs(residuals(model4))) #mae for good3


# AIC List
AIC(model1)
AIC(model2)
AIC(model3)
AIC(model4)

# Test BIC
BIC(model1)
BIC(model2) # this has lowest BIC and AIC scores, best model so far
BIC(model3)
BIC(model4)


# Check the VIF values to ensure we don't have multicollinearity
library(car)
sort(vif(forward.lm),decreasing=TRUE)
sort(vif(backward.lm),decreasing=TRUE)
sort(vif(stepwise.lm),decreasing=TRUE)
#sort(vif(bogus.lm), decreasing = TRUE)

sort(vif(model1),decreasing=TRUE)
sort(vif(model2),decreasing=TRUE)
sort(vif(model3),decreasing=TRUE)
sort(vif(model4),decreasing=TRUE)



#########################################
### Task 4 - Predictive Accuracy     ###
########################################

forward.test <- predict(forward.lm,newdata=test.clean)
backward.test <- predict(backward.lm,newdata=test.clean)
stepwise.test <- predict(stepwise.lm,newdata=test.clean)

# Selected Good model tests
good1.test <- predict(model1, newdata = test.clean)
mean((test.clean$SalePrice - predict.lm(model1, test.clean)) ^ 2)
mean(abs(test.clean$SalePrice - predict.lm(model1, test.clean))) 
sqrt(mean((test.clean$SalePrice - predict.lm(model1, test.clean)) ^ 2))

good2.test <- predict(model2, newdata = test.clean)
mean((test.clean$SalePrice - predict.lm(model2, test.clean)) ^ 2)
mean(abs(test.clean$SalePrice - predict.lm(model2, test.clean))) 
sqrt(mean((test.clean$SalePrice - predict.lm(model2, test.clean)) ^ 2))

good3.test <- predict(model3, newdata = test.clean)
mean((test.clean$SalePrice - predict.lm(model3, test.clean)) ^ 2)
mean(abs(test.clean$SalePrice - predict.lm(model3, test.clean))) 
sqrt(mean((test.clean$SalePrice - predict.lm(model3, test.clean)) ^ 2))

good4.test <- predict(model4, newdata = test.df)
mean((test.clean$SalePrice - predict.lm(model4, test.df)) ^ 2)
mean(abs(test.clean$SalePrice - predict.lm(model4, test.df))) 
sqrt(mean((test.clean$SalePrice - predict.lm(model4, test.df)) ^ 2))


########################################
### Task 5 - Operational Validation ###
#######################################

# Training Data
# Abs Pct Error
forward.pct <- abs(forward.lm$residuals)/train.clean$SalePrice;
MAPE <- mean(forward.pct)
MAPE
backward.pct <- abs(backward.lm$residuals)/train.clean$SalePrice;
MAPE <- mean(backward.pct)
MAPE
stepwise.pct <- abs(stepwise.lm$residuals)/train.clean$SalePrice;
MAPE <- mean(stepwise.pct)
MAPE

good1.pct <- abs(model1$residuals)/train.clean$SalePrice;
MAPE <- mean(good1.pct)
MAPE
good2.pct <- abs(model2$residuals)/train.clean$SalePrice;
MAPE <- mean(good2.pct)
MAPE
good3.pct <- abs(model3$residuals)/train.clean$SalePrice;
MAPE <- mean(good3.pct)
MAPE
good4.pct <- abs(model4$residuals)/train.df$SalePrice;
MAPE <- mean(good4.pct)
MAPE

# Test Data
# Abs Pct Error
forward.testPCT <- abs(test.clean$SalePrice-forward.test)/test.clean$SalePrice;
MAPE <- mean(forward.testPCT)
MAPE
backward.testPCT <- abs(test.clean$SalePrice-backward.test)/test.clean$SalePrice;
MAPE <- mean(backward.testPCT)
MAPE
stepwise.testPCT <- abs(test.clean$SalePrice-stepwise.test)/test.clean$SalePrice;
MAPE <- mean(stepwise.testPCT)
MAPE



good1.testpct <- abs(test.clean$SalePrice-good1.test)/test.clean$SalePrice;
MAPE <- mean(good1.testpct)
MAPE
good2.testpct <- abs(test.clean$SalePrice-good2.test)/test.clean$SalePrice;
MAPE <- mean(good2.testpct)
MAPE
good3.testpct <- abs(test.clean$SalePrice-good3.test)/test.clean$SalePrice;
MAPE <- mean(good3.testpct)
MAPE
good4.testpct <- abs(test.df$SalePrice- good4.test)/test.df$SalePrice;
MAPE <- mean(good4.testpct)
MAPE

# Assign Prediction Grades TRAINING data - forward (Good 2)
good.PredictionGrade <- ifelse(good1.pct<=0.10,'Grade 1: [0.0.10]',
                               ifelse(good1.pct<=0.15,'Grade 2: (0.10,0.15]',
                                      ifelse(good1.pct<=0.25,'Grade 3: (0.15,0.25]',
                                             'Grade 4: (0.25+]')))

good.trainTable <- table(good.PredictionGrade)
good.trainTable/sum(good.trainTable)


# Assign Prediction Grades TEST data - forward (Good 2)
good.testPredictionGrade <- ifelse(good1.testpct<=0.10,'Grade 1: [0.0.10]',
                                   ifelse(good1.testpct<=0.15,'Grade 2: (0.10,0.15]',
                                          ifelse(good1.testpct<=0.25,'Grade 3: (0.15,0.25]',
                                                 'Grade 4: (0.25+]')))

good.testTable <-table(good.testPredictionGrade)
good.testTable/sum(good.testTable)

#---------------------
#Model2
# Assign Prediction Grades TRAINING data - forward (Good 2)
good2.PredictionGrade <- ifelse(good2.pct<=0.10,'Grade 1: [0.0.10]',
                               ifelse(good2.pct<=0.15,'Grade 2: (0.10,0.15]',
                                      ifelse(good2.pct<=0.25,'Grade 3: (0.15,0.25]',
                                             'Grade 4: (0.25+]')))

good2.trainTable <- table(good2.PredictionGrade)
good2.trainTable/sum(good2.trainTable)


# Assign Prediction Grades TEST data - forward (Good 2)
good2.testPredictionGrade <- ifelse(good2.testpct<=0.10,'Grade 1: [0.0.10]',
                                   ifelse(good2.testpct<=0.15,'Grade 2: (0.10,0.15]',
                                          ifelse(good2.testpct<=0.25,'Grade 3: (0.15,0.25]',
                                                 'Grade 4: (0.25+]')))

good2.testTable <-table(good2.testPredictionGrade)
good2.testTable/sum(good2.testTable)


#---------------------
#Model3
# Assign Prediction Grades TRAINING data - forward (Good 2)
good3.PredictionGrade <- ifelse(good3.pct<=0.10,'Grade 1: [0.0.10]',
                               ifelse(good3.pct<=0.15,'Grade 2: (0.10,0.15]',
                                      ifelse(good3.pct<=0.25,'Grade 3: (0.15,0.25]',
                                             'Grade 4: (0.25+]')))

good3.trainTable <- table(good3.PredictionGrade)
good3.trainTable/sum(good3.trainTable)


# Assign Prediction Grades TEST data - forward (Good 2)
good3.testPredictionGrade <- ifelse(good3.testpct<=0.10,'Grade 1: [0.0.10]',
                                   ifelse(good3.testpct<=0.15,'Grade 2: (0.10,0.15]',
                                          ifelse(good3.testpct<=0.25,'Grade 3: (0.15,0.25]',
                                                 'Grade 4: (0.25+]')))

good3.testTable <-table(good3.testPredictionGrade)
good3.testTable/sum(good3.testTable)

#---------------------
#Model4
# Assign Prediction Grades TRAINING data - forward (Good 2)
good4.PredictionGrade <- ifelse(good4.pct<=0.10,'Grade 1: [0.0.10]',
                               ifelse(good4.pct<=0.15,'Grade 2: (0.10,0.15]',
                                      ifelse(good4.pct<=0.25,'Grade 3: (0.15,0.25]',
                                             'Grade 4: (0.25+]')))

good4.trainTable <- table(good4.PredictionGrade)
good4.trainTable/sum(good4.trainTable)


# Assign Prediction Grades TEST data - forward (Good 2)
good4.testPredictionGrade <- ifelse(good4.testpct<=0.10,'Grade 1: [0.0.10]',
                                   ifelse(good4.testpct<=0.15,'Grade 2: (0.10,0.15]',
                                          ifelse(good4.testpct<=0.25,'Grade 3: (0.15,0.25]',
                                                 'Grade 4: (0.25+]')))

good4.testTable <-table(good4.testPredictionGrade)
good4.testTable/sum(good4.testTable)



########################################
### Task 6 - Final 'Best' Model     ###
#######################################

# Review the MLR for proposed Best model on the entire sample set once more - Good 2
MLR1_result = lm(formula = SalePrice ~ LotArea + KitchenQual + TotRmsAbvGrd + 
                   GarageArea + HouseAge + QualityIndex + TotalSqFtCalc + TotalBathRooms + 
                   BldgTypeGrp + NbhdGrp + price_sqft, data = train.clean)

# Report the model and interpret the coefficients.
anova(MLR1_result)
summary(MLR1_result)
# Does the predicted model go through the mean of Y in each category?
# Plot MLR Result
par(mfrow=c(2,2))
plot(MLR1_result)

pred <- as.data.frame(predict(MLR1_result,sampledat))
names(pred)
library(reshape)
pred <- rename(pred, c("predict(MLR1_result, sampledat)" = "prd"))
sampledat$pred <- pred$prd
sampledat$res <- sampledat$SalePrice - sampledat$pred
sampledat$absres <- abs(sampledat$res)
MAE12 <- mean(sampledat$absres)
MAE12

# # Show the residual plot with line thru the mean for each category
# ggplot(sampledat, aes(x=kq_gd, y=res)) + 
#   geom_point(color="blue", size=2) +
#   ggtitle("Scatter Plot of Residual vs Predictor") +
#   theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
#   geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###

# Recheck Influence?
par(mfrow=c(1,1))
influencePlot(MLR1_result,	id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")

inflm.MLR1 <- influence.measures(MLR1_result)
summary(inflm.MLR1)
dffits <- dffits(MLR1_result)
summary(dffits)
#sampledat <- cbind(sampledat,dffits)
#str(sampledat)
nrow(sampledat)
# First determine the threshold for the influential points - trial #1
numpredictors <- 17
dfits_threshold <- 2 * sqrt( (numpredictors+1) / (nrow(sampledat)-numpredictors-1) )
dfits_threshold

# Remove influential points - trial #1
sampledat$absdf <- abs(sampledat$dffits)
subdatnuminf <- sampledat[which(sampledat$absdf < dfits_threshold),]
sort(subdatnuminf,decreasing=TRUE)
nrow(subdatnuminf)



########################################
### Conclusion & Misc Tests
########################################

# See report document
