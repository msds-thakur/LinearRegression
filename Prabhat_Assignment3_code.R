##################

mydata <- read.csv(file="ames_housing_data.csv",head=TRUE,sep=",")


str(mydata)
head(mydata)
names(mydata)
mydata$TotalFloorSF <- mydata$FirstFlrSF + mydata$SecondFlrSF
mydata$HouseAge <- mydata$YrSold - mydata$YearBuilt
mydata$QualityIndex <- mydata$OverallQual * mydata$OverallCond
mydata$logSalePrice <- log(mydata$SalePrice)
mydata$price_sqft <- mydata$SalePrice/mydata$TotalFloorSF
summary(mydata$price_sqft)
hist(mydata$price_sqft)
subdat <- subset(mydata, select=c("TotalFloorSF","HouseAge","QualityIndex",'Zoning','KitchenQual',
                                  "price_sqft", "SalePrice","LotArea",'GarageArea',
                                  "BsmtFinSF1","Neighborhood","HouseStyle",
                                  "LotShape","OverallQual","logSalePrice",
                                  "TotalBsmtSF","BldgType",'GrLivArea','SaleCondition'))

str(subdat)


subdatnum <- subset(mydata, select=c("TotalFloorSF","HouseAge","QualityIndex",
                                     "SalePrice","LotArea","OverallQual","logSalePrice"))
summary (subdat$KitchenQual)  
---------------------------------------------------------
  ggplot(subdat, aes(x=SalePrice)) + 
  geom_histogram(color="black", binwidth= 10000) +
  labs(title="Distribution of Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

summary(subdat$SalePrice)
length (which(subdat$SalePrice >450000))
subdat <- subset(subdat, subdat$SalePrice <= 450000)  #32
nrow(subdat)
---------------------------------------------------------------
  #Not using
  ggplot(subdat, aes(x=TotalFloorSF)) + 
  geom_histogram(color="black", binwidth= 100) +
  labs(title="Distribution of TotalFloorSF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))  

length (which(subdat$TotalFloorSF >3500))
#subdat <- subset(subdat, subdat$TotalFloorSF <= 450000)
#nrow(subdat)

---------------------------------------------------------------
  ggplot(subdat, aes(x=GrLivArea)) + 
  geom_histogram(color="black", binwidth= 100) +
  labs(title="Distribution of GrLivArea") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))  

length (which(subdat$GrLivArea >3500))
subdat <- subset(subdat, subdat$GrLivArea <= 3500)
nrow(subdat)

-------------------
summary (subdat$Zoning)  

length(which(subdat$Zoning != "C (all)" & subdat$Zoning != "I (all)" & subdat$Zoning != "A (agr)" ) )

subdat <- subset(subdat, subdat$Zoning != "C (all)" & subdat$Zoning != "I (all)" & subdat$Zoning != "A (agr)")
nrow(subdat)

---------------------------------------------------------------
  ggplot(subdat, aes(x=TotalBsmtSF)) + 
  geom_histogram(color="black", binwidth= 100) +
  labs(title="Distribution of TotalBsmtSF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))  

length (which(subdat$TotalBsmtSF > 3000))
subdat <- subset(subdat, subdat$TotalBsmtSF <= 3000)
nrow(subdat)

---------------------------------------------------------------
  ggplot(subdat) +
  geom_bar( aes(SaleCondition) ) +
  ggtitle("Number of houses per SaleCondition") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))


length (which(subdat$SaleCondition != 'Normal'))
#subdat <- subset(subdat, subdat$TotalBsmtSF <= 3000)
#nrow(subdat)


nrow(subdat) / nrow(mydata)

---------------------------------------------
  p1 <- ggplot(subdat, aes(x=GrLivArea)) + 
  geom_histogram(color="black", binwidth= 100) +
  labs(title="Distribution of GrLivArea") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

p2<- ggplot(subdat, aes(x=GrLivArea, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of SalePrice by GrLivArea") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

multiplot(p1, p2, cols=2)
-------------------------------
  
p1 <- ggplot(subdat, aes(x=OverallQual)) + 
  geom_bar(color="black") + labs(title="Number of houses per OverallQual") +
  labs(title="Distribution of OverallQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

p2<- ggplot(subdat, aes(x=OverallQual, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of SalePrice by OverallQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

multiplot(p1, p2, cols=2)



------------------------------------
  multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }


###################################################################
##################  Assignment 2  ################################
#################################################################

attach(subdat)

ggplot(subdat, aes(x=GrLivArea, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs GrLivArea") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=TRUE)  ## method=lm, se=FALSE ###

ggplot(subdat, aes(x=OverallQual, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs OverallQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm)  ## method=lm, se=FALSE ###



# 3D Scatterplot with Coloring and Vertical Lines
# and Regression Plane 
library(scatterplot3d)
attach(subdat)
s3d <-scatterplot3d(GrLivArea,OverallQual,SalePrice,pch=16, 
                    highlight.3d=TRUE,type="h", main="3D Scatterplot")
fit <- lm(SalePrice ~ GrLivArea + OverallQual) 
s3d$plane3d(fit)

library(Rcmdr)
attach(subdat)
scatter3d(SalePrice,GrLivArea,OverallQual)

############## fitting a SLR ###################################

SLRresult = lm(SalePrice ~ GrLivArea, data=subdat)#subdat2
anova(SLRresult)
summary(SLRresult)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(SLRresult)

pred <- as.data.frame(predict(SLRresult,subdat,interval="prediction"))
str(pred)
head(pred)
subdat <- cbind(subdat,pred)
str(subdat)
head(subdat)
subdat <- subset( subdat, select = -lwr)
subdat <- subset( subdat, select = -upr)
library(reshape)
subdat <- rename(subdat, c(fit="fitSLR"))

head(subdat)

---------------------------------
SLRresult1 = lm(SalePrice ~ OverallQual, data=subdat)#subdat2
anova(SLRresult1)
summary(SLRresult1)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(SLRresult1)

pred1 <- as.data.frame(predict(SLRresult1,subdat,interval="prediction"))
str(pred1)
head(pred1)
subdat <- cbind(subdat,pred1)
str(subdat)
head(subdat)
subdat <- subset( subdat, select = -lwr)
subdat <- subset( subdat, select = -upr)
#library(reshape)
subdat <- rename(subdat, c(fit="fitSLR1"))

head(subdat)


############## fitting a MLR ###################################

MLRresult = lm(SalePrice ~ GrLivArea+OverallQual, data=subdat)
anova(MLRresult)
summary(MLRresult)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresult)

pred <- as.data.frame(predict(MLRresult,subdat,interval="prediction"))
str(pred)
head(pred)
subdat <- cbind(subdat,pred)
subdat <- subset( subdat, select = -lwr)
subdat <- subset( subdat, select = -upr)
str(subdat)
head(subdat)
subdat <- rename(subdat, c(fit="fitMLR"))
subdat$res <- subdat$SalePrice - subdat$fitMLR

head(subdat)


### Section 4 - Log ( SalePrice )
########################################

# SLR1 for log(SalePrice) - GrLivArea
log_SLRresult1 = lm(log(SalePrice) ~ GrLivArea, data=subdat)
anova(log_SLRresult1)
summary(log_SLRresult1)

# Compare lm results with non-transformed results
#log_fit1

# pred <- as.data.frame(predict(SLRresult,sampledat,interval="prediction"))
# str(pred)
# summary(pred)


# SLR2 for log(SalePrice) - TotalBsmtSF
log_SLRresult2 = lm(log(SalePrice) ~ OverallQual, data=subdat)
anova(log_SLRresult2)
summary(log_SLRresult2)



# Second show the plots using log(SalePrice)
p1 <- ggplot(subdat, aes(x=GrLivArea, y=log(SalePrice))) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Log Sale Price vs GrLivArea") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=TRUE)  ## method=lm, se=FALSE ###

p2 <- ggplot(subdat, aes(x=OverallQual, y=log(SalePrice))) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Log Sale Price vs OverallQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm)  ## method=lm, se=FALSE ###

multiplot(p1, p2, cols=2)

# MLR for log(SalePrice)
log_MLRresult = lm(log(SalePrice) ~ GrLivArea+OverallQual, data=subdat)
anova(log_MLRresult)
summary(log_MLRresult)
par(mfrow=c(2,2))



gvlma::gvlma(x = log_MLRresult)
plot(log_MLRresult)
#----------------------------------------------

############################################
### Task 1 - Select Categorical Variable ###
###########################################

# Choose categorical variable: KitchenQual
#kq <- mydata$KitchenQual
class(subdat$KitchenQual)
summary(subdat$KitchenQual)
# Others tested: HouseStyle

aggregate(SalePrice~KitchenQual, FUN=mean, data=subdat)

# Simple Linear Model
SLRresult = lm(SalePrice ~ KitchenQual, data=subdat)
anova(SLRresult)
summary(SLRresult)

# In summary output, Intercept gives KitchenQualEx. Others are listed as amount subtracted from Ex.
# Does the predicted model go through the mean of Y in each category?
par(mfrow=c(2,2))  # visualize four graphs at once
plot(SLRresult)
306968-97177
306968-166447
306968-197278
306968-199468

#---------------------------------

# Dummy vars, use Ex as the basis category
subdat$KQ_GD <- ifelse(subdat$KitchenQual == "Gd", 1, 0)
subdat$KQ_TA <- ifelse(subdat$KitchenQual == "TA", 1, 0)
subdat$KQ_FA <- ifelse(subdat$KitchenQual == "Fa", 1, 0)
subdat$KQ_PO <- ifelse(subdat$KitchenQual == "Po", 1, 0)
#sampledat$kq_po <- ifelse(sampledat$KitchenQual == "Po", 1, 0)

# Fit a multiple regression model using the dummy coded variables.
MLRKQ_result = lm(SalePrice ~ KQ_GD + KQ_TA + KQ_FA + KQ_PO, data = subdat)

# Report the model and interpret the coefficients.
summary(MLRKQ_result)
anova(MLRKQ_result)


# Does the predicted model go through the mean of Y in each category?
# Plot MLR Result
par(mfrow=c(2,2))
plot(MLRkq_result)

#---------------------------------------
############## fitting a MLR ###################################

MLRresult = lm(SalePrice ~ GrLivArea+TotalBsmtSF, data=subdat)
anova(MLRresult)
summary(MLRresult)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresult)

pred <- as.data.frame(predict(MLRresult,subdat,interval="prediction"))
str(pred)
head(pred)
subdat <- cbind(subdat,pred)
subdat <- subset( subdat, select = -lwr)
subdat <- subset( subdat, select = -upr)
str(subdat)
head(subdat)
library(reshape)
subdat <- rename(subdat, c(fit="fitMLR"))
subdat$res <- subdat$SalePrice - subdat$fitMLR
subdat <- rename(subdat, c(res="Model1_Residual"))
subdat$absres <- abs(subdat$Model1_Residual)
MAE <- mean(subdat$absres)
MAE
head(subdat)

#-------------------------------------
#question 5
require(ggplot2)
ggplot(subdat, aes(y=Model1_Residual)) + 
  geom_boxplot(aes(x=reorder(Neighborhood,Model1_Residual)),fill="blue") +
  labs(title="Distribution of Residuals from Model 1") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))


library(plyr)
subdat1 <- ddply(subdat, .(Neighborhood), summarise, 
                 MAE = mean(absres))
subdat2 <- ddply(subdat, .(Neighborhood), summarise, 
                 MeanPrice = mean(SalePrice))
subdat3 <- ddply(subdat, .(Neighborhood), summarise, 
                 TotalPrice = sum(SalePrice))
subdat4 <- ddply(subdat, .(Neighborhood), summarise, 
                 TotalSqft = sum(TotalFloorSF))
subdat34 <- cbind(subdat3,subdat4)
subdat34$AvgPr_Sqft <- subdat34$TotalPrice/subdat34$TotalSqft

subdatall <- subdat1
subdatall$MeanPrice <- subdat2$MeanPrice
subdatall$AvgPr_Sqft <- subdat34$AvgPr_Sqft

require(ggplot2)
ggplot(subdatall, aes(x=AvgPr_Sqft, y=MeanPrice)) + 
  geom_point(color="blue", shape=1,size=3) +
  ggtitle("Scatter Plot") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))+
  geom_vline(xintercept = c(100, 114, 128,142))

#### Clean up of the Neighborhood varaible  ########

subdat$NbhdGrp <-
  ifelse(subdat$price_sqft<=100, "grp1", 
         ifelse(subdat$price_sqft<=114, "grp2",
                ifelse(subdat$price_sqft<=128, "grp3",
                       ifelse(subdat$price_sqft<=142, "grp4",
                              "grp5")))) 


subdat$Ngrp2 <- 
  ifelse(subdat$NbhdGrp == "grp2", 1, 0)
subdat$Ngrp3 <- 
  ifelse(subdat$NbhdGrp == "grp3", 1, 0)
subdat$Ngrp4 <- 
  ifelse(subdat$NbhdGrp == "grp4", 1, 0)
subdat$Ngrp5 <- 
  ifelse(subdat$NbhdGrp == "grp5", 1, 0)


MLRresultM2 = lm(SalePrice ~ GrLivArea+TotalBsmtSF+Ngrp2+Ngrp3+Ngrp4+Ngrp5, data=subdat)
anova(MLRresultM2)
summary(MLRresultM2)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresultM2)


pred1 <- as.data.frame(predict(MLRresultM2,subdat,interval="prediction"))

subdat <- cbind(subdat,pred1)
subdat <- subset( subdat, select = -lwr)
subdat <- subset( subdat, select = -upr)

subdat <- rename(subdat, c(fit="fitMLR2"))
subdat$Model2_Residual <- subdat$SalePrice - subdat$fitMLR2
subdat$absres2 <- abs(subdat$Model2_Residual)
MAEM2 <- mean(subdat$absres2)
MAEM2

polynomial
head(subdat)
#subdatX <- subdat

#Model 3------------------------------
subdat <- na.omit(subdat)

MLRresultM3 = lm(SalePrice ~ GrLivArea+TotalBsmtSF+LotArea+GarageArea+OverallQual+Ngrp2+Ngrp3+Ngrp4+Ngrp5, data=subdat)
anova(MLRresultM3)
summary(MLRresultM3)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresultM3)


pred3 <- as.data.frame(predict(MLRresultM3,subdat,interval="prediction"))

subdat <- cbind(subdat,pred3)
subdat <- subset( subdat, select = -lwr)
subdat <- subset( subdat, select = -upr)

subdat <- rename(subdat, c(fit="fitMLR3"))
subdat$Model3_Residual <- subdat$SalePrice - subdat$fitMLR3
subdat$absres3 <- abs(subdat$Model3_Residual)
MAEM3 <- mean(subdat$absres3)
MAEM3

#Model 4------------------------------

MLRresultM4 = lm(logSalePrice ~ GrLivArea+TotalBsmtSF+LotArea+GarageArea+OverallQual+Ngrp2+Ngrp3+Ngrp4+Ngrp5, data=subdat)
anova(MLRresultM4)
summary(MLRresultM4)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresultM4)


pred4 <- as.data.frame(predict(MLRresultM4,subdat,interval="prediction"))
str(pred4)
head(pred4)
subdat <- cbind(subdat,pred4)
subdat <- subset( subdat, select = -lwr)
subdat <- subset( subdat, select = -upr)
str(subdat)
head(subdat)
subdat <- rename(subdat, c(fit="fitMLR4"))
subdat$Model4_Residual <- subdat$logSalePrice - subdat$fitMLR4
MAE4 <- mean(abs(subdat$Model4_Residual))
MAE4

#--------------------------------------------
library(car)
vif(MLRresultM4)
par(mfrow=c(1,1))
influencePlot(MLRresultM4,	id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")


summary(inflm.MLRLog <- influence.measures(MLRresultM4))
dffitslog <- dffits(MLRresultM4)
subdat <- cbind(subdat,dffitslog)

subdat[which(abs(dffits(MLRresultM4)) >= 0.1185),]

#133 rows

str(subdat)

ggplot(subdat, aes(x=OverallQual, y=Model4_Residual)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Residual vs OverallQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###

################ influential points removed  #######
subdat$absdf <- abs(subdat$dffitslog)
head(subdat)
#2* sqrt((9+1)/(2860-9-1))
subdatinf <- subdat[which(subdat$absdf < 0.1185),]

#Model 5------------------------------

MLRresultM5 = lm(logSalePrice ~ GrLivArea+TotalBsmtSF+LotArea+GarageArea+OverallQual+Ngrp2+Ngrp3+Ngrp4+Ngrp5, data=subdatinf)
anova(MLRresultM5)
summary(MLRresultM5)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresultM5)


pred5 <- as.data.frame(predict(MLRresultM5,subdatinf,interval="prediction"))
str(pred5)
head(pred5)
subdatinf <- cbind(subdatinf,pred5)
subdatinf <- subset( subdatinf, select = -lwr)
subdatinf <- subset( subdatinf, select = -upr)

subdatinf <- rename(subdatinf, c(fit="fitMLR5"))
subdatinf$Model5_Residual <- subdatinf$logSalePrice - subdatinf$fitMLR5
MAE5 <- mean(abs(subdatinf$Model5_Residual))
MAE5



