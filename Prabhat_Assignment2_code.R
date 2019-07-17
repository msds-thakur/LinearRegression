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
subdat <- subset(mydata, select=c("TotalFloorSF","HouseAge","QualityIndex",'Zoning',
                                  "price_sqft", "SalePrice","LotArea",
                                  "BsmtFinSF1","Neighborhood","HouseStyle",
                                  "LotShape","OverallQual","logSalePrice",
                                  "TotalBsmtSF","BldgType",'GrLivArea','SaleCondition'))

str(subdat)


subdatnum <- subset(mydata, select=c("TotalFloorSF","HouseAge","QualityIndex",
                                     "SalePrice","LotArea","OverallQual","logSalePrice"))

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
