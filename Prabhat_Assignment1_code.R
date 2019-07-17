setwd("E:/MS2020/Courses/410/Week1/Assignment1")

##################

mydata <- read.csv(file="ames_housing_data.csv",head=TRUE,sep=",")
sp <- mydata$SalePrice

#######################################
### Section 1  - Sample Definition ###
######################################

# Initial view of data
str(mydata)
head(mydata)
names(mydata)
structure(mydata)

# Get the number of cols and rows
print(length(mydata)) # 82 vars
print(nrow(mydata)) # 2930 observations (properties)

# Summary of Sales Price
summary(mydata$SalePrice)

# Look at the data types
vartypes <- sapply(mydata, class)
vartypes

# Count the Numerics - fix this?
numcnt <- 0
for (i in mydata){
  if (typeof(i) == "integer"){
    numcnt <- numcnt + 1
  }
}
numcnt


# Show Hist of SalePrice
hist(sp)
quantile(sp)

#from example
ggplot(mydata, aes(x=SalePrice)) + 
  geom_histogram(color="black", binwidth= 10000) +
  labs(title="Distribution of Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))


#######################################
### Section 2  - Data Quality Check ###
#######################################

# First verify that all obserations have a reasonable SalePrice value (>0)
summary(mydata$SalePrice)

# Next Define some additional variables
mydata$price_sqft <- mydata$SalePrice/mydata$TotalFloorSF
summary(mydata$price_sqft)
mydata$TotalFloorSF <- mydata$FirstFlrSF + mydata$SecondFlrSF

mydata$TotalFloorSF <- mydata$FirstFlrSF + mydata$SecondFlrSF
mydata$HouseAge <- mydata$YrSold - mydata$YearBuilt
mydata$QualityIndex <- mydata$OverallQual * mydata$OverallCond
mydata$price_sqft <- mydata$SalePrice/mydata$TotalFloorSF



# Next identify some drop conditions
# 1. Non-typical residential zones: drop any Zoning other than Residential (RH, RL, RP, RM)
sub1 <- subset(mydata, mydata$Zoning == "RH" | mydata$Zoning == "RL" | mydata$Zoning == "RP" | mydata$Zoning == "RM")
nrow(sub1)
# 2. Non-normal sale condition: any sale condition other than 'Normal'
sub2 <- subset(sub1, sub1$SaleCondition == "Normal")
nrow(sub2)
# 3. Simplify by looking at only Single Family deteached property types
sub3 <- subset(sub2, sub2$BldgType == "1Fam")
nrow(sub3)

# # 4. Unusual Sale Types: any type other than COD
# mydata_sample <- subset(sub3, sub3$SaleType != "COD")
# nrow(mydata_sample)
# head(mydata_sample)

#Now set sub3 to mydata_sample
mydata_sample <- sub3
length(mydata_sample)
nrow(mydata_sample)

str(mydata_sample)
nrow(mydata_sample) / nrow(mydata)

# Determine the list of twenty variables to test in the EDA, also include SalePrice as the response var
subdat <- subset(mydata_sample, select=c("SalePrice", "TotalFloorSF","GrLivArea","BsmtFinSF1","TotalBsmtSF",
                                  "LotArea","LotShape","LotFrontage","GarageArea","YearBuilt",
                                  "BedroomAbvGr","TotRmsAbvGrd","FullBath","Neighborhood","CentralAir",
                                  "YrSold","KitchenQual","OverallCond","OverallQual","YearRemodel"))

subdat <- subset(mydata_sample, select=c("SalePrice","TotalFloorSF","HouseAge","price_sqft","LotArea","LotShape","FullBath",
                                  "BsmtCond","BsmtFinSF1","TotalBsmtSF","Neighborhood","HouseStyle","YrSold",
                                  "KitchenQual","OverallCond","OverallQual","YearRemodel",
                                  "GarageArea","GarageQual","PoolArea"))


str(subdat)
summary(subdat)


# LotShape, BldgType, CentralAir, KitchenQual are categorical (nominal or ordinal), the rest are numerics

# Break numeric variables into continuous and discrete categories
# Show relationships to each continuous var using scatter plot, and each discrete var using hist
#numeric vars
numdat <- subset(mydata_sample, select=c("SalePrice","BsmtFinSF1","TotalBsmtSF",
                                          "LotArea","LotFrontage","GarageArea","YearBuilt",
                                          "BedroomAbvGr","TotRmsAbvGrd","FullBath",
                                          "YrSold","OverallCond","OverallQual","YearRemodel"))

numdat <- subset(mydata_sample, select=c("SalePrice","TotalFloorSF","HouseAge","price_sqft","LotArea","FullBath",
                                         "BsmtFinSF1","TotalBsmtSF","YrSold","OverallCond","OverallQual","YearRemodel",
                                         "GarageArea","PoolArea"))

#categorical vars
catdat <- subset(mydata_sample, select = c("LotShape","BsmtCond","Neighborhood","HouseStyle","KitchenQual","GarageQual"))

contdat <- subset(mydata_sample, select = c("SalePrice","TotalFloorSF","price_sqft","LotArea","BsmtFinSF1","TotalBsmtSF",
                                            "GarageArea","PoolArea"))
discdat <- subset(mydata_sample, select = c("FullBath","YrSold","HouseAge","OverallCond","OverallQual","YearRemodel"))


summary(discdat)
summary(mydata$TotalFloorSF)

# Test univariate EDA assumptions
# first look at the Factor variables using bar charts
require(ggplot2)
ggplot(subdat, aes(x=mydata_sample$LotShape)) + 
  geom_bar(color="black") + labs(title="Number of houses per Lot Shape") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))    #keep
paste("Reg Shape: ", length(which(mydata_sample$LotShape == "Reg")) / length(mydata_sample$LotShape)) #48% are Reg shape
paste("IR1 Shape: ", length(which(mydata_sample$LotShape == "IR1")) / length(mydata_sample$LotShape)) #27% are Reg shape

paste("Reg Shape: ", length(which(mydata$LotShape == "Reg")) / length(mydata$LotShape)) #48% are Reg shape
paste("IR1 Shape: ", length(which(mydata$LotShape == "IR1")) / length(mydata$LotShape)) #27% are Reg shape


ggplot(subdat, aes(x=mydata_sample$Neighborhood)) + 
  geom_bar(color="black") + labs(title="Number of houses by Neighborhood") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=mydata_sample$BsmtCond)) + 
  geom_bar(color="black") + labs(title="Number of houses per Bldg Type") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))      # can ignore
length(which(mydata_sample$BsmtCond == "TA" | mydata_sample$BsmtCond == "Gd" | 
               mydata_sample$BsmtCond == "Ex")) / length(mydata_sample$BsmtCond) #76% have rating TA or better
length(which(mydata$BsmtCond == "TA" | mydata$BsmtCond == "Gd" | 
               mydata$BsmtCond == "Ex")) / length(mydata$BsmtCond) #76% have rating TA or better


#length(which(mydata_sample$BldgType == "1Fam")) / length(mydata_sample$BldgType) #65% are Single Fam homes

ggplot(subdat, aes(x=mydata_sample$HouseStyle)) + 
  geom_bar(color="black") + labs(title="Number of houses per House Style Type") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))  #keep

#length(mydata_sample[mydata_sample$CentralAir == "NA"])
#length(which(mydata_sample$CentralAir == "Y")) / length(mydata_sample$CentralAir) #73% have central air

ggplot(subdat, aes(x=mydata_sample$KitchenQual)) + 
  geom_bar(color="black") + labs(title="Number of houses per Kitchen Qual Type") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
length(which(mydata_sample$KitchenQual == "TA" | mydata_sample$KitchenQual == "Gd" | 
               mydata_sample$KitchenQual == "Ex")) / length(mydata_sample$KitchenQual) #76% have rating TA or better

ggplot(subdat, aes(x=mydata_sample$GarageQual)) + 
  geom_bar(color="black") + labs(title="Number of houses per GarageQual Type") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

length(which(mydata_sample$KitchenQual == "TA" | mydata_sample$KitchenQual == "Gd" | 
               mydata_sample$KitchenQual == "Ex")) / length(mydata_sample$KitchenQual) #76% have rating TA or better


# numeric univariate tests
ggplot(numdat, aes(x=SalePrice)) + 
  geom_histogram(color="black", binwidth= 10000) +
  labs(title="Distribution of SalePrice") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(numdat, aes(x=TotalFloorSF)) + 
  geom_histogram(color="black", binwidth= 200) +
  labs(title="Distribution of TotalFloorSF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))




length(which(mydata_sample$TotalFloorSF > 3000)) / length(mydata_sample$TotalFloorSF) 
length(which(mydata_sample$SalePrice > 600000)) / length(mydata_sample$TotalFloorSF) 




# length(numdat)
# for (n in numdat){
#   print(paste("name", quantile(n, na.rm = TRUE)))
# }

# Test bivariate EDA assumptions
ggplot(subdat, aes(x=TotalFloorSF, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of SalePrice by Total Floor SF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=price_sqft, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of SalePrice by price_sqft") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))


ggplot(subdat, aes(x=LotArea, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of SalePrice vs Lot Area") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=LotShape, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Boxplot for SalePrice by Lot Shape") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=KitchenQual, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Boxplot for SalePrice by KitchenQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=BedroomAbvGr, y=YearBuilt)) + 
  geom_boxplot(fill="blue") +
  labs(title="Boxplot for Year Built by  Bedrooms Abv Gr") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=LotFrontage, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of LotFrontage vs SalePrice") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=TotRmsAbvGrd, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Total Rooms Abv Grd vs SalePrice") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=GrLivArea, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of GrLivArea vs SalePrice") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#General tests
ggplot(subdat, aes(x=Neighborhood, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("SalePrice by Neighborhood") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=HouseStyle, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("SalePrice by HouseStyle") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))


# Identify Relevant Variables
#Focus on Contiuous Variables for now
cor(mydata$SalePrice, mydata$TotalFloorSF, use = "everything", method = c("pearson"))
    
for (i in contdat){
  print(cor(contdat$SalePrice, i, method = c("pearson")))
}


#######################################
### Section 3  - Inital EDA Tests  ###
######################################
# Choose ten variables to do more analysis
#del TotalFloorSF, KitchenQual, CentralAir
init_eda <- subset(mydata_sample, select=c("SalePrice", "GrLivArea","TotalBsmtSF","GarageArea","YearBuilt",
                                            "TotRmsAbvGrd","FullBath","YrSold","OverallQual", "Neighborhood"))

init_eda <- subset(mydata_sample, select=c("SalePrice","TotalFloorSF","price_sqft","LotArea","LotShape","TotalBsmtSF",
                                           "Neighborhood","KitchenQual","OverallQual","GarageArea"))

str(init_eda)
names(init_eda)

# THESE ARE MY TEN VARS FOR INIT EDA
num_eda <- subset(mydata_sample, select=c("SalePrice", "GrLivArea","TotalBsmtSF","GarageArea","YearBuilt",
                                           "TotRmsAbvGrd","FullBath","YrSold","OverallQual"))
str(num_eda)

#Get quantile summary for each of the selected vars
for (n in num_eda){
  print(summary(n))
}

# #Get descriptives for each of the selected vars
sapply(num_eda, min)
sapply(num_eda, max)
round(sapply(num_eda, median), 0)
round(sapply(num_eda, mean), 0)
# round(sapply(num_eda, sd), 2)

# Check Histogram for cleaned data
hist(mydata_sample$SalePrice, main = "Distribution of SalePrice - Cleaned", 
     xlab = "Sale Price", ylab = "Count", col = "Blue")

# Correlation
for (n in numdat){
  print(cor(mydata_sample$SalePrice, n, method = "pearson"))
}

#Check anova?
fit <- lm(mydata_sample$SalePrice ~ mydata_sample$GrLivArea)
summary(aov(fit))


# Model focused EDA for SalePrice as the response

ggplot(mydata, aes(x=GrLivArea, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs GrLivArea - original data") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) #shows pos cor

ggplot(numdat, aes(x=GrLivArea, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs GrLivArea- cleaned data") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) #shows pos cor

ggplot(numdat, aes(x=OverallQual, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs Qual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))


ggplot(numdat, aes(x=GarageArea, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs GarageArea") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(numdat, aes(x=YearBuilt, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs Year Built") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#compare saleprice to cat var KitchenQual
ggplot(subdat, aes(x=KitchenQual, y=SalePrice)) + 
  geom_boxplot(fill="blue") + labs("Scatter Plot of Sale Price vs KithcenQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#compare saleprice to cat var CentralAir
ggplot(subdat, aes(x=CentralAir, y=SalePrice)) + 
  geom_boxplot(fill="blue") + labs("Scatter Plot of Sale Price vs CentralAir") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))


# Extra, test correlations for the ten vars
require(corrplot)
mcor <- cor(num_eda)
corrplot(mcor, method = "shade", shade.col = NA, tl.col = "black", tl.cex = 0.5)


########################################
### Section 4 - EDA 
########################################
#Now choose the three vars you want to use as predictors for SalePrice and log(SalePrice)
eda_vars <- subset(mydata_sample, select=c("SalePrice", "GrLivArea","TotalBsmtSF","OverallQual"))
eda_vars$logSalePrice <- log(mydata_sample$SalePrice)
str(eda_vars)


# First show plots using SalePrice
ggplot(mydata_sample, aes(x=GrLivArea, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs GrLivArea - EDA") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(mydata_sample, aes(x=TotalBsmtSF, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs TotalBsmtSF - EDA") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(mydata_sample, aes(x=OverallQual, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs OverallQual - EDA") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

# Second show plots using log(SalePrice)
ggplot(mydata_sample, aes(x=GrLivArea, y=log(SalePrice))) + 
  geom_point(color="red", size=2) +
  ggtitle("Scatter Plot of Log Sale Price vs GrLivArea - EDA") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(mydata_sample, aes(x=TotalBsmtSF, y=log(SalePrice))) + 
  geom_point(color="red", size=2) +
  ggtitle("Scatter Plot of Log Sale Price vs TotalBsmtSF - EDA") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(mydata_sample, aes(x=OverallQual, y=log(SalePrice))) + 
  geom_point(color="red", size=2) +
  ggtitle("Scatter Plot of Log Sale Price vs OverallQual - EDA") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

# Also test pair-wise plots
require(lattice)
pairs(eda_vars)

require(GGally)
ggpairs(eda_vars)

require(corrplot)
mcor <- cor(eda_vars)
corrplot(mcor, method = "shade", shade.col = NA, tl.col = "black", tl.cex = 0.5)

mfrow=c(1, 2)
p1 <- ggplot(mydata_sample, aes(x=TotalBsmtSF, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs TotalBsmtSF - EDA") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

p2 <- ggplot(mydata_sample, aes(x=OverallQual, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs OverallQual - EDA") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

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


multiplot(p1, p2, cols=2)

init_eda <- subset(mydata_sample, select=c("SalePrice","TotalFloorSF","price_sqft","LotArea","LotShape","TotalBsmtSF",
                                           "Neighborhood","KitchenQual","OverallQual","GarageArea"))

p1 <- ggplot(init_eda, aes(x=TotalFloorSF)) + 
  geom_histogram(color="black", binwidth= 100) +
  labs(title="Distribution of TotalFloorSF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

p2<- ggplot(init_eda, aes(x=TotalFloorSF, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of SalePrice by Total Floor SF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

multiplot(p1, p2, cols=2)
#--------------------------------

p1 <- ggplot(init_eda, aes(x=LotArea)) + 
  geom_histogram(color="black", binwidth= 1000) +
  labs(title="Distribution of LotArea") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

p2<- ggplot(init_eda, aes(x=LotArea, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of SalePrice by LotArea") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

multiplot(p1, p2, cols=2)
#---------------------------------------------
  
p1 <- ggplot(init_eda, aes(x=OverallQual)) + 
  geom_bar(color="black") + labs(title="Number of houses per Kitchen Qual Type") +
  labs(title="Distribution of OverallQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

p2<- ggplot(init_eda, aes(x=OverallQual, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of SalePrice by OverallQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

multiplot(p1, p2, cols=2)
#---------------------------------------------


p1 <- ggplot(init_eda, aes(x=Neighborhood)) + 
  geom_bar(color="black") + labs(title="Number of houses per Neighborhood") +
  labs(title="Distribution of Neighborhood") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

p2<- ggplot(init_eda, aes(x=Neighborhood, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("SalePrice by Neighborhood") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))


multiplot(p1, p2, cols=2)
#---------------------------------------------

p1 <- ggplot(init_eda, aes(x=mydata_sample$KitchenQual)) + 
  geom_bar(color="black") + labs(title="Number of houses per Kitchen Qual Type") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))


p2<- ggplot(init_eda, aes(x=KitchenQual, y=SalePrice)) + 
  geom_boxplot(fill="blue") + labs("Scatter Plot of Sale Price vs KithcenQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))


multiplot(p1, p2, cols=2)
#---------------------------------------------

init_eda <- subset(mydata_sample, select=c("SalePrice","TotalFloorSF","price_sqft","LotArea","LotShape","TotalBsmtSF",
                                           "Neighborhood","KitchenQual","OverallQual","GarageArea"))

str(init_eda)
init_eda_num <- subset(mydata_sample, select=c("SalePrice","TotalFloorSF","price_sqft","LotArea",
                                               "TotalBsmtSF","OverallQual","GarageArea"))
mcor <- cor(init_eda_num)
corrplot(mcor, method = "shade", shade.col = NA, tl.col = "black", tl.cex = 0.5)


init_eda_num <- 

# First show plots using SalePrice
  p1 <- ggplot(mydata_sample, aes(x=TotalFloorSF, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs TotalFloorSF - EDA") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))


p2 <- ggplot(mydata_sample, aes(x=TotalFloorSF, y=log(SalePrice))) + 
  geom_point(color="red", size=2) +
  ggtitle("Scatter Plot of Log Sale Price vs TotalFloorSF - EDA") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
multiplot(p1, p2, cols=2)
#--------------------


p1 <- ggplot(mydata_sample, aes(x=TotalBsmtSF, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs TotalBsmtSF - EDA") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

p2 <- ggplot(mydata_sample, aes(x=TotalBsmtSF, y=log(SalePrice))) + 
  geom_point(color="red", size=2) +
  ggtitle("Scatter Plot of Log Sale Price vs TotalBsmtSF - EDA") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
multiplot(p1, p2, cols=2)
#--------------------


p1 <- ggplot(mydata_sample, aes(x=OverallQual, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs OverallQual - EDA") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

p2 <-  ggplot(mydata_sample, aes(x=OverallQual, y=log(SalePrice))) + 
  geom_point(color="red", size=2) +
  ggtitle("Scatter Plot of Log Sale Price vs OverallQual - EDA") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
multiplot(p1, p2, cols=2)
#--------------------

# Second show plots using log(SalePrice)

eda_vars <- subset(mydata_sample, select=c("SalePrice", "TotalFloorSF","TotalBsmtSF","OverallQual"))
eda_vars$logSalePrice <- log(mydata_sample$SalePrice)
str(eda_vars)

require(lattice)
pairs(eda_vars)

require(GGally)
ggpairs(eda_vars)

require(corrplot)
mcor <- cor(eda_vars)
corrplot(mcor, method = "shade", shade.col = NA, tl.col = "black", tl.cex = 0.5)


