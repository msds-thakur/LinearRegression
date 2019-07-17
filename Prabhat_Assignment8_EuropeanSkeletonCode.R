
################################################################

my.data <- read.csv(file="EuropeanEmployment.csv",head=TRUE,sep=",")
str(my.data)
head(my.data)

##################################################################

require(cluster)
require(useful)
require(Hmisc)
library(HSAUR)
library(MVA)
library(HSAUR2)
library(fpc)
library(mclust)
library(lattice)
library(car)
require(ggplot2)

# EDA to do scatterplots - Task 2 and 3

ggplot(my.data, aes(x=SER, y=FIN, colour = Group, label= Country)) + 
  geom_point() + geom_text(aes(label=Country),hjust=0, vjust=0) +
  ggtitle("Scatter Plot Financial vs Services") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(my.data, aes(x=MAN, y=FIN, colour = Group, label= Country)) + 
  geom_point() + geom_text(aes(label=Country),hjust=0, vjust=0) +
  ggtitle("Scatter Plot Financial vs Manufacturing") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

# Pairwise scatterplot
pairs(my.data[,-c(1,2)])

# Do PCA to reduce the dimension from 9 to 2 - Task 4

apply(my.data[,-c(1,2)],MARGIN=1,FUN=sum)
pca.out <- princomp(x=my.data[,-c(1,2)],cor=FALSE);
names(pca.out)

pc.1 <- pca.out$scores[,1];
pc.2 <- pca.out$scores[,2];
str(pc.1)
pcdf = data.frame(pc1=pc.1, pc2=pc.2)
pcdf1 = cbind(pcdf,my.data$Country)
pcdf2 = cbind(pcdf1,my.data$Group)
str(pcdf2)

ggplot(pcdf2, aes(x=pc1, y=pc2, colour = my.data$Group, label= my.data$Country)) + 
  geom_point() + geom_text(aes(label=my.data$Country),hjust=0, vjust=0) +
  ggtitle("Scatter Plot PC1 vs PC2") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

# Hirerarchical clustreing - Task 5

hier.dist = dist(my.data[,-c(1,2)])
require(maptree)
hclustmodel <- hclust(hier.dist, method = 'complete')
plot(hclustmodel,labels=my.data$Country)

pcdf2[,c(1,2)]

# choose the number of clusters k = 3
cut.3 <- cutree(hclustmodel, k=3)
head(cut.3)
cut.3
pcdf3 <- cbind(pcdf2,cut.3)
pcdf3

# now choose k=6
cut.6 <- cutree(hclustmodel, k=6)
head(cut.6)
cut.6
pcdf6 <- cbind(pcdf2,cut.6)
pcdf6

# cross tab of clusters vs Group

table(pcdf3$'my.data$Group',pcdf3$cut.3) 
table(pcdf6$'my.data$Group',pcdf6$cut.6)


# accuracy - Between % ss for K=3
subdat <- my.data[,-c(1,2)]
TSS <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
TSS
require(fpc)
complete3 <- cutree(hclust(hier.dist),3)
WSS <- cluster.stats(hier.dist,complete3, alt.clustering=NULL)$within.cluster.ss
WSS
BetSSPer <- (TSS-WSS)/TSS
BetSSPer

# accuracy - Between % ss for K=6
complete6 <- cutree(hclust(hier.dist),6)
WSS <- cluster.stats(hier.dist,complete6, alt.clustering=NULL)$within.cluster.ss
WSS
BetSSPer <- (TSS-WSS)/TSS
BetSSPer


# PCA Hirerarchical clustreing - Task 5

PCAhier.dist = dist(pcdf2[,c(1,2)])
PCAhclustmodel <- hclust(PCAhier.dist, method = 'complete')
plot(PCAhclustmodel,labels=pcdf2$`my.data$Country`)


# choose the number of clusters k = 3
cut.3 <- cutree(PCAhclustmodel, k=3)
PCApcdf3 <- cbind(pcdf2,cut.3)
PCApcdf3

# now choose k=6
cut.6 <- cutree(PCAhclustmodel, k=6)
PCApcdf6 <- cbind(pcdf2,cut.6)
PCApcdf6

# cross tab of clusters vs Group

table(PCApcdf3$'my.data$Group',PCApcdf3$cut.3) 
table(PCApcdf6$'my.data$Group',PCApcdf6$cut.6)


# accuracy - Between % ss for K=3
PCAsubdat <- pcdf2[,c(1,2)]
PCATSS <- (nrow(PCAsubdat)-1)*sum(apply(PCAsubdat,2,var))
PCATSS

PCAcomplete3 <- cutree(hclust(PCAhier.dist),3)
PCAWSS <- cluster.stats(PCAhier.dist,PCAcomplete3, alt.clustering=NULL)$within.cluster.ss
PCAWSS
PCABetSSPer <- (PCATSS-PCAWSS)/PCATSS
PCABetSSPer

# accuracy - Between % ss for K=6
PCAcomplete6 <- cutree(hclust(PCAhier.dist),6)
PCAWSS <- cluster.stats(PCAhier.dist,PCAcomplete6, alt.clustering=NULL)$within.cluster.ss
PCAWSS
PCABetSSPer <- (PCATSS-PCAWSS)/PCATSS
PCABetSSPer


# kmeans clustering with k=3 clusters - Task 6

clusterresults <- kmeans(my.data[,-c(1,2)],3)
names(clusterresults)
BetSSPer <- clusterresults$betweenss/clusterresults$totss
BetSSPer
clusterresults$totss

p1 <- plot(clusterresults, data=my.data[,-c(1,2)],title ='K-Means Results, K=3')



# cluster plots for kmeans

library(cluster) 
 clusplot(my.data[,-c(1,2)], clusterresults$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# kmeans clustering with k=6 clusters - Task 6

clusterresults6 <- kmeans(my.data[,-c(1,2)],6)
names(clusterresults6)
BetSSPer6 <- clusterresults6$betweenss/clusterresults6$totss
BetSSPer6
clusterresults6$totss

p2 <- plot(clusterresults6, data=my.data[,-c(1,2)],title ='K-Means Results, K=6')


multiplot(p1, p2, cols=2)

#par(mfrow=c(1,1))


# cluster plots for kmeans

 clusplot(my.data[,-c(1,2)], clusterresults6$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

 
 # PCA
 PCAclusterresults <- kmeans(pcdf2[,c(1,2)],3)
 PCABetSSPer <- PCAclusterresults$betweenss/PCAclusterresults$totss
 PCABetSSPer

 p3 <- plot(PCAclusterresults, data=pcdf2[,c(1,2)],title ='PCA K-Means Results, K=3')
 
 
 PCAclusterresults6 <- kmeans(pcdf2[,c(1,2)],6)
 PCABetSSPer6 <- PCAclusterresults6$betweenss/PCAclusterresults6$totss
 PCABetSSPer6
 
 p4 <-  plot(PCAclusterresults6, data=pcdf2[,c(1,2)],title ='PCA K-Means Results, K=6')

 multiplot(p3, p4, cols=2) 

# Internal validation - Task 7

## K means clustering

wssplot <- function(subdat, nc=15, seed=1234) {
  wss <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(subdat, centers=i)$withinss)}
    rs <- (wss[1] - wss)/wss[1]
     plot(1:nc, wss, type="b", xlab="Number of Clusters",
         ylab="Within groups sum of squares")
     plot(1:nc, rs, type="b", xlab="Number of Clusters",
       ylab="% of Between SS")} 

wssplot(subdat)


## Hierarchical clustering

wssplot <- function(subdat, nc=15, seed=1234) {
  wss <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
  for (i in 2:nc) {
    require(fpc)
    set.seed(seed)
    hier.dist <- dist(subdat)
    complete3 <- cutree(hclust(hier.dist),i)
  wss[i] <- cluster.stats(hier.dist,complete3, alt.clustering=NULL)$within.cluster.ss}
  rs <- (wss[1] - wss)/wss[1]
     plot(1:nc, wss, type="b", xlab="Number of Clusters - Hierarchical clustering",
       ylab="Within groups sum of squares")
     plot(1:nc, rs, type="b", xlab="Number of Clusters - Hierarchical clustering ",
         ylab="% of Between SS")

    return(wss)}

wssplot(subdat)



#---------------------------------
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

