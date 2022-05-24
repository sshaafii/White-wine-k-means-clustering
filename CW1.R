
library(factoextra)
library("readxl")
library("NbClust")
library(fpc)

Whitewine_v2 <- read_excel("Whitewine_v2.xlsx")
WineData<-data.frame(Whitewine_v2)

dim(WineData)

sapply(WineData,class)

head(WineData)

summary(WineData)


sum(is.na(WineData))
#detect outlier function
detect_outlier<- function(x){
  Q1<- quantile(x,probs=.25)
  Q3<- quantile(x,probs=.75)
  
  IQR = Q3-Q1
  
  x > Q3 + (IQR*3.7) | x < Q3 - (IQR*3.7)
}

#create remove outlier function 
remove_outlier<- function(dataframe,columns = names(dataframe)){
  for (col in columns) {
    dataframe <- dataframe[!detect_outlier(dataframe[[col]]),]
  }
  
 
  dataframe
  
  
  
}


noOutliers<-data.frame(remove_outlier(WineData,c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","alcohol","quality" )))
noOutliers_scaled<-data.frame(scale(noOutliers[-12]))

set.seed(1234)
clusterNo=NbClust(noOutliers_scaled,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")

set.seed(1234)
fviz_nbclust(noOutliers_scaled, kmeans, method = 'wss')


set.seed(1234)
fviz_nbclust(noOutliers_scaled, kmeans, method = 'silhouette')


#Kmeans for 2 clusters
set.seed(1234)
fit.km2 = kmeans(noOutliers_scaled, centers = 2, nstart = 10)

(fit.km2$betweenss/fit.km2$totss)*100

plotcluster(noOutliers_scaled,fit.km2$cluster)

confuse<- table(noOutliers$quality,fit.km2$cluster)
confuse

accuracy.k2<-(832+1295+715+147)/(564+832+1295+844+715+161+147+26)
accuracy.k2

precision.km2.c1<-(1295+715+147)/(1295+715+147+564)
precision.km2.c1
precision.km2.c2<-832/(832+844+161+26)
precision.km2.c2

recall.km2.r1<-(832)/(832+564)
recall.km2.r1
recall.km2.r2<-1295/(1295+844)
recall.km2.r2
recall.km2.r3<-715/(715+161)
recall.km2.r3
recall.km2.r4<-147/(147+26)
recall.km2.r4

#Kmeans for 3 clusters
set.seed(1234)
fit.km3 = kmeans(noOutliers_scaled, centers = 3, nstart = 10)

(fit.km3$betweenss/fit.km3$totss)*100

plotcluster(noOutliers_scaled,fit.km3$cluster)

confuse1<- table(noOutliers$quality,fit.km3$cluster)
confuse1

accuracy.k3<-(776+774+412+62)/(333+776+287+663+774+702+321+143+412+62+24+87)
accuracy.k3

precision.km3.c1<-62/(321+663+333)
precision.km3.c1
precision.km3.c2<-(776+774)/(776+774+143+24)
precision.km3.c2
precision.km3.c3<-412/(287+702+412+87)
precision.km3.c3

recall.km3.r1<-776/(333+776+287)
recall.km3.r1
recall.km3.r2<-774/(663+774+702)
recall.km3.r2
recall.km3.r3<-412/(321+ 143+ 412)
recall.km3.r3
recall.km3.r4<-62/(62+24+87)
recall.km3.r4


#Kmeans for 4 clusters
set.seed(1234)
fit.km4 = kmeans(noOutliers_scaled, centers = 4, nstart = 10)

(fit.km4$betweenss/fit.km4$totss)*100

plotcluster(noOutliers_scaled,fit.km4$cluster)

confuse2<- table(noOutliers$quality,fit.km4$cluster)
confuse2
accuracy.km4<-(671+583+385+38)/(319+87+319+671+583+416+502+638+202+385+174+115+30+84+38+21)
accuracy.km4

precision.km4.c1<-671/(671+638+115+21)
precision.km4.c1
precision.km4.c2<-583/(319+583+202+30)
precision.km4.c2
precision.km4.c3<-385/(87+416+385+84)
precision.km4.c3
precision.km4.c4<-671/(671+638+115+21)
precision.km4.c4

recall.km4.r1<-671/(671+319+87+319)
recall.km4.r1
recall.km4.r2<-583/(583+416+502+628)
recall.km4.r2
recall.km4.r3<-385/(202+385+174+115)
recall.km4.r3
recall.km4.r4<-38/(30+84+38+21)
recall.km4.r4

# 2 clusters is the best as it has highest accuracy.

pca_wine=prcomp(noOutliers[-12],center = TRUE,scale=TRUE)
summary(pca_wine)

#PC1 TO PC9 explain 96%of the dataset
 wine_transform=as.data.frame(-pca_wine$x[,1:9])
head(wine_transform)

fviz_nbclust(wine_transform, kmeans, method = 'silhouette')

fviz_nbclust(wine_transform, kmeans, method = 'gap_stat')

set.seed(1234)
pca.km2=kmeans(wine_transform,centers = 2,nstart = 10)
pca.km2
plotcluster(wine_transform,pca.km2$cluster)
