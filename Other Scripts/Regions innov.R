region<-read.csv('C:/Users/nasty/Downloads/data_regions_2020.csv',header=TRUE,sep=';')
pairs(region[2:11])
cor(region[2:11])
library('corrplot') #package corrplot
corrplot(cor(region[2:11]), method = "circle") #plot matrix

# delete x2,x3,x5,x7,x9
region_1<-region[,-c(3,4,6,8,10)]
corrplot(cor(region_1[2:6]), method = "circle") #plot matrix

#model
region.X<-as.matrix(cbind(log(region_1$x1),log(region_1$x10),region_1[3:5]))
region.Y<-as.matrix(log(region$x7))
model<-lm(region.Y ~ region.X)
summary(model)

#normalize
average <- apply(region_1[2:6],2,mean)
sd <- apply(region_1[2:6],2,sd)
nor<-data.frame(ncol=5,nrow=nrow(region_1))
for (i in 1:nrow(region_1)) {
  for (j in 1:(ncol(region_1)-1)) {
    nor[i,j]<-(region_1[i,1+j]-average[j])/sd[j]
  }}

colnames(nor)<-colnames(region_1[2:6])

#clusters
distance = dist(nor)

nor.hclust = hclust(distance)
plot(nor.hclust)

library(factoextra) 
k2 <- kmeans(nor, centers = 2, nstart = 25)
fviz_cluster(k2, data = nor)