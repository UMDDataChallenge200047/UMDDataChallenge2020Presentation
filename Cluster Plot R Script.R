# 1. Load cluster package
install.packages("cluster")
install.packages("ggplot2")
install.packages("GGally")

library("cluster")
library("ggplot2")
library(GGally)


#Read the data file.  Change the file location.
setwd("/Users/dacruznorberto/Google Drive/UMD MBA Program/Data Challenge 2020/")
#reads credit approval csv file into R
mydata<-read.csv(file="/Users/dacruznorberto/Google Drive/UMD MBA Program/Data Challenge 2020/Data_Level2_HUD_HUDPrograms_Fulldataset.csv", head=TRUE, sep=",")
mydata <- na.omit(mydata) # listwise deletion of missing
View(mydata)
#remove the variety.of.wheat
mydata2<- mydata
mydata2$pgm_type_edited<-NULL
write.csv(mydata2, file = "Wine Quality (Not Scaled).csv")
head(mydata2)
scale(mydata2, center = TRUE, scale = TRUE)
write.csv(mydata2, file = "Wine Quality (Scaled).csv")
# 4. Run the method
#make sure that the result is reproducible
set.seed(1234)
#---------------
#Cluster Selection
ss <- rep(0,20)
for (i in 1:20) {
  set.seed(20)
  ss[i] <- sum(kmeans(mydata2,centers=i,iter.max = 30, nstart = 5)$withinss)
}

ss2 <- data.frame(cbind(ss,seq(1,20)))
colnames(ss2) <- c("SS","Cluster")

ggplot(data = ss2,aes(x = Cluster, y=SS)) +
  geom_point(color="blue") + geom_line()

#Run the method and store the result in kc variable
kc<-kmeans(mydata2, 3)

#output the result
print(kc)
kc$iter
kc$betweenss
kc$totss

# 5. cluster to class evaluation
table(mydata$pgm_type_edited, kc$cluster)
hist(kc$cluster, col = rainbow(kc$cluster))
plot(kc$centers)
# 6. cluster plot
clusplot(mydata, kc$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

