# Austin Burgess, Dallas Griffiths
# Lab 3 -- Hieroglyphics/k-means clustering
# DA 350
# Dr. Bonifonte
# Due 02/13/2020

#setwd("~/DA350/Hieroglyphics_Images")
#files = list.files()
library(png)
#output <- lapply(files, function(x) readPNG(x))
#df <- data.frame(do.call(rbind, lapply(output, function(x)
#  t(matrix(x)))))

#write.csv(file="df", x=df)

setwd("C:/Users/Austin Burgess/Downloads")
df <- read.csv("df.csv")
hieroPCA <- prcomp(df, scale.=TRUE)

#Plot variance explained
pca_var = hieroPCA$sdev^2
prop_varex <- pca_var/sum(pca_var) #Proportion of variance

plot(cumsum(prop_varex),type='b') #Cumulative proportion of variance

# Using 550 components (Eyeballed it)

set.seed(1) #For consistency
#Plot within-cluster distances as a function of cluster size
wss = list(51)
kFunc <- function(k){ 
  wss[k] = sum(kmeans(hieroPCA$x[,1:550], k, nstart=10)$tot.withinss)
}
wss <- sapply(200:250, kFunc)

library(tibble)
wssDf <- enframe(wss, name = "NumClusters", value = "sumSquares")

#plot(200:250, wss[200:250], type="b", xlab="Number of Clusters",
#     ylab="Within groups sum of squares")

library(ggplot2)
library(dplyr)

wssDf %>%
  ggplot(aes(x = NumClusters, y = sumSquares)) +
  geom_line() +
  labs(x = "Number of Clusters", y = "Within groups sum of squares")

# As k increases, the total within sum of squares decreases, which is to be expected as more clusters are added.

# choosing k = 250

wss2 = list(1000)
randomStarts <- function(n){
  wss2[n] = sum(kmeans(hieroPCA$x[,1:550], 250, nstart=1)$tot.withinss)
}
wss2 = sapply(1:1000, randomStarts)

hist(wss2)

# This looks fairly normally-distributed honestly. There's a spike around 5100000-5120000, which is where
# the within sum of squares most frequently resided, but it tails out in both ends, very similar to a normal-like
# distribution. The effect that the number of random starts has on this result is shown by the variance in this
# histogram, which there is quite a bit of it -- the shown range on the x-axis is of size 100000. That's quite a large
# difference in sum of squares, and that difference has the possibility of being experienced just by running two different
# k-means clusters each with nstart = 1. That displays the importance of having a sufficiently high enough number
# of random starts for your model.

wss3 = sum(kmeans(hieroPCA$x[,1:550], 250, nstart=1000)$tot.withinss)

finalModel = kmeans(hieroPCA$x[,1:550], 250, nstart=1000)

finalSummary = data.frame(n=finalModel$size, finalModel$centers)

makeFolders <- function(k){
  dir.create(paste("Cluster",k, sep=""))
}
setwd("C:/Users/Austin Burgess/Documents/Lab3Clusters")
sapply(1:250,makeFolders)

groupPics <- function(n){
  vec <- as.numeric(df[n,2:3751])
  dim(vec) <- c(75,50)
  
  setwd(paste("C:/Users/Austin Burgess/Documents/Lab3Clusters/Cluster",finalModel$cluster[n],sep = ""))
  writePNG(vec,target = paste("img_",finalModel$cluster[n],"_",n,".png",sep = ""))
  
}

sapply(1:4410, groupPics)

library(clue)
setwd("C:/Users/Austin Burgess/Documents/Lab3Predictions/Predictions")

files = list.files()
output <- lapply(files, function(x) readPNG(x))
df2 <- data.frame(do.call(rbind, lapply(output, function(x)
  t(matrix(x)))))

cl_predict(finalModel, newdata = df2)