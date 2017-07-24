#define the location path of the image datasets
imageLocation = "C:/microsoft R/demo/ORL_DATABASE/ORL92112/bmp"
dirs=paste("s",1:40,sep = "")

# construct a dataset to find the suitable face image similarity threshold
data = data.frame(Image1 = character(),Image2 = character(),
                  same = character(),stringsAsFactors = FALSE)
for(dirNum in 1:(length(dirs) - 1)){ # for every dir
  for(n in 1:9){ # for every face image in the dir
    for(m in (n+1):10){#construct the item that the same person
      i1 = paste(dirs[dirNum],paste(n,".bmp",sep = ""),sep = "/")
      i2 = paste(dirs[dirNum],paste(m,".bmp",sep = ""),sep = "/")
      item = c(i1,i2,"same")
      data = rbind(data,item,stringsAsFactors = FALSE)
    }
    
    for(k in (dirNum + 1):length(dirs)){
      for(i in 1:10){
        i1 = paste(dirs[dirNum],paste(n,".bmp",sep = ""),sep = "/")
        i2 = paste(dirs[k],paste(i,".bmp",sep = ""),sep = "/")
        item = c(i1,i2,"diff")
        data = rbind(data,item,stringsAsFactors = FALSE)
      }
    }
  }
}


colnames(data) = c("image1","image2","isSame")
#

images = c()
# using for loops to fill the image paths
for(dir in dirs){
  for(n in 1:10){
    path = paste(dir,paste(n,"bmp",sep = "."),sep = "/")
    images= append(images,file.path(imageLocation,path),after = length(images))
  }
}

imagesDF <- data.frame(Image = images, stringsAsFactors = FALSE)

imageFeatureVectorDF <- rxFeaturize(
  data = imagesDF,
  mlTransforms = list(
    loadImage(vars = list(Features = "Image")),
    resizeImage(vars = "Features", width = 224, height = 224),
    extractPixels(vars = "Features"),
    featurizeImage(var = "Features", dnnModel = "resnet18")   
  ))

distances = c()

for(i in 1:length(rownames(data))){
  image1 = file.path(imageLocation,data[i,1])
  image2 = file.path(imageLocation,data[i,2])
  feature1 = imageFeatureVectorDF[which(imageFeatureVectorDF$Image == image1),]
  feature2 = imageFeatureVectorDF[which(imageFeatureVectorDF$Image == image2),]
  distVals <- dist(rbind(feature1, feature2)[,-1], "euclidean")
  distances = append(distances,as.matrix(distVals)[1,2],after = length(distances))
  
}

data$distance = distances

innerDistance = data$distance[which(data$sameLabel == 0)]
interDistance = data$distance[which(data$sameLabel == 1)]

data$sameLabel[data$isSame == "same"] = 0
data$sameLabel[data$isSame == "diff"] = 1
histogram(innerDistance,breaks = 50,col = "green",xlim = c(3,30))
histogram(interDistance,breaks = 50,col = "red",xlim = c(3,30))

threshold = 11.8
data$predictSame[data$distance > threshold] = 1
data$predictSame[data$distance <= threshold] = 0

inner_accuracy = sum(data$sameLabel == 0 & data$predictSame == 0)/sum(data$sameLabel == 0)
inter_accuracy = sum(data$sameLabel == 1 & data$predictSame == 1)/sum(data$sameLabel == 1)
print(paste("inner_accuracy:",inner_accuracy,sep = ""))
print(paste("inter_accuracy:",inter_accuracy,sep = ""))






