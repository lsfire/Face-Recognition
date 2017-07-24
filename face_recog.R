#define the location path of the image datasets
imageLocation = "C:/microsoft R/demo/ORL_DATABASE/ORL92112/bmp"
dirs=paste("s",1:40,sep = "")
images = c()
# using for loops to fill the image paths
for(dir in dirs){
  for(n in 1:9){
    path = paste(dir,paste(n,"bmp",sep = "."),sep = "/")
    images= append(images,file.path(imageLocation,path),after = length(images))
  }
}

imagesDF <- data.frame(Image = images, stringsAsFactors = FALSE)
typelist = c()
for(t in 1:40){
  for(m in 1:9){
    typelist = append(typelist,paste("s",t,sep = ""),after = length(typelist))
  }
}
imagesDF$type = typelist

for( a in 1:40){
  imagesDF$Label[imagesDF$type == paste("s",a,sep = "")] = a - 1 
}
imageModel <- rxLogisticRegression(
  formula = Label~Features,
  data = imagesDF,
  type = "multiClass",
  mlTransforms = list(
    loadImage(vars = list(Features = "Image")),
    resizeImage(vars = "Features", width = 227, height = 227),
    extractPixels(vars = "Features"),
    featurizeImage(var = "Features", dnnModel = "alexnet")) 
)

imageFile <- c()

for(i in 1:40){
  imageFile = append(imageFile,file.path(imageLocation,paste(paste("s",i,sep = ""),
                                                             "/10.bmp",sep = "")),
                                                            after = length(imageFile))
}
# Convert to a dataframe 
imageToMatch <- data.frame(Image = imageFile, stringsAsFactors = FALSE)
# MicrosoftML expects a Label column to exist, but it can contain bogus data
imageToMatch[,"Label"] <- -99 

prediction <- rxPredict(imageModel, data = imageToMatch, extraVarsToWrite = list("Label", "Image"))
prediction$PredictedLabel
