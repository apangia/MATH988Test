# Author: Andrew Pangia
# MATH 988
# Homework 1

#set the seed for replication
set.seed(5);
directory=dirname(parent.frame(2)$ofile);
#dirname(parent.frame(2)$ofile);
setwd(directory);

#2. Write a program that performs classification using any model
#     (some model of your own choosing)

#having read Chapter 4 of ISLR, it appears that a multi-variable logistic
#model will work best. This is done most efficiently by R's glm function.



#4. Use your classification program to classify images of cats and dogs

#The plan is to treat every pixel as a variable. As bad as it is, the 
#different size of the photos will be accounted for by finding the smallest
#picture, and either cropping all other pictures into the same size, 
#or attempting to magnify the smaller photos to a better size.
#Corrupted data will be handled by omitting it.

#to handle the images, the package 'magick' will be used
#this may not work
if(!require("magick"))
{
  install.packages("magick");
}
library("magick");


#in case of an error, return null
gurkeIR<- function(image)
{
  return(tryCatch(image_read(image),error=function(e) {} ));
}

#read the files out of their folders

catFiles<-list.files("Cats", pattern="*.jpg",full.names=TRUE);
catImages<-lapply(catFiles, gurkeIR);
corrupt<-lapply(catImages, is.null);
#cut out corrupt files if there are any of them
if(any(unlist(corrupt)))
{
  catImages<-catImages[-which(unlist(corrupt))];
}

#repeat for dogs
dogFiles<-list.files("Dogs", pattern="*.jpg",full.names=TRUE);
dogImages<-lapply(dogFiles, gurkeIR);
corrupt<-lapply(dogImages, is.null);
if(any(unlist(corrupt)))
{
  dogImages<-dogImages[-which(unlist(corrupt))];
}
#resize all the images; lapply doesn't work for some reason
#catImagesTest<-lapply(catImages, image_resize(), geometry=picSize)

picSize=geometry_size_pixels(width=150, height=150, 
                             preserve_aspect = FALSE);

#convert the images into three-dimensional matrices; then convert these
#into vectors of average color values;
#the color vectors will all be added to a list of 
#vectors, and another list will contain the labels 'cat' or 'dog';
#these two lists will be added to a dataframe, and that dataframe will
#then be murderized by the logistic method

#the list of vectors that act as the data
#dataList<-list();
nameList<-list();

redList<-list();
greenList<-list();
blueList<-list();

catIndex=1;
for (imageSlot in catImages)
{
  #imageSlot
  imageSlot=image_resize(imageSlot, picSize);
  catTestMat=as.integer(image_data(
    image=imageSlot, channel="RGB", frame=1));
  redList[[catIndex]]=mean(as.vector(catTestMat[,,1]));
  greenList[[catIndex]]=mean(as.vector(catTestMat[,,2]));
  blueList[[catIndex]]=mean(as.vector(catTestMat[,,3]));
  #dataList[[catIndex]]=as.vector(catTestMat);
  nameList[[catIndex]]='cat';
  catIndex=catIndex+1;
}

#back up one to point to the correct element of dataList
catIndex=catIndex-1;

dogIndex=1;
for (imageSlot in dogImages)
{
  #imageSlot
  imageSlot=image_resize(imageSlot, picSize);
  dogTestMat=as.integer(image_data(
    image=imageSlot, channel="RGB", frame=1));
  redList[[catIndex+dogIndex]]=mean(as.vector(dogTestMat[,,1]));
  greenList[[catIndex+dogIndex]]=mean(as.vector(dogTestMat[,,2]));
  blueList[[catIndex+dogIndex]]=mean(as.vector(dogTestMat[,,3]));
  #dataList[[catIndex]]=as.vector(catTestMat);
  nameList[[catIndex+dogIndex]]='dog';
  dogIndex=dogIndex+1;
}


avgRed<-unlist(redList);
avgGreen<-unlist(greenList);
avgBlue<-unlist(blueList);
name<-factor(unlist(nameList));
imagesFrame=data.frame(avgRed,avgGreen,avgBlue,name);
#write.csv(imagesFrame,"temp.csv")

#note that 0 represents cat, and 1 represents dog

#remove the categorical variable, which we know is at the end
#len=dim(imagesFrame)[2];

#design a training set of random values; take 75% of the values:
#runif is a Random UNIForm sampler
temp=runif(dim(imagesFrame)[1],0,1);
train=rep(FALSE,dim(imagesFrame)[1]);
train[temp<0.75]=TRUE;

#trainSet=imagesFrame[!!train,];
testSet=imagesFrame[!train,];

fit=glm(imagesFrame$name~.,data=imagesFrame,family=binomial, subset=train);
#look at colours

#set the predicted values
logitChance=predict(fit,testSet,type="response");
predictImages=rep("cat",dim(testSet)[1]);
#images closer to 1 are considered dogs
predictImages[logitChance>0.5]="dog";

ratioCorrect=mean(predictImages==testSet$name);
gurke=table(predictImages,testSet$name);


#new plan--too much data to use the bitmap, so get three variables
#avgRed, avgGreen, avgBlue; get these from summing over the vectors;
#
