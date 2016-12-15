bayesClassifier<-function(menClass, womenClass, document, menPrior, womenPrior){
  #gets row count of men and women data frames
  wCount<-nrow(womenClass)
  mCount<-nrow(menClass)
  #adds frequency column to menClass and womenClass data frames and calculates the frequency each word appeards in the classes
  #This also compacts the the like words together in one row so we can get vocabulary size
  menClass<-as.data.frame(table(menClass))
  womenClass<-as.data.frame(table(womenClass))
  #get the voacbulary size of each class
  vocabM<-nrow(menClass)
  vocabW<-nrow(womenClass)
  #finds intersection of document data frame and the manClass and womanClass dataframes
  intersectM<-menClass[is.element(menClass$menClass, intersect(document$`unlist(x)`, menClass$menClass)),]
  intersectW<-womenClass[is.element(womenClass$womenClass, intersect(document$`unlist(x)`, womenClass$womenClass)),]
  #conditional probabilities that each intersecting word, this would be the place to add smoothing if desired in place of the 0s
  intersectM$Freq<-(intersectM$Freq+0)/(mCount+vocabM+0)
  intersectW$Freq<-(intersectW$Freq+0)/(wCount+vocabW+0)
  #finds product the frequency column and multiplies by the priors
  posteriorM<-prod(intersectM$Freq)*menPrior
  posteriorW<-prod(intersectW$Freq)*womenPrior
  #test for higher posterior
  if(posteriorW>posteriorM){
    answer<-"Woman"
    return("Woman")
  }
  answer<-"Man"
  return("Man")
}

