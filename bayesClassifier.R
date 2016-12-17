bayesClassifier<-function(menClass, womenClass, document, menPrior, womenPrior){
  #gets counts of words in each class
  mCount<-nrow(menClass)
  wCount<-nrow(womenClass)
  #combines the menClass and womenClass dataframes into a vocabulary dataframe
  vocabAll<-rbind(menClass, womenClass)
  #collapses like words in vocabAll and find count of all unique words in vacabulary
  vocabAll<-as.data.frame(table(vocabAll))
  vocabCount<-nrow(vocabAll)
  #collapses menClass and womenClass data frames and finds the frequency of each word
  menClass<-as.data.frame(table(menClass))
  womenClass<-as.data.frame(table(womenClass))
  #finds intersection of document data frame and the menClass and womenClass dataframes
  intersectM<-menClass[is.element(menClass$menClass, intersect(document$`unlist(x)`, menClass$menClass)),]
  intersectW<-womenClass[is.element(womenClass$womenClass, intersect(document$`unlist(x)`, womenClass$womenClass)),]
  #conditional probabilities that each intersecting word, this would be the place to add smoothing if desired in place of the 0s
  intersectM$Freq<-(intersectM$Freq+0)/(mCount+vocabCount+0)
  intersectW$Freq<-(intersectW$Freq+0)/(wCount+vocabCount+0)
  #finds product the frequency column and multiplies by the priors
  posteriorM<-prod(intersectM$Freq)*menPrior
  posteriorW<-prod(intersectW$Freq)*womenPrior
  #test for higher posterior
  if(posteriorW>posteriorM){
    return("Woman")
  }
  return("Man")
}

