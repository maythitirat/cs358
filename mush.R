library(data.tree)
library(ggplot2)
library(treemap)
library(stringi)
# mushroom <- read.csv("mushrooms.csv", header = TRUE)
# data(mushroom)

IsPure<-function(data){
  
  length(unique(data[,ncol(data)])) == 1
  
}
# print(IsPure(mushroom))


Entropy<-function(vl){
  en <- vl/sum(vl) * log2(vl/sum(vl))
  en[vl == 0] <-0 #asign Nan
  -sum(en)
}
# print(Entropy(table(mushroom[,ncol(mushroom)])))

InformationGain<-function(tble){
  
  tble<- as.data.frame.matrix(tble)
  enBefore<- Entropy(colSums(tble))
  s <- rowSums(tble)
  enAfter <- sum(s/sum(s)*apply(tble, MARGIN = 1,Entropy))#apply = for loop
  informationGain <- enBefore - enAfter
  return(informationGain)
}

TrainID3 <- function(node, data) {
  node$obsCount <- nrow(data)
  #if the data-set is pure (e.g. all toxic), then
  if (IsPure(data)) {
    #construct a leaf having the name of the pure feature (e.g. 'toxic')
    child <- node$AddChild(unique(data[,ncol(data)]))# Target value มีกี่ค่า
    
    node$feature <- tail(names(data), 1)#ได้Traget value
    child$obsCount <- nrow(data)#count value
    child$feature <- ''
  } else {
    #chose the feature with the highest information gain (e.g. 'color')
    ig <- sapply(colnames(data)[-ncol(data)],
                 function(x) InformationGain(
                   table(data[,x], data[,ncol(data)])#x คือatt ที่ไม่ใช่ traget value
                 )
    )
    # print(ig)
    feature <- names(ig)[ig == max(ig)][1]#ใช้ 1 จะเลือกตัวแรกเสมอ color เลยมาก่อน
    node$feature <- feature
    # print(feature)
    # print(node$feature)
    
    #take the subset of the data-set having that feature value
    childObs <- split(data[,!(names(data) %in% feature)],
                      data[,feature], drop = TRUE)
    
    for(i in 1:length(childObs)) {
      #construct a child having the name of that feature value (e.g. 'red')
      child <- node$AddChild(names(childObs)[i])
      # print(child)
      #call the algorithm recursively on the child and the subset
      TrainID3(child, childObs[[i]])
    }
  }
}
acme <- Node$new("mushroom")
# print(InformationGain(table(mushroom[,c('points','edibility')])))
# print(InformationGain(table(mushroom[,c('color','edibility')])))
TrainID3(acme,mushroom)
print(acme,"feature","obsCount")
# predict <- function(tree,features){
#   if (tree$children[[1]]isLeaf())
# }
#
