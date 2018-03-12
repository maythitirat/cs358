library(data.tree)
library(treemap)
mushroom <- read.csv("cs358/mushrooms.csv", header = TRUE)
# print(summary(mushroom))
# print(attributes(mushroom$cap.color))
# print(str(mushroom))
IsPure <- function(data) {
  length(unique(data[,1])) == 1
}

Entropy <- function( vls ) {
  res <- vls/sum(vls) * log2(vls/sum(vls))
  res[vls == 0] <- 0
  -sum(res)
}


InformationGain <- function( tble ) {
  tble <- as.data.frame.matrix(tble)
  entropyBefore <- Entropy(colSums(tble))
  s <- rowSums(tble)
  entropyAfter <- sum (s / sum(s) * apply(tble, MARGIN = 1, FUN = Entropy ))
  informationGain <- entropyBefore - entropyAfter
  return (informationGain)
}


Attribute_Selection <- function(data){
  for (count in 2:ncol(data)){
    print(c(names(data[count]),InformationGain(table(data[,count],data[,1]))))
  }
}
# print(Attribute_Selection(mushroom))

library ( ggplot2 ) # Visualization

# visualize attribute that has most ig (odor) 
scatter <- ggplot( mushroom, aes( x = mushroom$class,
                                  y = mushroom$odor, color = class ))
scatter <- scatter + geom_jitter()
scatter
hist <- ggplot( mushroom, aes( x = odor, fill = class )) 
hist <- hist + geom_bar(stat='count', 
                        position='dodge') + labs(x = 'Odor',
                                                 y = 'Count of Class')
hist

TrainID3 <- function(node, data) {
  
  node$obsCount <- nrow(data)
  
  #if the data-set is pure (e.g. all toxic), then
  if (IsPure(data)) {
    #construct a leaf having the name of the pure feature (e.g. 'toxic')
    child <- node$AddChild(unique(data[,1]))
    node$feature <- tail(names(data), 1)
    child$obsCount <- nrow(data)
    child$feature <- ''
  } else {
    #chose the feature with the highest information gain (e.g. 'color')
    ig <- sapply(colnames(data)[-1], 
                 function(x) InformationGain(
                   table(data[,x], data[,1])
                 )
    )
    feature <- names(ig)[ig == max(ig)][1]
    node$feature <- feature
    
    #take the subset of the data-set having that feature value
    childObs <- split(data[,!(names(data) %in% feature)], data[,feature], drop = TRUE)
    
    for(i in 1:length(childObs)) {
      #construct a child having the name of that feature value (e.g. 'red')
      child <- node$AddChild(names(childObs)[i])
      
      #call the algorithm recursively on the child and the subset      
      TrainID3(child, childObs[[i]])
    }
  }
}
# tree <- Node$new("mushroom")
# TrainID3(tree, mushroom)
# print(tree, "feature", "obsCount")
# plot(tree)

Predict <- function(tree, features) {
  if (tree$children[[1]]$isLeaf) return (tree$children[[1]]$name)
  child <- tree$children[[features[[tree$feature]]]]
  return ( Predict(child, features))
}
