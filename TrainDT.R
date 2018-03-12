library(data.tree)
mushroom <- read.csv("movie.csv", header = TRUE)
IsPure <- function(data) {
# all first column in mushroom is class
  length(unique(mushroom[,1]) == 1)
}
Entropy <- function(vls){
  res <- vls/sum(vls) * log2(vls/sum(vls))
  print(res)
  res[vls == 0] <- 0
  -sum(res)
}
InformationGain <- function(tble) {
  tble <- as.data.frame.matrix(tble)
  entropyBefore <- Entropy(colSums(tble))
  s <- rowSums(tble)
  entropyAfter <- sum (s / sum(s) * apply(tble, MARGIN = 1, FUN = Entropy ))
  informationGain <- entropyBefore - entropyAfter
  return(informationGain)
}
print(Entropy(m))