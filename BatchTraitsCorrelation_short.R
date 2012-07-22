batchTrait <- function(clinical, batch){
  outputList = lapply(clinical,comparison, batch)
  outputFrame = data.frame(matrix(unlist(outputList),nrow=length(outputList),byrow=T),row.names=names(outputList))
  colnames(outputFrame) = c("DataType","NumberOfNAs","Test","Pvalue")
  write.table(outputFrame,paste("BatchClinicalInfoCorrelations",".txt",sep=""),sep="\t")
  return(outputFrame)
}

comparison <- function(x,y){
  numberOfNAs = length(which(is.na(x)))
  columnType = class(x)
  print(columnType)
  if (class(x)=="factor" & (length(levels(x)) >= 2) & length(which(is.na(x)))<=length(x)/2){
    testType = chisq.test(x,y)$method
    pValue = chisq.test(x,y)$p.value
  }
  else if (class(x)=="factor" & length(levels(x)) < 2) {
    testType = "less than 2 levels"
    pValue = "not calculated"
  }
  else if (class(x)=="integer" & length(which(is.na(x))) <= length(x)/2){
    testType = kruskal.test(x,as.factor(y))$method
    pValue = kruskal.test(x,as.factor(y))$p.value
  }
  else {
    testType = "too many NA's"
    pValue = "not calculated"
  }
  return(list(columnType,numberOfNAs,testType,pValue))
}
