#This function performs the Chi-square test only if inputs have at least 2 levels
#If one of the inputs doesn't have at least 2 levels then it returns NA

chisqLevelTest = function(x,y){
  x = factor(x)
  y = factor(y)
  if(length(levels(x))<2 | length(levels(y))<2){
    return(NA)
  }
  else{
    return(chisq.test(x,y)$p.value)
  }
}