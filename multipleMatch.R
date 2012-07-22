###This function calculates indices for the matches in the vector. Vector that needs
###to be match contains more than one element
###I suspect there is R function for doing this task but I can't think of one

###Author: Vitalina Komashko
###Date: April 23, 2012

multMatch = function(toMatch,matchAgainst){
  indices = unlist(sapply(toMatch,function(x) which(x==matchAgainst)),use.names=FALSE)
  return(indices)
}