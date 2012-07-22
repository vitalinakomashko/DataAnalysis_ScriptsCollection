###This function creates a plot of the number of clusters according to k means versus within
###groups sum of squares.
###This function was created according to the example here:
###http://www.statmethods.net/advstats/cluster.html

###Input = a matrix with genes as rows and patients as columns, it will be transposed during the run
###Title - is the character string that you want to use to name the plot
###Result of the function is the plot of the 15 clusters versus the withing groups sum of squares

###Wrapped into the function by Vitalina Komashko, PhD
###Date: April 23d, 2012


kmeansEval = function(data,plotTitle){
  dataScale = scale(t(data))
  wss <- (nrow(dataScale)-1)*sum(apply(dataScale,2,var))
  for(i in 2:15){
    wss[i] <- sum(kmeans(dataScale, centers=i)$withinss)
  }
  plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares",main=plotTitle)
}
