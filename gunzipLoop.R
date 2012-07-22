##This function unzippes all files in the directory that end with
## .gz extension. I created it to deal with downloaded data from
## GEO.

##Date: May 7, 2012
##Author: Vitalina Komashko

require(R.utils)
gunzipLoop = function(){
  files = list.files(pattern=".gz")
  for (i in seq(along=files)){
    gunzip(files[i],remove=FALSE)
  }
}
