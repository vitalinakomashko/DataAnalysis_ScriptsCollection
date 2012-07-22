###I found this function on the R mailing list. It concatenates together 2 vectors and retains their levels.
###I walked through this code in an interactive session and I have to admit I wouldn't have throught of doing so myself
c.Factor = function (x, y){
  ##Combine the levels of two factors
  newlevels = union(levels(x), levels(y))
  ##Get the indices of levels that belong to the second factor
  m = match(levels(y), newlevels)
  ##This part is intersting. First we remove the class attribute from each factor. When I types class(unclass(x)) I got "integer" and here is how
  ##it looks like: [1] 1 1 2 3; attr(,"levels"); [1] "d" "e" "k". Now we subset m according to the elements of y. So if m was 4,5,6 and then using
  ##this example m[unclass(y)] will return 4,4,5,6. It will be an integer vector. So by concatenating both I will retain the levels 1,2,3,4,5,6. Next
  ##I just need to assign the levels to this new vector and then convert to a new factor class.
  ans = c(unclass(x), m[unclass(y)])
  levels(ans) = newlevels
  class(ans) = "factor"
  ans
}
