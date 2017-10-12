#k fold cross validation example
var <- NA
for (i in (1:10)){
  var[i] <- i 
}


var <- lapply(1:10, function(i){
  return(i)
})