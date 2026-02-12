#'compute cv using population sd
#'
#'@importFrom magrittr "%>%"
#'
#'@examples cv.p(c(value1, value2, value3))
#'
#'@export
sd.p=function(x){sd(x, na.rm=T)*sqrt((length(x)-1)/length(x))}
cv.p=function(x){
  
  if(is.na(sd.p(x))==T) {
    return(NA_real_)
  } else if(sd.p(x)==0) {
    return(0)
  } else {100*sd.p(x)/mean(x, na.rm=T)}
}