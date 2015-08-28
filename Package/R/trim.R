#' Remove spaces from a string
#' 
#' @param text Input text to be trimmed
#' @return the trimmed text
#' 
#' @author Antoine Godin

trim<-function(text){
  return(gsub("[[:space:]]","",text))
}
