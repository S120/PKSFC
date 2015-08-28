#' Find index.
#' 
#' Find the indexs for sfc object inputs.
#' 
#' @param model an sfc object.
#' @param var the variable that you want the index for, if applicable.
#' @param eq the equation that you want the index for, if applicable.
#' @param end the endogenous variable that you want the index for, if applicable.
#' @return the required index.
#' 
#' @author Antoine Godin


sfc.getIndex<-function(model=stop("Need a model"),var=NA,eq=NA,end=NA){
  if(!is.na(var)){
    ind = which(model$variables[,1]==var,arr.ind=T)
    if(length(ind)==0){ind=-1}
    return(ind)
  }else if(!is.na(eq)){
    ind = which(model$equations[,1]==eq,arr.ind=T)
    if(length(ind)==0){ind=-1}
    return(ind)
  }else if(!is.na(end)){
    ind = which(model$endogenous[,1]==end,arr.ind=T)	
    if(length(ind)==0){ind=-1}
    return(ind)
  }else{
    stop("Need either a variable (var), and endogenous (end) or an equation (eq)!")
  }
}
