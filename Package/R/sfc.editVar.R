#' Edit variable.
#' 
#' edit the variable of a sfc object .
#' 
#' @inheritParams sfc.addVar
#' @param ind index of the variable to be edited.
#' @return an sfc object.
#' 
#' @export
#' @author Antoine Godin


sfc.editVar<-function(model=stop("Need a model"),ind=NA,var=NA,init=NA,desc=NA){
  if(is.na(ind)&&is.na(var)){
    stop("Need an index or a varname")
  }
  if(is.null(model$variables)){
    stop("The model doesn't have variables yet")
  }else{
    if(is.na(ind)){
      ind=sfc.getIndex(model,var=var)
      if(ind<0){
        stop("The model does not contain a variable ", var)
      }
    }
    if(!is.na(init)){model$variables[ind,2]<-init}
    if(!is.na(desc)){model$variables[ind,3]<-desc}
  }
  return(model)
}
