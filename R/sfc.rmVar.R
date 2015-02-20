#' Remove a variable.
#' 
#' Remove a variable from an sfc object.
#' 
#' @inheritParams sfc.editVar
#' @return an sfc object.
#' 
#' @export
#' @author Antoine Godin


sfc.rmVar<-function(model=stop("Need a model"),ind=stop("Need an index!")){
  if(is.null(model$variables)){
    stop("The model doesn't have variables yet")
  }else{
    model$variables<-model$variables[-ind,]
  }
  return(model)
}

