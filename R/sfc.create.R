#' create an sfc object.
#' 
#' Create an emtpy sfc object.
#' 
#' @inheritParams sfc.model
#' @return an sfc object.

#' @export
#' @author Antoine Godin

sfc.create<-function(modelName=stop("Need a model name")){
  model<-{}
  model$name<-modelName
  model$simulated<-FALSE
  class(model)<-"sfc"
  return(model)
}
