#' Add variable.
#' 
#' Add a single variable to a sfc object.
#' 
#' @param model an sfc object.
#' @param var the variable to be added to the sfc object.
#' @param init the inital conditions for the variable.
#' @param desc a description of the variable.
#' @return an sfc object.
#' 
#' @author Antoine Godin
#' @export

sfc.addVar<-function(model=stop("Need a model"),var=stop("Need a variable name!"),init="NA",desc=""){
  var = gsub("in","inv",var)
  var<-trim(var)
  
  if(is.null(model$variables)){
    variables<-matrix(nrow=1,ncol=3,dimnames=list(NULL,c("name","initial value","description")))
    variables[1,1]<-var
    variables[1,2]<-init
    variables[1,3]<-desc
  }else{
    variables<-model$variables
    variables<-rbind(variables,c(var,init,desc))
  }
  model$variables<-variables
  return(model)
}
