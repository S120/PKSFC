#' Add endogenous values to sfc.
#' 
#' Add a single endogenous variable to the sfc object.
#' 
#' @param model an sfc object.
#' @param var the endogenous variable to be added.
#' @param init the initial condtions for the endogenous variable.
#' @param lag which lag is to be used for updating this variable.
#' @param desc descriptions of endogenous variable.
#' @return an sfc object.
#' 
#' @author Antoine Godin
#' @export

sfc.addEnd<-function(model=stop("Need a model"),var=stop("Need a variable name!"),init=NA,lag=0,desc=NA){
  
  variable = gsub("\\bin\\b","inv",var)
  variable<-trim(variable)
  
  if(is.null(model$endogenous)){
    endogenous<-matrix(nrow=1,ncol=4,dimnames=list(NULL,c("name","initial value","lag","description")))
    endogenous[1,1]<-variable
    endogenous[1,2]<-init
    endogenous[1,3]<-lag
    endogenous[1,4]<-desc
  }else{
    endogenous<-model$endogenous
    endogenous<-rbind(endogenous,c(variable,init,lag,desc))
  }
  ind=sfc.getIndex(model,var=variable)
  if(ind<0){
    model<-sfc.addVar(model,var=var,init=NA,desc=desc)
  }
  
  model$endogenous<-endogenous
  return(model)
}
