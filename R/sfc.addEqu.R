#' Add equation.
#' 
#' Add a single equation to a sfc object.
#' 
#' @param model an sfc object.
#' @param var the varaibles in the eqauation.
#' @param eq the equations to be added to the sfc object.
#' @param desc a descriptions of the equation.
#' @return an sfc object.
#' 
#' @author Antoine Godin
#' @export

sfc.addEqu<-function(model=stop("Need a model"),var=stop("Need a variable name!"),eq=stop("Need an equation!"),desc=""){
  
  variable = gsub("in","inv",var)
  variable<-trim(variable)
  equation = gsub("in","inv",eq)
  equation<-trim(equation)
  #replacing all lags. This might be an issue if there are a lag of 2 but not of 1.
  lag=1
  while(grepl(paste("(-",lag,")",sep=""),equation)){
    equation=gsub(paste("(-",lag,")",sep=""), paste("_",lag,sep=""),equation , fixed = T)
    lag=lag+1
  }
  
  if(is.null(model$equations)){
    equations<-matrix(nrow=1,ncol=3,dimnames=list(NULL,c("endogenous value","equation","description")))
    equations[1,1]<-variable
    equations[1,2]<-equation
    equations[1,3]<-desc
  }else{
    equations<-model$equations
    equations<-rbind(equations,c(variable,equation,desc))
  }
  ind=sfc.getIndex(model,end=variable)
  if(ind<0){
    model<-sfc.addEnd(model,var=var,init=NA,lag=0,desc=desc)
  }
  model$equations<-equations
  return(model)
}
