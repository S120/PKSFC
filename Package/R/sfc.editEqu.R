#' Edit equation.
#' 
#' Edit an equation in an sfc object.
#' 
#' @inheritParams sfc.addEqu
#' @param ind the index of the equation to be edited.
#' @return an sfc object.
#' 
#' @export
#' @author Antoine Godin
 


sfc.editEqu<-function(model=stop("Need a model"),ind=NA,var=NA,eq=NA,desc=NA){
  if(is.na(ind)&&is.na(var)){
    stop("Need an index or a varname")
  }
  if(is.null(model$equations)){
    stop("The model doesn't have equations yet")
  }else{
    if(!is.na(eq)){
      if(is.na(ind)){
        ind=sfc.getIndex(model,eq=var)
        if(ind<0){
          stop("The model does not contain an equation for ",var)
        }
      }
      eq = gsub("\\bin\\b","inv",eq)
      eq<-trim(eq)
      #replacing all lags. This might be an issue if there are a lag of 2 but not of 1.
      lag=1
      while(grepl(paste("(-",lag,")",sep=""),eq)){
        eq=gsub(paste("(-",lag,")",sep=""), paste("_",lag,sep=""),eq , fixed = T)
        lag=lag+1
      }
      model$equations[ind,2]<-eq
    }
    if(!is.na(desc)){model$equations[ind,3]<-desc}
  }
  return(model)
}
