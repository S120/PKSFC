#' Evaluate an sfc object.
#' 
#' Reformats an sfc object into a format that can be used by sfc.GaussSeidel.
#' 
#' @param object sfc object
#' @param check would you like to run a test for missing information?
#' @return a list of strings consistent with what is required to get the GaussSeidel
#' algorithm to work.
#' 
#' @export
#' @author Antoine Godin



sfc.eval <- function(object=stop("Need a model to evaluate"),check=F) {
  options(warn=-1)
  interLine="\n"
  
  if(!is.sfc(object)){
    stop("The object to be evaluated is not an SFC model")
  }else{	
    
    
    varStr = "variables <- list(\n"
    endVarStr<-")\n"
    firstVar=TRUE
    variables<-object$variables
    for (i in 1:length(variables[, 1])) {
      value = as.double(variables[i, 2])
      if(firstVar){
        varStr <- paste(varStr,variables[i, 1],"=",variables[i, 2],sep = "")
        firstVar=FALSE
      }else{
        varStr <- paste(varStr,",\n",variables[i, 1],"=",variables[i, 2],sep = "")
      }
    }
    varStr <- paste(varStr,")\n", sep = "")
    
    if(!check){
      endogenous = object$endog
      firstPrev=TRUE
      prevStr = "prev <- list(\n"
      for (i in 1:length(endogenous[, 1])) {
        value = as.double(endogenous[i, 2])
        if(endogenous[i,3]>0){
          for(l in 1:endogenous[i,3]){
            if(firstPrev){
              prevStr <- paste(prevStr, endogenous[i, 1], "_",l, "=", endogenous[i, 2], sep = "")
              firstPrev=FALSE
            }else{
              prevStr <- paste(prevStr,",",interLine, endogenous[i, 1], "_",l, "=", endogenous[i, 2], sep = "")
            }
          }
        }
      }
      prevStr <- paste(prevStr,interLine, ")\n", sep = "")
      
      equations = object$equations
      blocks=object$blocks
      eqStr = "equations <- list(\n"
      blockStr<-"c(\n"
      intraEqStr = " = quote("
      endEqStr = "),\n"
      lastEqStr = ")\n"
      endBlockStr = "),\n"
      lastBlockStr = ")\n)\n"
      for(j in 1:length(blocks)){
        indexBlock=blocks[[j]]
        eqStr=paste(eqStr,blockStr,sep="")
        for (i in 1:length(indexBlock)) {
          equ=object$equations[indexBlock[i], 2]
          if (i == length(indexBlock)) {
            eqStr = paste(eqStr, equations[indexBlock[i], 1], intraEqStr, equ , lastEqStr, sep = "")
          } else {
            eqStr = paste(eqStr, equations[indexBlock[i], 1], intraEqStr, equ , endEqStr, sep = "")
          }
        }
        if(j==length(blocks)){
          eqStr=paste(eqStr,lastBlockStr,sep="")
        }else{
          eqStr=paste(eqStr,endBlockStr,sep="")
        }
      }
      
      blockStr<-"blocks<-list(\n"
      innerBlockStr<-"c("
      endInnerBlockStr<-"),\n"
      lastInnerBlockStr<-")\n)\n"
      for(i in 1:length(blocks)){
        innerBlock=blocks[[i]]
        blockStr<-paste(blockStr,innerBlockStr,sep="")
        for(j in 1:length(innerBlock)){
          if(j==length(innerBlock)){
            blockStr<-paste(blockStr,innerBlock[j],sep="")
          }else{
            blockStr<-paste(blockStr,innerBlock[j],",",sep="")
          }
        }
        if(i==length(blocks)){
          blockStr<-paste(blockStr,lastInnerBlockStr,sep="")
        }else{
          blockStr<-paste(blockStr,endInnerBlockStr,sep="")
        }
        
      }
    }else{
      endogenous = object$endog
      firstPrev=TRUE
      prevStr = "prev <- list(\n"
      for (i in 1:length(endogenous[, 1])) {
        value = 0
        if(firstPrev){
          prevStr <- paste(prevStr, endogenous[i, 1],"=", value, sep = "")
          firstPrev=FALSE
        }else{
          prevStr <- paste(prevStr,",",interLine, endogenous[i, 1], "=", value, sep = "")
        }
        if(endogenous[i,3]>0){
          for(l in 1:endogenous[i,3]){
            prevStr <- paste(prevStr,",",interLine, endogenous[i, 1], "_",l, "=", endogenous[i, 2], sep = "")
          }
        }
      }
      prevStr <- paste(prevStr,interLine, ")\n", sep = "")
      
      equations = object$equations
      eqStr = "equations <- list(\n"
      intraEqStr = " = quote("
      endEqStr = "),\n"
      lastEqStr = "))\n"
      for(j in 1:length(equations[,1])){
        if(j==length(equations[,1])){
          eqStr = paste(eqStr, equations[j, 1], intraEqStr, equations[j, 2],lastEqStr, sep = "")
        }else{
          eqStr = paste(eqStr, equations[j, 1], intraEqStr, equations[j, 2],endEqStr, sep = "")
        }
      }
      blockStr=""
    }
  }
  options(warn=0)
  return(paste(c(eqStr,blockStr,varStr,prevStr),sep=""))
}
