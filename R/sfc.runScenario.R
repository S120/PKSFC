#' Run a scenario.
#' 
#' Run a scenario for an sfc object.
#' 
#' @inheritParams simulate.sfc
#' @param data data structure that contians the parmeters, endogenous 
#' variables and number of itterations perblock.
#' @param equations the equations of the system.
#' @param blocks the blocks that each equation corresponds to.
#' @param variables the variables in the system.
#' @param prev previous lag.
#' @return the results of the scenario.
#' 
#' @author Antoine Godin



sfc.runScenario<-function(model,data,tolValue,maxIter,equations,blocks,variables,prev){
  iterations <- length(model$time)
  GSiterations<-matrix(0,nrow=iterations,ncol=length(equations),dimnames=list(model$time,paste("iter block",seq(1:length(equations)))))
  
  for (t in 2:iterations) {
    
    for(i in 1:length(model$variables[,1])){
      varName<-model$variables[i,1]
      if(is.na(data[t-1,varName])){
        variables[varName]=1
      }else{
        variables[varName]=data[t-1,varName]
      }
    }
    for(i in 1:length(model$endog[,3])){
      if(model$endog[i,3]>0){
        for(j in 1:model$endog[i,3]){
          varName<-model$endog[i,1]
          prevName=paste(varName,"_",j,sep="")
          lag<-t-j
          if(lag<1){
            lag<-1
          }
          prev[prevName]=data[lag,varName]
        }
      }
    }
    for (b in 1:length(equations)) {
      blockOfEquations <- equations[[b]]
      varNames<-names(blockOfEquations)
      resultGS<-sfc.GaussSeidel(blockOfEquations, tolValue,maxIter,variables,prev)
      values<-resultGS$values
      GSiterations[t,b]<-resultGS$iterations
      for(i in 1:length(blockOfEquations)){
        #				print(variables[varNames[i]])
        variables[varNames[i]]<-values[[varNames[i]]]
        #				print(variables[varNames[i]])
        data[t,varNames[i]]=values[[varNames[i]]]
      }
      
    }
  }	
  
  data<-cbind(data,GSiterations)
  return(data)
}
