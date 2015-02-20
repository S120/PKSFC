#' Simulating SFC models
#' 
#' Simulate using the Gaiss-Seodel algorithm an sfc model.
#' 
#' Simulates, via the Gauss-Seidel algorithm, an SFC model for the baseline and
#' all scenarios that where added. See sfc.addScenario for more on scenarios
#' and sfc.GaussSeidel for more on the Gauss-Seidel algorithm.
#' ore details than the description above ~~
#' 
#' @param model contains the sfc model to be simulated
#' @param tolValue minimum variation between two iteration of the Gauss-Seidel
#' algorithm.
#' @param maxIter maximum number of iteration for the Gauss-Seidel algorithm
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' 
#' @author Antoine Godin
#' @export

simulate.sfc <- function(model, tolValue = 1e-10, maxIter=10000) {
  eval(parse(text=sfc.eval(model)))
  variablesMat<-matrix(data=0,nrow=length(model$time),ncol=length(model$variables[,1]),dimnames=list(c(model$time),c(model$variables[,1])))
  for(i in 1:length(model$variables[,1])){
    variablesMat[,i]=as.double(model$variables[i,2])
  }
  baseline<-sfc.runScenario(model,variablesMat,tolValue,maxIter,equations,blocks,variables,prev)
  result<-list(baseline=baseline)
  if(!is.null(model$scenarios)){
    for(i in 1:length(model$scenarios)){
      variablesMatTemp<-variablesMat
      scen<-model$scenarios[[i]]
      if(sum(is.na(scen$start))!=length(scen$start)){
        for(j in 1:length(names(scen$start))){
          if(!grepl("block",names(scen$start)[j])){
            variablesMatTemp[as.character(seq(from=model$time[1],to=scen$init)),names(scen$start)[j]]=as.double(scen$start[j])	
          }
        }
      }
      for(j in 1:length(scen$var)){
        variablesMatTemp[as.character(seq(from=scen$init,to=scen$end)),scen$var[j]]=as.double(scen$value[j])
      }
      scenName<-paste("scenario_",i,sep="")
      result[[scenName]]<-sfc.runScenario(model,variablesMatTemp,tolValue,maxIter,equations,blocks,variables,prev)
    }
  }
  
  return(result)
}
