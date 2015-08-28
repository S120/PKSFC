#' Add a scenario.
#' 
#' Add a scenario (such as a shock to the model) to be added to the sfc object.
#' 
#' @param model an sfc object.
#' @param vars variable that is to be changed.
#' @param values the new values for the variable.
#' @param inits when the scenario should start.
#' @param ends when should the scenario end.
#' @param starts the inital conditions for the scenario.
#' @return an sfc object.
#' 
#' @author Antoine Godin
#' @export

sfc.addScenario <- function(model=stop("Need a model"),vars=stop("Need variables"),values=stop("Need values"),inits=stop("Need starting values"),ends=stop("Need ending values"),starts=NA){
  if(length(vars)!=length(values)&&length(vars)!=length(inits)&&length(vars)!=length(ends)){
    stop("Check vars, values, inits and ends")
  }else{
    if(is.null(model$scenarios)){
      scenario<-{}
      j=0
    }
    else{
      scenario<-model$scenarios
      j=length(scenario)
    }
    for(i in 1:length(vars)){
      scen<-{}
      scen$var=vars[[i]]
      scen$value=values[[i]]
      scen$init=inits[i]-1
      scen$end=ends[i]
      scen$start=starts
      scenario[[i+j]]<-scen
    }
  }
  model$scenarios<-scenario
  return(model)
}
