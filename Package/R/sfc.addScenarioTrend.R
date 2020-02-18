#' Add a scenario with a trend.
#' 
#' Add a scenario (such as a shock to the model) to be added to the sfc object.
#' 
#' @param model an sfc object.
#' @param vars variable that is to be changed.
#' @param trends the new values for the variable, as a matrix of time series.
#' @param starts the inital conditions for the scenario.
#' @return an sfc object.
#' 
#' @author Antoine Godin
#' @export

sfc.addScenarioTrend <- function(model=stop("Need a model"),vars=stop("Need variables"),trends=stop("Need trends"),starts=NA){
	if(length(vars)!=length(trends)){
		stop("Check vars and trends")
	}else{
		if(is.null(model$scenarios)){
			scenario<-{}
			nbScen=0
		}
		else{
			scenario<-model$scenarios
			nbScen=length(scenario)
		}
		for(iScen in 1:length(vars)){
			#TODO: TEST LENGTH OF TREND WITH LENGTH OF TIME IN MODEL
			scen<-{}
			var=vars[[iScen]]
			trend=trends[[iScen]]
			if(ncol(trend)!=(model$time[2]+1-model$time[1]))
					stop("Trend length is not consistent with timelength of model")
			else{
				variablesMat <- model$baselineMat
				if(sum(is.na(starts))!=length(starts)){
					for(j in 1:length(names(start))){
						if(!grepl("block",names(start)[j])){
							variablesMat[as.character(seq(from=model$time[1],to=init)),names(start)[j]]=as.double(start[j])	
						}
					}
				}
				for(j in 1:length(var)){
					variablesMat[,var[j]]=trend[,j]
				}
				scen$mat<-variablesMat
				scenario[[iScen+nbScen]]<-scen
			}
		}
		model$scenarios<-scenario
		return(model)
	}
}
