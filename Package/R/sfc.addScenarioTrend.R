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
		if(nrow(model$baselineMat)==0|ncol(model$baselineMat)!=nrow(model$variables)){
			variablesMat<-matrix(data=0,nrow=length(model$time),ncol=length(model$variables[,1]),dimnames=list(c(model$time),c(model$variables[,1])))
			for(i in 1:length(model$variables[,1])){
				variablesMat[,i]=as.double(model$variables[i,2])
			}
			model$baselineMat<-variablesMat
		}
		if(is.null(model$scenarios)){
			scenario<-{}
			nbScen=0
		}
		else{
			scenario<-model$scenarios
			nbScen=length(scenario)
		}
		for(iScen in 1:length(vars)){
			scen<-{}
			var=vars[[iScen]]
			trend=trends[[iScen]]
			if(dim(trend)[1]!=length(model$time))
					stop("Trend length is not consistent with timelength of model")
			else{
				variablesMat <- model$baselineMat
				if(sum(is.na(starts))!=length(starts)){
					for(j in 1:length(names(starts))){
						if(!grepl("block",names(starts)[j])){
							variablesMat[as.character(model$time[1]),names(starts)[j]]=as.double(starts[j])	
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
