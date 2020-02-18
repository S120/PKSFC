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
	if(length(vars)!=length(values)|length(vars)!=length(inits)|length(vars)!=length(ends)){
		stop("Check vars, values, inits and ends")
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
			var <- vars[[iScen]]
			val <- values[[iScen]]
			init <- inits[iScen]-1
			end <- ends[iScen]
			variablesMat <- model$baselineMat
			if(sum(is.na(starts))!=length(starts)){
				for(j in 1:length(names(starts))){
					if(!grepl("block",names(starts)[j])){
						variablesMat[as.character(seq(from=model$time[1],to=init)),names(starts)[j]]=as.double(starts[j])	
					}
				}
			}
			for(j in 1:length(var)){
				variablesMat[as.character(seq(from=init,to=end)),var[j]]=as.double(val[j])
			}
			scen$mat<-variablesMat
			scenario[[iScen+nbScen]]<-scen
		}
	}
	model$scenarios<-scenario
	return(model)
}
