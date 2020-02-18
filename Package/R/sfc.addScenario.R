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
			start <- starts[[iScen]]
			variablesMat <- variablesMat <- model$baselineMat
			if(sum(is.na(start))!=length(start)){
				for(j in 1:length(names(start))){
					if(!grepl("block",names(start)[j])){
						variablesMat[as.character(seq(from=model$time[1],to=init)),names(start)[j]]=as.double(start[j])	
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
