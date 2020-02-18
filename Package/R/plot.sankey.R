#' Plots the sankey of a specific dataset.
#' 
#' @param dataset a matrix containing the results of an SFC simulation where the column names corresponds to the variables of the model
#' @param filename A csv file containing a Flow Matrix in symbolic form, using the same name of variables than the dataset
#' @param outputfile The name of the file to generate containing the plot
#' @param period The period to use for the values in the Sankey
#' @return a Sankey plot.
#' 
#' @export
#' @author Antoine Godin

plot.sankey <- function(dataset=stop("Need a dataset to plot a sanket"), filename=stop("Need a Flow Matrix csv file"), outputfile=NA, period=1) {
	
	if( require(networkD3) ){
		flowmat<-as.data.frame(read.csv(filename,row.names=1))
		
		sectors<-colnames(flowmat)
		sectors_id<-colnames(flowmat)
		flows<-rownames(flowmat)
		flows_id<-rownames(flowmat)
		nbSectors<-length(sectors_id)
		nbFlows<-length(flows_id)
		
		nodes<-data.frame(ID=c(paste(sectors_id,"_Exp",sep=""),flows_id,paste(sectors_id,"_Inc",sep="")),x=c(rep(1,length(sectors_id)),rep(2,length(flows_id)),rep(3,length(sectors_id))),label=c(sectors,flows,sectors))
		source<-c()
		target<-c()
		Value<-c()
		for(r in 1:nrow(flowmat)){
			for(c in 1:ncol(flowmat)){
				cellVal<-as.character(flowmat[r,c])
				if(nchar(cellVal)>0){
					#If there's an entry in the cell
					if(substr(cellVal,1,1)=="-"){
						cellVal<-substr(cellVal,2,nchar(cellVal))
						source<-c(source,(c-1))
						target<-c(target,(nbSectors+r-1))
						Value<-c(Value,dataset[[cellVal]][period])
					}else{
						source<-c(source,(nbSectors+r-1))
						target<-c(target,(nbFlows+nbSectors+c-1))
						Value<-c(Value,dataset[[cellVal]][period])
					}
				}
			}
		}
		links<-data.frame(source=source,target=target,value=Value)
		network=sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target", Value = "value", NodeID = "ID", units = "GBP", fontSize = 12, nodeWidth = 30)
		
		if(!is.na(outputfile)){
			saveNetwork(network,file=outputfile)
		}
		network  
	}else{
		stop( "networkD3 could not be loaded. Please install it to use this function." )
	}
}