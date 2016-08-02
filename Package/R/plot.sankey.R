#' Plots the sankey of a specific dataset.
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

plot.sankey <- function(dataset=stop("Need a dataset to plot a sanket"), filename=stop("Need a Flow Matrix csv file"), outputfile=NA, period=1) {
  
  flowmat<-as.data.frame(read.csv(filename,row.names=1))
  
  sectors<-colnames(flowmat)
  sectors_id<-colnames(flowmat)
  flows<-rownames(flowmat)
  flows_id<-rownames(flowmat)
  nbSectors<-length(sectors_id)
  nbflows<-length(flows_id)
  
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
}