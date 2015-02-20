#' Create an sfc object from an eviews file.
#' 
#' Given an eviews file, the file will be parsed and a sfc object will be created.

#' 
#' @inheritParams sfc.model
#' @return An sfc object
#' 
#' @author Antoine Godin
#' @export


sfc.eviews <-function(fileName,modelName="SFCmodel"){
  
  options(warn=-1)
  
  eviewsFile = file(fileName)
  eviewsText = readLines(eviewsFile, n = -1)
  close(eviewsFile)
  
  endOfModel=FALSE
  
  model<-sfc.create(modelName)
  
  for (i in 1:length(eviewsText)) {
    lineText = eviewsText[i]
    #Check wether the line is not a comment
    if (!grepl("'", lineText)&&!endOfModel) {
      
      #If the line declares a serie
      if (grepl("series", lineText)) {
        myTable = strsplit(lineText, " ")
        if (grepl("series", myTable[1])) {
          name=myTable[[1]][2]
          model<-sfc.addVar(model,name,NA,NA)
        }
      } #end if (grepl("series", lineText))
      
      #If the line gives a description of an existing serie
      else if (grepl("displayname", lineText)) {
        myTable = strsplit(lineText, " ")
        if (grepl("displayname", myTable[1])) {
          myTable2=strsplit(myTable[[1]][1],"\\.")
          name=myTable2[[1]][1]
          nameSearch=gsub("in","inv",name)
          ind=sfc.getIndex(model,var=nameSearch)
          displayName=""
          for(j in 2:length(myTable[[1]])){
            displayName=paste(displayName,myTable[[1]][j])
          }
          if(ind<0){
            model<-sfc.addVar(model=model,var=name,init=NA,desc=displayName)
          }else{
            model<-sfc.editVar(model=model,ind=ind,desc=displayName)
          }
        }
      }#end if (grepl("displayname", lineText))
      
      #If the line adds an equation to the model
      else if(grepl(".append", lineText)){
        myTable = strsplit(lineText, "=")
        #This is to manage the case when there are other = in the equation (use of logical operators)
        if(length(myTable[[1]])>2){
          tempStr<-myTable[[1]][2]
          for(iter in 3:length(myTable[[1]])){
            tempStr<-paste(tempStr,myTable[[1]][3],sep="=")
            myTable[[1]]=myTable[[1]][-3]
          }
          myTable[[1]][2]=tempStr
        }
        
        myTable2=strsplit(myTable[[1]][1], " ")
        #Here need to do something about more than one space between append and the variable to add
        nameEnd=myTable2[[1]][2]
        equation = myTable[[1]][2]
        
        #replacing all lags. This might be an issue if there are a lag of 2 but not of 1.
        lag=1
        while(grepl(paste("(-",lag,")",sep=""),equation)){
          equation=gsub(paste("(-",lag,")",sep=""), paste("_",lag,sep=""),equation , fixed = T)
          lag=lag+1
        }
        
        if(grepl("\\'",eviewsText[i-1])){
          displayEquation=gsub("\\'","#",eviewsText[i-1])
        }else{
          displayEquation=""
        }
        nameEndSearch=gsub("in","inv",nameEnd)
        ind = sfc.getIndex(model,var=nameEndSearch)
        if(ind<0){
          warning("The equation ",lineText," has an endogenous variable ",nameEndSearch," which is not declared.")
        }
        model<-sfc.addEqu(model,nameEnd,equation,displayEquation)
      }#end if(grepl(".append", lineText))
      
      #If the line decleares the timespan of simulations
      else if(grepl("wfcreate", lineText)){
        myTable = strsplit(lineText, " ")
        initYears=myTable[[1]][length(myTable[[1]])-1]
        endYears=myTable[[1]][length(myTable[[1]])]
        model<-sfc.setYear(model,initYears,endYears)
      }#end if(grepl("wfcreate", lineText))
      
      #Usually the end of the model declaration
      else if(grepl(".scenario", lineText)){
        endOfModel=TRUE
      }#end if(grepl(".scenario", lineText))
      
      #The line gives a value for a variable (either initial value for endogenous variables, or value for exogenous variable)
      else if (grepl("=", lineText)) {
        myTable = strsplit(lineText, "=")
        name=myTable[[1]][1]
        nameSearch=gsub("in","inv",name)
        nameSearch<-trim(nameSearch)
        ind=sfc.getIndex(model,var=nameSearch)
        value=gsub("in","inv",myTable[[1]][2])
        value<-trim(value)
        if(ind<0){
          model<-sfc.addVar(model,name,value,NA)
        }else{
          model<-sfc.editVar(model=model,ind=ind,init=value)
          ind=sfc.getIndex(model,end=nameSearch)
          if(ind>0){
            model<-sfc.editEnd(model=model,ind=ind,init=value)
          }
        }			
      } #end if (grepl("=", lineText))
      
    } #end if (grepl(\'\",lineText)>0)"
  } #end for (i in 1:length(eviewsText))
  
  model<-sfc.check(model)
  
  options(warn=0)
  return(model)
}
