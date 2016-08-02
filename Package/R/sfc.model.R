#' Generating SFC models
#' 
#' Generate an SFC model from a text file and an eventual database. The
#' function generates the model and checks it.
#' 
#' The left hand side (LHS) of an equation should contain only one variable
#' which is considered as endogenous. If the right hand side (RHS) part of the
#' equation contains only a number, this number is set as the initial value
#' (resp. fixed value) for endogenous (resp. exogenous) variable contained in
#' the LHS part of the equation. Each equation might preceded by a description
#' of the equation. The timeline of the model is specified with the reserved
#' keyword 'timeline' and two dates separated by spaces.  #Disposable income yd
#' = w*n - t timeline 1945 2010 The format of the data file (respecting csv
#' format, 'white space' separator and "." decimal) is as follows: Names of
#' each variable should be in the first line, years in the first column.
#' 
#' @param fileName The path to the filename containing the equations. See
#' details hereunder for a description of the format of the text file
#' @param dataFile Optional. The path to the file containing the values for the
#' variables (endogenous) and (exogenous) of the model. See details hereunder
#' for a description of the format of the data file
#' @param modelName Optional, default="SFCmodel". Specifies the name of the
#' model.
#' @param fill Optional, default FALSE. Wether the check of the model offers to
#' fill the lacks found in the model. See sfc.check for more details.
#' @return An object of the class "sfc" representing a model
#' @note %% ~~further notes~~
#' @author Antoine Godin
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% here is where the references go
#' @keywords ~kwd1 ~kwd2
#' 
#' @author Antoine Godin
#' @export


sfc.model <-function(fileName,dataFile=NA,modelName="SFCmodel",fill=F){
  options(warn=-1)
  
  model<-sfc.create(modelName)
  
  if(!is.na(dataFile)){
    data <- read.csv(dataFile)
    variablesName <- names(data)
    for(i in 2:length(data[1,])){
      model<-sfc.addVar(model,var=variablesName[i],init=data[1,i],desc="")
    }
    model<-sfc.setYear(model,as.numeric(data[1,1]),as.numeric(data[length(data[,1]),1]))
  }
  
  
  options(warn=-1)
  modelFile <- file(fileName)
  modelText <- readLines(modelFile, n = -1)
  close(modelFile)
  firstEquation=TRUE
  
  for (i in 1:length(modelText)) {
    lineText = modelText[i]
    if (!grepl("#", lineText)) {
      if(grepl("timeline", lineText)){
        myTable = strsplit(lineText, " ")
        initYears=myTable[[1]][2]
        endYears=myTable[[1]][3]
        model<-sfc.setYear(model,initYears,endYears)
      }else{
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
        #Replacing all reseverved words, for now only in-> inv
        nameEnd = myTable[[1]][1]
        equation = myTable[[1]][2]
        equation<-trim(equation)
        value<-as.double(equation)
        if(!is.na(value)){
          nameEndSearch = gsub("\\bin\\b","inv",nameEnd)
          nameEndSearch<-trim(nameEndSearch)
          indEnd<-sfc.getIndex(model,end=nameEndSearch)
          if(indEnd>0){
            model<-sfc.editEnd(model,ind=indEnd,init=value)
          }else{
            indVar<-sfc.getIndex(model,var=nameEndSearch)
            if(indVar>0){
              model<-sfc.editVar(model,ind=indEnd,init=value)
            }else{
              if(grepl("#",modelText[i-1])){
                display=substring(modelText[i-1],2)
              }else{
                display=""
              }			
              model<-sfc.addVar(model,var=nameEnd,init=value,desc=display)
            }
          }
        }else{
          if(grepl("#",modelText[i-1])){
            displayEquation=substring(modelText[i-1],2)
          }else{
            displayEquation=""
          }			
          model<-sfc.addEqu(model,nameEnd,equation,displayEquation)
        }
      }
    }
  }
  
  options(warn=0)
  
  model<-sfc.check(model,fill=fill)
  return(model)
}
