#' Check format of sfc object.
#' 
#' Run a sanity check for errors in a sfc object.
#' 
#' @param model an sfc object.
#' @param fill if some values are missing would you like to be prompted to fill them in?
#' 
#' @export
#' @author Antoine Godin

sfc.check<-function(model=stop("Need a model"),fill=F){
  if(is.null(model$equations)){stop("The model doesn't contain any equations")}
  if(is.null(model$endogenous)){warning("Need to define endogenous variables for the model")}
  if(is.null(model$variables)){warning("Need to define variables for the model")}
  if(length(model$endogenous[,1])!=length(model$equations[,1])){
    warning("Number of equations not equal to number of endogenous variables, check the model!")
    return(model)
  }
  #This part deals with the value of variables that depends on other variables values
  notDefined = matrix(nrow = 1, ncol = 4)
  firstNotDefined = TRUE
  firstVar = TRUE
  varStr = "varTemp <- list(\n"
  for(i in 1:length(model$variables[,1])){
    if(!is.na(model$variables[i, 2])){
      value = as.double(model$variables[i, 2])
      if (is.na(value)) {
        if (firstNotDefined) {
          notDefined[1, 1] = model$variables[i, 1]
          notDefined[1, 2] = model$variables[i, 2]
          notDefined[1, 3] = model$variables[i, 3]
          firstNotDefined = FALSE
        } else {
          notDefined = rbind(notDefined, c(model$variables[i, 1], model$variables[i, 2], model$variables[i, 3]))
        }
      } else {
        if (firstVar) {
          varStr <- paste(varStr, model$variables[i, 1], "=", model$variables[i, 2], sep = "")
          firstVar = FALSE
        } else {
          varStr <- paste(varStr, ",", model$variables[i, 1], "=", model$variables[i, 2], sep = "")
        }
      }
    }
  }
  varStr2 <- paste(varStr, ")", sep = "")
  eval(parse(text = varStr2))
  textUndefined = "value<- eval(substitute(substitute(var, varTemp), list(var =quote("
  i=1
  while(!is.na(notDefined[1,1])&&i<=length(notDefined[, 1])) {
    textUndefined2 = paste(textUndefined, notDefined[i, 2], "))))", sep = "")
    eval(parse(text = textUndefined2))
    options(show.error.messages = FALSE)
    value<-try(eval(value))
    options(show.error.messages = TRUE)
    if(!is.double(value)){
      row=notDefined[i,]
      notDefined=notDefined[-i,]
      notDefined=rbind(notDefined,c(row[1],row[2],row[3],row[4]))
      i=i-1
    }else{
      indVar=sfc.getIndex(model,var=notDefined[i,1])
      indEnd=sfc.getIndex(model,end=notDefined[i,1])
      model<-sfc.editVar(model,ind=indVar,init=value)
      if(indEnd>0){model<-sfc.editEnd(model,ind=indEnd,init=value)}
      varStr <- paste(varStr, ",", notDefined[i, 1], "=", eval(value), sep = "")
      varStr2 <- paste(varStr, ")", sep = "")
      eval(parse(text = varStr2))
    }
    i=i+1
  }
  #This section deals with the lags
  for (j in 1:length(model$endogenous[,1])) {
    for (i in 1:length(model$equations[,1])) {
      temp <- gsub("[ \t\n\r\f\v()/\\+\\<\\>\\*-]+"," ",paste(" ",model$equations[i,2]))
      ind <- regexpr(paste("([ \t\n\r\f\v])",model$endogenous[j, 1],"_([0-9])",sep=""), temp)
      while (ind[1] > -1) {
        temp <- substring(temp, ind[1] + nchar(model$endogenous[j, 1])+1)
        if (nchar(temp) > 0 && substr(temp, 0, 1) == "_") {
          value <-as.integer(substr(temp,2,3))
          if (!is.na(value)) {
            model<-sfc.editEnd(model,ind=j,lag=max(model$endogenous[j,3],value))
          }
        }
        ind <- regexpr(paste("([ \t\n\r\f\v])",model$endogenous[j, 1],"_([0-9])",sep=""), temp)
      }
    }
  }#end for (j in 1:length(endogenous[,1]))
  ind <- which(model$endogenous[, 3] > 0&is.na(model$endogenous[, 2]))
  if(length(ind)>0){
    for(i in 1:length(ind)){
      indVar<-sfc.getIndex(model,var=model$endogenous[ind[i],1])
      model$endogenous[ind[i],2]<-model$variables[indVar,2]
    }
  }
  ind <- which(model$endogenous[, 3] > 0&is.na(model$endogenous[, 2]))
  if(length(ind)>0){
    if(fill){
      for(i in 1:length(ind)){
        cat("Initial value for",model$endogenous[ind[i],1],"[0]?")
        ans = scan(what=double(),nlines=1,quiet=TRUE)
        if(length(ans)==0){ans=0}
        model$endogenous[ind[i],2]<-ans
        indVar<-sfc.getIndex(model,var=model$endogenous[ind[i],1])
        model$variables[indVar,2]<-ans
      }
    }else{
      warning("The following variables have lags but no initial values:",paste(" - ",model$endogenous[ind,1]))
    }
  }
  #this computes the matrix containing the relations between endogenous values
  equEndMatrix = matrix(data = 0, nrow = length(model$endogenous[, 1]), ncol = length(model$equations[, 1]),dimnames=list(c(model$endogenous[,1]),c(model$equations[,1])))
  for (j in 1:length(model$endogenous[, 1])) {
    ind <- which(model$equations[, 1] == model$endogenous[j, 1])
    temp <- gsub("[ \t\n\r\f\v()/\\<\\>\\+\\*-]+", " ", model$equations[ind, 2])
    for (i in 1:length(model$endogenous[, 1])) {
      ind2 <- regexpr(paste("([ \t\n\r\f\v]|^)", model$endogenous[i, 1], "([ \t\n\r\f\v]|$)", sep = ""), temp)
      if (length(ind2)>0&&ind2 > -1) {
        equEndMatrix[ind[1], i] = 1
      }
    }
  }
  #this creates the block of independant equations.
  #TODO: If there are no equations with no independencies, all equations left are treated as one block
  matrix<-equEndMatrix
  blocks<-{}
  total<-matrix(data=0,nrow=length(matrix[,1]),ncol=1)
  for(i in 1:length(matrix[,1])){
    total[i]=sum(matrix[i,])
  }
  indexes<-sort.int(total,index.return=T)
  maxBlocks=max(indexes$x)
  alreadyFound=c()
  iter=1
  while(length(alreadyFound)!=length(matrix[,1])){
    blockIndex=c()
    eqsInd<-indexes$ix[which(indexes$x==0)]
    if(length(eqsInd)>0){
      for(j in 1:length(eqsInd)){
        if(length(which(alreadyFound==eqsInd[j]))==0){
          matrix[,eqsInd[j]]=0
          alreadyFound=c(alreadyFound,eqsInd[j])
          blockIndex=c(blockIndex,eqsInd[j])
        }
      }
    }
    if(length(blockIndex)==0){
      eqsInd<-indexes$ix[which(indexes$x>0)]
      for(j in 1:length(eqsInd)){
        if(length(which(alreadyFound==eqsInd[j]))==0){
          matrix[,eqsInd[j]]=0
          alreadyFound=c(alreadyFound,eqsInd[j])
          blockIndex=c(blockIndex,eqsInd[j])
        }
      }
    }
    if(!is.list(blocks)){
      blocks=as.list(blocks)	
    }
    blocks[[iter]]<-blockIndex
    total<-matrix(data=0,nrow=length(matrix[,1]),ncol=1)
    for(i in 1:length(matrix[,1])){
      total[i]=sum(matrix[i,])
    }
    indexes<-sort.int(total,index.return=T)
    iter=iter+1
  }
  eval(parse(text=sfc.eval(model,check=T)))
  errEq=c()
  for(i in 1:length(equations)){
    eq <- eval(substitute(substitute(var, prev), list(var = equations[[i]])))
    eq <- eval(substitute(substitute(var, variables), list(var = eq)))		
    value=NULL
    try(value <- eval(eq),silent=TRUE)
    if(is.null(value)){
      #			errEq=c(errEq,equations[[i]])
      errEq<-c(errEq,eq)
    }
  }
  if(fill&length(errEq)>0){
    exit=FALSE
    while(length(errEq)>0&&!exit){	
      cat("One or more exogenous variables are not defined in the following equations ",paste(errEq,"\n")," do you want to insert these manually [Yes]/No?")
      ans = scan(what=character(),nlines=1,quiet=TRUE)
      if(length(ans)==0||ans=="Yes"){
        while(length(ans)==0||ans=="Yes"){
          cat("Insert name, value and description (return after each value)")
          ans = scan(what=character(),nlines=3,quiet=TRUE)
          name=ans[1]
          value=as.double(ans[2])
          desc=""
          for(i in 3:length(ans)){
            desc=paste(desc,ans[i],sep=" ")
          }
          model<-sfc.addVar(model,var=name,init=value,desc=desc)
          cat("Are there other variables to add [Yes]/No?")
          ans = scan(what=character(),nlines=1,quiet=TRUE)
          if(length(ans)>0&&ans=="No"){exit=TRUE}
        }
        eval(parse(text=sfc.eval(model,check=T)))
        errEq=c()
        for(i in 1:length(equations)){
          eq <- eval(substitute(substitute(var, prev), list(var = equations[[i]])))
          eq <- eval(substitute(substitute(var, variables), list(var = eq)))		
          value=NA
          try(value <- eval(eq),silent=TRUE)
          if(is.na(value)){
            errEq=c(errEq,equations[[i]])
          }
        }
      }else{
        warning("Equations ",paste("\n",errEq),"\ncontain variables that are not defined, check the model")
      }
    }
  }else if(length(errEq)>0){
    warning("Equations ",paste("\n",errEq),"\ncontain variables that are not defined, check the model")
  }
  
  if(is.null(model$time)){
    if(fill){
      cat("Years are not set, do you want to inser these manually [Yes]/No?")
      ans = scan(what=character(),nlines=1,quiet=TRUE)
      if(length(ans)==0||ans=="Yes"){
        cat("Insert initial period and final period (return after each value)")
        ans = scan(what=double(),nlines=2,quiet=TRUE)
        model<-sfc.setYear(model,init=ans[1],end=ans[2])
      }else{
        warning("No year defined")
      }
    }else{
      warning("No year defined")
    }
  }
  
  #creation of the result
  model$matrix<-equEndMatrix
  model$blocks<-blocks
  return(model)
  
}
