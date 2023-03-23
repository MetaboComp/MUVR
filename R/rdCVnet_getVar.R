#' the function to get the variables selected
#' @param rdCVnetObject a object from rdCVnet
#' @param span for smooth curve:  how smooth the curve need to be
#' @param outlier if remove ourlier variables or not
#' @param percent_quantile range from 0 to 0.5. When select_variables_by quantile, this value represent the first quantile.
#' @param percent_smoothcurve If select_variables_by smoothcurve, then it is robust
#' @param option quantile or smoothcurve
#'
#' @return a rdCVnet object
#' @export
#'
rdCVnet_getVar<-function(rdCVnetObject,
               option=c("quantile", "smoothcurve"),
               span=1,   # c(0.5, 0.75, 1,1.25),
               outlier=T,
               percent_smoothcurve=0.05,
               percent_quantile=0.25){
   if(percent_quantile<=0|percent_quantile>=0.5|!is.numeric(percent_quantile)){
      stop("\npercent_quantile must be between 0 and 0.5")
    }
    if(percent_smoothcurve<=0|percent_smoothcurve>1|!is.numeric(percent_smoothcurve)){
      stop("\npercent_smoothcurve must be between 0 and 1")
    }
  if(missing(option)){
    option="smoothcurve"
  }
  if(outlier==T){
    quartile_075<-quantile(rdCVnetObject$nonZeroRep)[4]
    quartile_025<-quantile(rdCVnetObject$nonZeroRep)[2]
    IQR<-quartile_075-quartile_025
    low_boundary<-quartile_025-1.5*IQR
    high_boundary<-quartile_075+1.5*IQR
    num_of_variables<-vector()
    fitness<-vector()
    for(i in 1:length(rdCVnetObject$nonZeroRep)){
    if(rdCVnetObject$nonZeroRep[i]<= high_boundary&rdCVnetObject$nonZeroRep[i]>=low_boundary){
      num_of_variables<-c(num_of_variables,rdCVnetObject$nonZeroRep[i])
      fitness<-c(fitness,rdCVnetObject$fitnessRep[i])
    }else{
      cat("\n",rdCVnetObject$nonZeroRep[i],
          "is an outlier for the number of variables selected. Therefore the combination of",
          "number of variables =",rdCVnetObject$nonZeroRep[i],
          ",fitness =",rdCVnetObject$fitnessRep[i],",is removed")
    }
    }

  }else if (outlier==F){
    num_of_variables<-rdCVnetObject$nonZeroRep
    fitness<-rdCVnetObject$fitnessRep

  } else {stop("Outlier needs to be logical")}
##############################################################
  if(option=="quantile"){
    cum_varTable<-rdCVnetObject$cum_varTable
    varTable<-rdCVnetObject$varTable
    minmidmax_quantile<-quantile(num_of_variables,
                                 c(percent_quantile, 0.5, 1-percent_quantile))
    minlimit_quantile=floor( minmidmax_quantile[1])  ### take the floor value in case no value is selected
    midlimit_quantile=floor( minmidmax_quantile[2])  ###
    maxlimit_quantile=floor( minmidmax_quantile[3])
    ##min limit: take less
    for(s in 1:length(cum_varTable)){
      ## set safeguard argument in case there are 0 values
      if(s!=1){
        if(minlimit_quantile<cum_varTable[s]){
          minlimit_num_quantile<-as.numeric(names(cum_varTable)[1:s-1])
          minlimit_num_quantile<-minlimit_num_quantile[!is.na(minlimit_num_quantile)]
          break}
      }else{
        if(minlimit_quantile<cum_varTable[s]){
          minlimit_num_quantile<-as.numeric(names(cum_varTable)[1])
          minlimit_num_quantile<-minlimit_num_quantile[!is.na(minlimit_num_quantile)]
          break}
      }
    }

    ##min limit: take less
    for(s in 1:length(cum_varTable)){
      ## set safeguard argument in case there are 0 values
      if(s!=1){
        if(midlimit_quantile<cum_varTable[s]){
          midlimit_num_quantile<-as.numeric(names(cum_varTable)[1:s-1])
          midlimit_num_quantile<-midlimit_num_quantile[!is.na(midlimit_num_quantile)]
          break}
      }else{
        if(midlimit_quantile<cum_varTable[s]){
          midlimit_num_quantile<-as.numeric(names(cum_varTable)[1])
          midlimit_num_quantile<-midlimit_num_quantile[!is.na(midlimit_num_quantile)]
          break}
      }
    }

    ##min limit: take less
    for(s in 1:length(cum_varTable)){
      ## set safeguard argument in case there are 0 values
      if(s!=1){
        if(maxlimit_quantile<cum_varTable[s]){
          maxlimit_num_quantile<-as.numeric(names(cum_varTable)[1:s-1])
          maxlimit_num_quantile<-maxlimit_num_quantile[!is.na(maxlimit_num_quantile)]
          break}
      }else{
        if(maxlimit_quantile<cum_varTable[s]){
          maxlimit_num_quantile<-as.numeric(names(cum_varTable)[1])
          maxlimit_num_quantile<-maxlimit_num_quantile[!is.na(maxlimit_num_quantile)]
          break}
      }
    }

    minnames_quantile<-vector()
    midnames_quantile<-vector()
    maxnames_quantile<-vector()

    for(s in 1:length(varTable)){
      if(varTable[s]%in%minlimit_num_quantile){
        minnames_quantile<-c(minnames_quantile,names(varTable)[s])}
      if(varTable[s]%in%midlimit_num_quantile){
        midnames_quantile<-c(midnames_quantile,names(varTable)[s])}
      if(varTable[s]%in%maxlimit_num_quantile){
        maxnames_quantile<-c(maxnames_quantile,names(varTable)[s])}
    }

    nVar<- c(minlimit_quantile,midlimit_quantile,maxlimit_quantile)
    Var<-list(min=minnames_quantile,
                       mid=midnames_quantile,
                       max=maxnames_quantile
    )

    names(nVar)<-c("Qmin","Qmid","Qmax")

  }else if (option=="smoothcurve"){
    cum_varTable<-rdCVnetObject$cum_varTable
    varTable<-rdCVnetObject$varTable
    nonZeroRep_vector<-c(num_of_variables)
    fitnessRep_vector<-c(fitness)
    nonZeroRep_vector_grid<-seq(min(nonZeroRep_vector),max(nonZeroRep_vector),1)

    fit_temp<-loess(fitnessRep_vector~nonZeroRep_vector, span = span,degree=2)
    predict_temp<-predict(fit_temp,
                          newdata = data.frame(nonZeroRep_vector=nonZeroRep_vector_grid)
    )
    #fit_temp<-lm(fitnessRep_vector ~ bs(nonZeroRep_vector,
    #                          df=3,  ### when intercept is false degree of freedom = df-degree  df must >=3,  df = length(knots) + degree
    #                          degree=3))
    #predict_temp<-predict(fit_temp,
    #        newdata = list(nonZeroRep_vector=seq(min(nonZeroRep_vector),
    #                                           max(nonZeroRep_vector),
    #                                           1)))


    scaled_predict_temp<-(predict_temp-min(predict_temp))/abs(diff(range(predict_temp)))
    maxIndex_smoothcurve <-
      max(which(scaled_predict_temp <= percent_smoothcurve))
    minIndex_smoothcurve <-
      min(which(scaled_predict_temp <= percent_smoothcurve))
    varMin_smoothcurve <- nonZeroRep_vector_grid[minIndex_smoothcurve]
    varMax_smoothcurve <- nonZeroRep_vector_grid[maxIndex_smoothcurve]
    varMid_smoothcurve <-
      round(exp(mean(log(c(
        nonZeroRep_vector_grid[minIndex_smoothcurve], nonZeroRep_vector_grid[maxIndex_smoothcurve]
      ))))) # Geometric mean of min and max. This one has decimals



    ##min limit: take less
    for(s in 1:length(cum_varTable)){
      ## set safeguard argument in case there are 0 values
      if(s!=1){
        if(varMin_smoothcurve<cum_varTable[s]){
          varMin_num_smoothcurve<-as.numeric(names(cum_varTable)[1:s-1])
          varMin_num_smoothcurve<-varMin_num_smoothcurve[!is.na(varMin_num_smoothcurve)]
          break}
      }else{
        if(varMin_smoothcurve<cum_varTable[s]){
          varMin_num_smoothcurve<-as.numeric(names(cum_varTable)[1])
          varMin_num_smoothcurve<-varMin_num_smoothcurve[!is.na(varMin_num_smoothcurve)]
          break}
      }
    }

    ##mid limit: take less
    for(s in 1:length(cum_varTable)){
      ## set safeguard argument in case there are 0 values
      if(s!=1){
        if(varMid_smoothcurve<cum_varTable[s]){
          varMid_num_smoothcurve<-as.numeric(names(cum_varTable)[1:s-1])
          varMid_num_smoothcurve<-varMid_num_smoothcurve[!is.na(varMid_num_smoothcurve)]
          break}
      }else{
        if(varMid_smoothcurve<cum_varTable[s]){
          varMid_num_smoothcurve<-as.numeric(names(cum_varTable)[1])
          varMid_num_smoothcurve<-varMid_num_smoothcurve[!is.na(varMid_num_smoothcurve)]
          break}
      }
    }

    ##max limit: take less
    for(s in 1:length(cum_varTable)){
      ## set safeguard argument in case there are 0 values
      if(s!=1){
        if(varMax_smoothcurve<cum_varTable[s]){
          varMax_num_smoothcurve<-as.numeric(names(cum_varTable)[1:s-1])
          varMax_num_smoothcurve<-varMax_num_smoothcurve[!is.na(varMax_num_smoothcurve)]
          break}
      }else{
        if(varMax_smoothcurve<cum_varTable[s]){
          varMax_num_smoothcurve<-as.numeric(names(cum_varTable)[1])
          varMax_num_smoothcurve<-varMax_num_smoothcurve[!is.na(varMax_num_smoothcurve)]
          break}
      }
    }

    minnames_smoothcurve<-vector()
    midnames_smoothcurve<-vector()
    maxnames_smoothcurve<-vector()

    for(s in 1:length(varTable)){
      if(varTable[s]%in%varMin_num_smoothcurve){
        minnames_smoothcurve<-c(minnames_smoothcurve,names(varTable)[s])}
      if(varTable[s]%in%varMid_num_smoothcurve){
        midnames_smoothcurve<-c(midnames_smoothcurve,names(varTable)[s])}
      if(varTable[s]%in%varMax_num_smoothcurve){
        maxnames_smoothcurve<-c(maxnames_smoothcurve,names(varTable)[s])}
    }

    nVar<- c(varMin_smoothcurve,
                         varMid_smoothcurve,
                         varMax_smoothcurve)
    Var<-list(min=minnames_smoothcurve,
                          mid=midnames_smoothcurve,
                          max=maxnames_smoothcurve
    )
    rdCVnetObject$span<-span
    names(nVar)<-c("min","min","max")

  }else{
    stop("\n There is no such option")
    }

  rdCVnetObject$Var<-Var
    rdCVnetObject$nVar<-nVar
return(rdCVnetObject)
}
