#' PLS model in inner CV loop
#'
#' Called from Wrapper
#'
#' @param xTrain Training data (samples as rows; variables as columns)
#' @param yTrain Training response
#' @param xVal Validation data
#' @param yVal Validation response (for tuning)
#' @param DA Logical for discriminant analysis (classification)
#' @param fitness Fitness function ('MISS', 'AUROC' or 'RMSEP')
#' @param comp Max number of components to try
#' @param scale Whether or not to scale inData (X)
#'
#' @return An object containing:
#' @return (`miss`, `auc` or `rmsep`) A fitness metric
#' @return `nComp` Optimised number of components within range (1:comp)
#' @return `virank` variable importance rankings
#'
#' @export
plsInner=function(xTrain,
                  yTrain,
                  xVal,
                  yVal,
                  DA,
                  fitness,
                  comp=5,
                  scale=TRUE) {
  cond=TRUE
  while(cond) {
    yValInner=tryCatch(
      {  ###These functions provide a mechanism for handling unusual conditions, including errors and warnings.
      if (DA) plsModIn=MUVR::plsda(xTrain,
                                   yTrain,
                                   ncomp=comp,
                                   near.zero.var=TRUE,
                                   scale=scale)
      else plsModIn=MUVR::pls(xTrain,
                              yTrain,
                              ncomp=comp,
                              near.zero.var=TRUE,
                              scale=scale)
####
      yValInner=predict(plsModIn,
                        newdata=xVal,
                        onlyPred=TRUE,
                        scale=scale)$predict[,,]    ##observation new, y variables col, component 1
    }, ##extract the mean sd of training and use it to scale xVal
    error=function(e) return('error'))
###tryCatch(stop(e), error = function(e) e, finally = print("Hello"))
###[1] "Hello"
###<simpleError: test error>

####if there is only one component
    if ((length(yValInner)==1 && yValInner=='error') | any(is.na(yValInner))) comp=comp-1
    ###if any value is NA
     else cond=FALSE
    if (comp==0) cond=FALSE
  }
  returnIn=list()

  if (comp>0){
    if(!DA & !is.matrix(yValInner)) yValInner=as.matrix(yValInner)
    ###(observations,y variables, component) each is changed to a matrix
    if (DA) {
      if (fitness=='MISS') {
        if(comp>1) classes=apply(yValInner,
                                 c(1,3),       ## the max in each row, the output is row observation, col component
                                 which.max)    ###the position of the max one
        else classes=matrix(apply(yValInner,1,which.max),ncol=1)   ###output is a column, choose the biggest in each row
        misClass=apply(classes,
                       2,
                       function(x) sum(x!=as.numeric(yVal)))   ###miss classification number , this is a vector , the miss number under each component
      ##after as.numeric() y class become1,2,3, the same as the position number
        returnIn$miss=min(misClass,na.rm=T)  ##which component has the smallest number of miss classfication

        nComp=which.min(misClass)   ###position, which component has the min (miss Classification)

      } else if (fitness=='BER') {
        if(comp>1) classes=apply(yValInner,
                                 c(1,3),
                                 which.max)
        else classes=matrix(apply(yValInner,1,which.max),ncol=1)

        BER=apply(classes,2,function(x) getBER(actual=as.numeric(yVal),predicted=x))

        returnIn$ber=min(BER,na.rm=T)

        nComp=which.min(BER)

      }    else {
        auc=apply(yValInner[,1,],
                  2,
                  function(x) roc(yVal,x)$auc)    ####area under the curve for the Y group 1
        returnIn$auc=max(auc,na.rm=T)
        nComp=which.max(auc)
      }


    } else {               ####when it is not DA and comp>0
#####################################################################################################################
##Here  I don't understand, why assume the validation set only have -1 and 1
      if (fitness=='MISS') {
        # cat(' miss',count)
        yClassInner=ifelse(yValInner>0,
                           1,
                           -1)
        misClass=apply(yClassInner,
                       2,
                       function(x) sum(x!=yVal))
        returnIn$miss=min(misClass,na.rm=T)
        nComp=which.min(misClass)
      }

      if (fitness=='AUROC') {
        # cat(' auc',count)
        auc=apply(yValInner,
                  2,
                  function(x) roc(yVal,x)$auc)
        returnIn$auc=max(auc,na.rm=T)
        nComp=which.max(auc)
      }
###########################################################################################################################
##################################################################################################################################################
      if (fitness=='RMSEP') {
        # cat(' rmsep',count)
        rmsep=apply(yValInner ,
                    2,
                    function(x) sqrt(sum((yVal-x)^2,na.rm=T)/(length(yVal)-sum(is.na(x)))))
        ####RMSEP = sqrt(mean((y[i] - yhat[i])^2))
        returnIn$rmsep=min(rmsep,na.rm=T)

        nComp=which.min(rmsep)
      }
    }

    returnIn$virank=rank(-MUVR::vip(plsModIn)[,nComp])   ##rank each component's variable of importance

###when comp is 0
  } else {
    nComp=0
    if (fitness=='MISS') returnIn$miss=length(yVal)
    if (fitness=='AUROC') returnIn$auc=0
    if (fitness=='BER') returnIn$ber=1
    if (fitness=='RMSEP') returnIn$rmsep=1E10
    returnIn$virank=rep(1,ncol(xTrain))
  }

  returnIn$nComp=nComp

  return(returnIn)
}
