#'This can be run to test if the command input of parameters contradict to each other
#'And also check the structure of the data
#'If something goes wrong warning messages are given
#'
#'@param X   The original data of X, not the result after onehotencoding
#'@param Y   The original data of Y
#'@param ML  ML in MUVR
#'@param DA  DA in MUVR
#'@param method  RF or PLS so far in MUVR
#'@param fitness fitness in MUVR
#'
#'@param nInner nInnerin MUVR
#'@param nOuter nOuter in MUVR
#'@param varRatio varRatio in MUVR
#'@param scale scale
#'@param modReturn modReturn in MUVR
#'@param logg logg in MUVR
#'@param parallel parallel in MUVR
#'
#'@return correct_input: the original input(call) and the real input used in MUVR when you enter your input
#'@export
#'


checkinput<-function(X,
                     Y,
                     ML,
                     DA,
                     method,
                     fitness,
                     nInner,
                     nOuter,
                     varRatio,
                     scale,
                     modReturn,
                     logg,
                     parallel){

###Step 0: save the model in put
  call<- match.call()

####Step 1: check if X exists and what is the structure of X
#check for  existence
  if(missing(X)){stop("X data is missing.")}

#analyze X datatype
  if(class(X)[1]=="data.frame"){X<-MUVR::onehotencoding(X)
    cat("X is transformed to a matrix by onehotencoding.","\n")
    }
  factor_number<-0
  numeric_integer_number<-0
  character_number<-0
  logical_number<-0
  for (i in 1:ncol(X))
  {if (class(X[,i])=="logical"){logical_number=logical_number+1}
    if (class(X[,i])=="numeric"|class(X[,i])=="integer"){numeric_integer_number=numeric_integer_number+1}
    if (class(X[,i])=="character"){character_number=character_number+1}
    if (class(X[,i])=="factor"){factor_number=factor_number+1}
  }
  cat("\n","In X","\n","There are","\n",numeric_integer_number, "numeric variables,","\n",
      factor_number,"factor variables","\n",
      character_number,"character varaibles","\n",
      logical_number,"logical variables.","\n")




######Step 2.check if the datatype of input parameters are correct when they are not missing. For example, when DA=1, it should give an error
#In short: The scenario when parameters are not missing but the input is wrong ################################

#, ML, DA, method, fitness, nInner, nOuter,varRatio
##Parameters Group 1: These parameters are correlated with each other and with Y.
# when missing() and when the input of the is correct are discusse later

if(!missing(ML)){if(class(ML)!="logical"){stop("\n","ML must be T or F.")}}
if(!missing(DA)){if(class(DA)!="logical"){stop("\n","DA must be T or F.")}}
if(!missing(method)){{if(!method%in%c("PLS","RF")){stop("\n","method type is not correct.")}}}
if(!missing(fitness)){if(!fitness%in%c("MISS","RMSEP","AUROC","BER")){stop("\n","fitness type is not correct.")}}

##Parameters Group 2: nInner and nOuter can be correlated with each other and liminted by lengh(Y)
if(!missing(nInner)){if(class(nInner)!="numeric"&class(nInner)!="integer"){stop("\n","nInner is a number.")}
  if(nInner<2){stop("\n","nInner is too small.")}}
if(!missing(nOuter)){if(class(nOuter)!="numeric"&class(nOuter)!="integer"){stop("\n","nOuter is a number.")}
  if(nOuter<3){stop("\n","nOuter is too small.")}}


# Parameters Group 3: These parameters are not correlated with each other
if(!missing(scale)){if(class(scale)!="logical"){stop("\n","scale must be T or F.")}}
if(!missing(modReturn)){if(class(modReturn)!="logical"){stop("\n","modReturn must be T or F.")}}
if(!missing(logg)){if(class(logg)!="logical"){stop("\n","logg must be T or F.")}}
if(!missing(parallel)){if(class(parallel)!="logical"){stop("\n","parallel must be T or F.")}}
if(!missing(varRatio)){if(class(varRatio)!="numeric"&class(varRatio)!="integer"){stop("\n","varRatio is a number.")}
                      if(varRatio<=0|varRatio>1){stop("\n","varRation should be in (0,1].")}
                      if(varRatio<0.5|varRatio>0.95){warning("varRatio is recommened to be within [0.5,0.95]")}}

########## Step 3: For parameters group 3 that are not correlated with each other, give them a value when they are missing
  ##when they are not missing, leave them be
if(missing(scale))scale=T
if(missing(modReturn))modReturn=F
if(missing(logg))logg=F
if(missing(parallel))parallel=T
if(missing(varRatio))varRatio=0.75

######Step 4: #Specify the scenario when ML is missing and ML=T. The scenario of ML=F will be discussed later
  if (missing(ML)){ML=F}
  if(ML==T){
    if(missing(DA)){DA=F}
    if(missing(fitness)){fitness="MISS"}
    if(fitness!="MISS"|DA==T){warning("\n","Multilevel model must have DA=F and fitness=MISS.")
      fitness="MISS"
      DA=F}
    if(missing(method)){method="RF"}
  }

#####Step 5:Y is allowed missing when ML=T, so here discuss when Y is missing, and when it is not missing, analyze its structure
  if(missing(Y)){if(ML!=T){stop("\n","Y data is missing")}   ###when ML=T, a Y will be built.
    } else{if(nrow(X)!=length(Y)){stop("The number of observations are not the same.")}

  ##analyze Y data type
  if(class(Y)=="logical"){stop("\n","Y must be a numeric or factor variable.")
  }else if(class(Y)=="character"){Y=factor(Y)
                               warning("\n","Original Y is a character variable and transformed to a factor variable now.")
                                  if(length(levels(Y))>10){warning("\n","There are more than 10 levels in Y.")}
                            cat("Y is transformed to a factor variable with",length(levels(Y)),"levels and",length(Y),"observations.")
  }else if(class(Y)=="factor"){if(length(levels(Y))>10){warning("\n","There are more than 10 levels in Y.")}
                               cat("\n","Y is a factor variable with",length(levels(Y)),"levels and",length(Y),"observations.")
  }else{cat("\n","Y is a numeric variable with",length(Y),"obsevations","\n")}
    }

####Step 6 For parameters group 2: when nOuter and nInner is missing, they are given a value
##They are also limited by Y
  #nInner and nOuter
  if(missing(nOuter)){nOuter=6}
  if(missing(nInner)){nInner=nOuter-1}
  #Y
  if(length(Y)<=nInner)stop("\n","Y has too few observations.")


##now there are som scenarios needs to be discussed for when ML=F
##When DA,fitness and method is missing and when DA=T/F, fitness, and method



#####step 7.about datatype and contracdictory of command when ML=F
##now original X is a data frame and Y is a factor or numeric variable
##ML=F


if(ML==F)
{
  if(class(Y)=="factor"){if(missing(DA))DA=T
                        if(DA==F){warning("\n","When Y is a factor, must use DA.")
                                  DA=T}
                        if(missing(fitness)){fitness="MISS"}
                        if(fitness=="RMSEP"){warning("\n","When is a factor, fitness must be MISS, AUROC or BER.")}

                        if(missing(method)){method="RF"}}

 if(class(Y)=="numeric"|class(Y)=="integer")
 {if(missing(DA))DA=F
   if(DA==T){if(length(levels(factor(Y)))<=10){Y=factor(Y)
                                             warning("\n","Y is a numeric variable. It will be transformed to a factor variable when DA==T.")
                                            }else{Y=factor(Y)
                                             stop("\n","Y is a numeric variable. It will be transformed to a factor variable when DA==T.","\n","There are >10 levels when transformed to factor.")}
           if(missing(fitness)){fitness="MISS"}
          if(fitness=="RMSEP"){warning("\n","When is a factor, fitness must be MISS, AUROC or BER.")}

          if(missing(method)){method="RF"}
           }

   if(missing(fitness)){fitness="RMSEP"}
   if(fitness!="RMSEP"){warning("\n","When is a factor, fitness must be RMSEP.")}

   if(missing(method)){method="RF"}

  }


}
correct_input<-list(call,
        ML,
        DA,
        method,
        fitness,
        nInner,
        nOuter,
        varRatio,
        scale,
        modReturn,
        logg,
        parallel)
names(correct_input)<-c("call","ML","DA","method","fitness","nInner","nOuter","varRatio","scale","modReturn","logg","parallel")


cat("\n","\n","Original input(call) and the input that will used in MUVR:\n")
return(correct_input)

}












