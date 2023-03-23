#' Extract autoselected variables from MUVR model object
#'
#' @param MUVRclassObject An object of MUVR class
#' @param model Which model to use ("min", "mid" (default), or "max")
#'
#' @return Data frame with order, name and average rank of variables (`order`, `name` & `rank`)
#' @export
getVIRank=function(MUVRclassObject,model='mid') {
  nMod=ifelse(model=='min',
              1,
              ifelse(model=='mid',2,3))
  if(class(MUVRclassObject)[3]=="rdCVnet"){
    VIRanks_vector<-rank(-as.vector(MUVRclassObject$varTable))




    VIRanks=data.frame(order=1:length(MUVRclassObject$Var[[nMod]]),
                       name=names(MUVRclassObject$varTable)[1:length(MUVRclassObject$Var[[nMod]])],
                       rank=VIRanks_vector[1:length(MUVRclassObject$Var[[nMod]])])
    VIRanks$name<-as.character(VIRanks$name)
    rownames(VIRanks)<-VIRanks$name

  }else{


  nVar=round(MUVRclassObject$nVar[nMod])
  VIRanks=sort(MUVRclassObject$VIRank[,nMod])[1:nVar]       ###sequencing them and take the first few of them
                                                            ###sort() do not return rank but the true value
                                                            ##however it sort() rank, so the result is still VIRanks but sequenced from small to big
  VIRanks=data.frame(order=1:nVar,
                     name=names(VIRanks),
                     rank=VIRanks)
  VIRanks$name=as.character(VIRanks$name)

  }
  return(VIRanks)
}
