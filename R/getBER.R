#' get Balanced Error Rate from classification analysis
#'
#' @param actual Vector of actual classifications of samples
#' @param predicted Vector of predicted classifications of samples
#'
#' @return Balanced Error Rate (BER)
#' @export
getBER<-function (actual, predicted)
{
  if (length(actual) != length(predicted)) stop ("Mismatch in length of arguments")
  if (!is.factor(actual)) actual = factor(actual)
  predicted=factor(predicted,levels=levels(actual))
  levs = levels(actual)
  nlevs = length(levs)
  confMat = matrix(0, nrow=nlevs, ncol=nlevs + 1)
  rownames(confMat) = levs
  colnames(confMat) = paste0("pred.", c(levs, "NA"))
  for (i in 1:nlevs) {
    whLev.i = which(actual == levs[i])
    for (j in 1:nlevs) confMat[i, j] = sum(predicted[whLev.i] == levs[j], na.rm = TRUE)
    #if i=1,j=2 confMat is the number of the obs actual in group 1, but predicted in group 2
    confMat[i, nlevs + 1] = sum(is.na(predicted[whLev.i]))
  }   ##the last column is to see how many predicted are NA when actual is level i
  if (sum(is.na(predicted)) == 0) confMat = confMat[, -(nlevs + 1)]
  ## When there is no NA in predicted, there is no value for the whole column,then remove the column
  confMat.wrong = confMat
  diag(confMat.wrong) = 0
  BER = sum(apply(confMat.wrong, 1, sum, na.rm = TRUE)/apply(confMat, 1, sum, na.rm = TRUE), na.rm = TRUE)/nlevs
  ##balance error rate
  return(BER)
}
