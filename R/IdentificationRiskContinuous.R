#' @useDynLib IdentificationRiskCalculation
#' @importFrom Rcpp sourceCpp
NULL


#' An Identification Risk Function
#'
#' This function will compute the identification risk for a dataset with synthetic categorical variables.
#' @param origdata dataframe of the origonal data
#' @param syndata list of the different synthetic dataframes
#' @param known vector of the names of the columns in the dataset assumed to be known
#' @param syn vector of the names of the columns in the dataset that are synthetic
#' @param r radius to compare with for continous variables. Radius is either percentage (default) or fixed
#' @param percentage true for a percentage radius, false for a constant radius
#' @export

IdentificationRiskContinuous = function(origdata, syndata, known, syn, r, percentage = TRUE) {
  
  origdataMatrix = data.matrix(origdata)
  colnames(origdataMatrix) = NULL
  
  syndataMatrixList = vector(mode = "list", length(syndata))
  for (i in 1:length(syndata)) {
    temp = data.matrix(syndata[[i]])
    colnames(temp) = NULL
    syndataMatrixList[[i]] = temp
  }
  
  numericKnown = vector(length = length(known))
  numericSyn = vector(length = length(syn))
  
  for (i in 1:length(numericKnown)) {
    numericKnown[i] = match(known[i], colnames(origdata)) - 1
  }
  
  for (i in 1:length(numericSyn)) {
    numericSyn[i] = match(syn[i], colnames(origdata)) - 1
  }
  
  categoricalVector = rep(0, length(colnames(origdata)))
  temp = names(Filter(is.factor, origdata))
  for (i in 1:length(colnames(origdata))) {
    if (is.element(colnames(origdata)[i], temp)) {
      categoricalVector[i] = 1
    }
  }
  
  if (percentage) {
    percentage = 1
  } else {
    percentage = 0
  }
  
  result = .IdentificationRiskContinuousC(origdataMatrix, as.integer(nrow(origdataMatrix)),
                                          as.integer(ncol(origdataMatrix)), syndataMatrixList, as.integer(length(syndata)),
                                          numericKnown, as.integer(length(numericKnown)), numericSyn, as.integer(length(numericSyn)), r, percentage, categoricalVector)
  
  #result = .Call("_categorical_IdentificationRiskContinuousC", origdataMatrix, as.integer(nrow(origdataMatrix)),
  #               as.integer(ncol(origdataMatrix)), syndataMatrixList, as.integer(length(syndata)),
  #               numericKnown, as.integer(length(numericKnown)), numericSyn, as.integer(length(numericSyn)), r, percentage, categoricalVector, PACKAGE = "categorical")
  
  return(result)
}

