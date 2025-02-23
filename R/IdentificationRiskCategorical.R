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
#' @export

IdentificationRiskCategorical = function(origdata, syndata, known, syn) {

  origdataMatrix = dfToMatrix(origdata)
  colnames(origdataMatrix) = NULL

  syndataMatrixList = vector(mode = "list", length(syndata))
  for (i in 1:length(syndata)) {
    temp = dfToMatrix(syndata[[i]])
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

  result = .IdentificationRiskC(origdataMatrix, as.integer(nrow(origdataMatrix)),
                                as.integer(ncol(origdataMatrix)), syndataMatrixList, as.integer(length(syndata)),
                                numericKnown, as.integer(length(numericKnown)), numericSyn, as.integer(length(numericSyn)))
  
  #result = .Call("_categorical_IdentificationRisk", origdataMatrix, as.integer(nrow(origdataMatrix)),
  #            as.integer(ncol(origdataMatrix)), syndataMatrixList, as.integer(length(syndata)),
  #            numericKnown, as.integer(length(numericKnown)), numericSyn, as.integer(length(numericSyn)), PACKAGE = "categorical")

  return(result)
}

