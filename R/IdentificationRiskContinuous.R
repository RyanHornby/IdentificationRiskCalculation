#' @useDynLib IdentificationRiskCalculation
#' @importFrom Rcpp sourceCpp
NULL


#' An Identification Risk Function
#'
#' This function will compute the identification risk for a dataset with synthetic categorical variables.
#' This function assumes categorical variables will be as factors.
#' @param origdata dataframe of the original data
#' @param syndata list of the different synthetic dataframes
#' @param known vector of the names of the columns in the dataset assumed to be known
#' @param syn vector of the names of the columns in the dataset that are synthetic
#' @param r radius to compare with for continuous variables. Radius is either percentage (default) or fixed.
#' Radius can be the same for all continuous variables or specific to each. To specify for each use a vector, with
#' the radii ordered in the same order those columns appear in the dataset.
#' @param percentage true for a percentage radius, false for a constant radius
#' @param euclideanDist true for a euclidean distance radius, false otherwise
#' @export

IdentificationRisk = function(origdata, syndata, known, syn, r, percentage = TRUE, euclideanDist = FALSE) {
  
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
  
  flag = 0
  if (length(r) == 1) {
    r = rep(r, length(colnames(origdata)))
    flag = 1
  } 
  
  categoricalVector = rep(0, length(colnames(origdata)))
  temp = names(Filter(is.factor, origdata))
  tempR = vector(length = length(colnames(origdata)))
  j = 0
  for (i in 1:length(colnames(origdata))) {
    if (is.element(colnames(origdata)[i], temp)) {
      categoricalVector[i] = 1
      
      tempR[i] = 0
    } else if ((i %in% (numericKnown + 1) || i %in% (numericSyn + 1)) && flag == 0) {
      j = j + 1
      if (j > length(r)) {
        print("Error: radius vector must be length 1 or number of continuous synthetic and known variables.")
        return(NULL)
      }
      tempR[i] = r[j]
    }
  }
  
  if (j != length(r)) {
    print("Warning: length of radius vector longer than number of synthetic and known variables.")
    print(paste("Warning: only using the first", j, "radii."))
  }
  
  if (flag == 0) {
    r = tempR
  }
  
  if (percentage) {
    percentage = 1
  } else {
    percentage = 0
  }
  
  if (euclideanDist) {
    euclideanDist = 1
  } else {
    euclideanDist = 0
  }
  
  result = .IdentificationRiskContinuousC(origdataMatrix, as.integer(nrow(origdataMatrix)),
                                          as.integer(ncol(origdataMatrix)), syndataMatrixList, as.integer(length(syndata)),
                                          numericKnown, as.integer(length(numericKnown)), numericSyn, as.integer(length(numericSyn)), r, percentage, euclideanDist, categoricalVector)
  
  #result = .Call("_categorical_IdentificationRiskContinuousC", origdataMatrix, as.integer(nrow(origdataMatrix)),
  #               as.integer(ncol(origdataMatrix)), syndataMatrixList, as.integer(length(syndata)),
  #               numericKnown, as.integer(length(numericKnown)), numericSyn, as.integer(length(numericSyn)), r, percentage, categoricalVector, PACKAGE = "categorical")
  
  return(result)
}

