#include <Rcpp.h>
using namespace Rcpp;

/*int IdentificationRisk(int *dataMatrix, int rows, int cols, int *syndataMatrices,
                   int num, int *knowncols, int numKnown, int *syncols, int numSyn
                   //,int *sVectorRet, int *expRiskVectorRet, int *trueRateVectorRet,
                   //int *falseRateVectorRet, int *cVectorRet, int *tVectorRet,
                   //int *kVectorRet, int * fVectorRet
                   ) {

  /*int cMatrix[rows][num]; // Rcpp::NumericMatrix cMatrix(rows, num);
  int tMatrix[rows][num];
  int kMatrix[rows][num];
  int fMatrix[rows][num];

  int sArray[num]; // NumericVector sArray(num);
  int expArray[num];
  int trueRateArray[num];
  int falseRateArray[num];

  memset(cMatrix, 0, rows * num);
  memset(tMatrix, 0, rows * num);
  memset(kMatrix, 0, rows * num);
  memset(fMatrix, 0, rows * num);

  memset(sArray, 0, num);
  memset(expArray, 0, num);
  memset(trueRateArray, 0, num);
  memset(falseRateArray, 0, num);

  Rcpp::NumericMatrix cMatrix(rows, num);
  Rcpp::NumericMatrix tMatrix(rows, num);
  Rcpp::NumericMatrix kMatrix(rows, num);
  Rcpp::NumericMatrix fMatrix(rows, num);

  NumericVector sArray(num);
  NumericVector expArray(num);
  NumericVector trueRateArray(num);
  NumericVector falseRateArray(num);

  char match_k[rows];
  int (*syndataMatrix)[cols];

  for (int i = 0; i < rows; i++) {
    for (int j = 0; j < num; j++) {
      syndataMatrix = syndataMatrices[j];
      memset(match_k, 1, rows);

      for (int k = 0; k < rows; k++) {
        // try to match person i with every person in the synthetic set
        // by first checking if the known variables
        for (int l = 0; l < numKnown; l++) {
          if (dataMatrix[i][knowncols[l]] != syndataMatrix[k][knowncols[l]]) {
            match_k[k] = 0;
            break;
          }
        }
        // then if those match check the synthetic ones
        if (match_k[k]) {
          for (int l = 0; l < numSyn; l++) {
            if (dataMatrix[i][syncols[l]] != syndataMatrix[k][syncols[l]]) {
              match_k[k] = 0;
              break;
            }
          }
          cMatrix[i][j]++;

        }
      }

      if (cMatrix[i][j] == 1) {
        sArray[j]++;
      }

      if (cMatrix[i][j] != 0) {
        expArray[j]++;
      }

      tMatrix[i][j] = match_k[k];
      // Are these correct interpretations??????????????????????????????????????????????????????????????????????????????????????
      kMatrix[i][j] = (cMatrix[i][j] * tMatrix[i][j]) == 1;
      fMatrix[i][j] = (cMatrix[i][j] * (1 - tMatrix[i][j])) == 1;

      trueRateArray[j] += kMatrix[i][j];
      falseRateArray[j] += fMatrix[i][j];
    }
  }

  for (int i = 0; i < num; i++) {
    trueRateArray[i] /= rows;
    falseRateArray[i] /= sArray[i];
  }


  // Copy to return values
  for (int i = 0; i < rows; i++) {

  }


}*/

//' This function will compute the identification risk for a dataset with synthetic categorical variables.
//' @param origdata dataframe of the origonal data
//' @param syndata list of the different synthetic dataframes
//' @param known vector of the names of the columns in the dataset assumed to be known
//' @param syn vector of the names of the columns in the dataset that are synthetic
//' @export
// [[Rcpp::export]]
Rcpp::List IdentificationRisk(Rcpp::NumericMatrix dataMatrix, int rows, int cols, Rcpp::List syndataMatrices,
                   int num, NumericVector knowncols, int numKnown, NumericVector syncols, int numSyn) {

  Rcpp::NumericMatrix cMatrix(rows, num);
  Rcpp::NumericMatrix tMatrix(rows, num);
  Rcpp::NumericMatrix kMatrix(rows, num);
  Rcpp::NumericMatrix fMatrix(rows, num);
  Rcpp::NumericMatrix riskMatrix(rows, num);

  NumericVector sArray(num);
  NumericVector expArray(num);
  NumericVector trueRateArray(num);
  NumericVector falseRateArray(num);

  char match_k[rows];
  Rcpp::NumericMatrix syndataMatrix;

  for (int i = 0; i < rows; i++) {
    for (int j = 0; j < num; j++) {
      // try SEXP temp = list[i] then syndata = Rcpp::NumericMatrix(temp)
      SEXP temp = syndataMatrices[j];
      syndataMatrix = Rcpp::NumericMatrix(temp);
      memset(match_k, 1, rows);

      for (int k = 0; k < rows; k++) {
        // try to match person i with every person in the synthetic set
        // by first checking if the known variables
        for (int l = 0; l < numKnown; l++) {
          if (dataMatrix(i,knowncols[l]) != syndataMatrix(k, knowncols[l])) {
            match_k[k] = 0;
            break;
          }
        }
        // then if those match check the synthetic ones
        if (match_k[k]) {
          for (int l = 0; l < numSyn; l++) {
            if (dataMatrix(i,syncols[l]) != syndataMatrix(k,syncols[l])) {
              match_k[k] = 0;
              break;
            }
          }
          if (match_k[k]) {
            cMatrix(i,j) = cMatrix(i,j) + 1;
          }
        }
      }

      if (cMatrix(i,j) == 1) {
        sArray[j] = sArray[j] + 1;
      }

      if (cMatrix(i,j) != 0) {
        tMatrix(i,j) = match_k[i];
        riskMatrix(i,j) = tMatrix(i,j) / cMatrix(i,j);
        expArray[j] += tMatrix(i, j)/cMatrix(i,j);
      } else {
        tMatrix(i,j) = 1;
        riskMatrix(i,j) = 0;
      }

      kMatrix(i,j) = (cMatrix(i,j) * tMatrix(i,j)) == 1;
      fMatrix(i,j) = (cMatrix(i,j) * (1 - tMatrix(i,j))) == 1;

      trueRateArray[j] = trueRateArray[j] + kMatrix(i,j);
      falseRateArray[j] = falseRateArray[j] + fMatrix(i,j);
    }
  }

  for (int i = 0; i < num; i++) {
    trueRateArray[i] = trueRateArray[i] / rows;
    falseRateArray[i] = falseRateArray[i] / sArray[i];
  }

  return List::create(Named("s_vector")=sArray,
                      Named("exp.risk_vector")=expArray,
                      Named("true.rate_vector")=trueRateArray,
                      Named("false.rate_vector")=falseRateArray,
                      Named("c_vector")=cMatrix,
                      Named("T_vector")=tMatrix,
                      Named("K_vector")=kMatrix,
                      Named("F_vector")=fMatrix,
                      Named("Risk_vector")=riskMatrix);
}








