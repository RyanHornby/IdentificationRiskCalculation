#' An Identification Risk Function
#'
#' This function will compute the identification risk for a dataset with synthetic categorical variables.
#' @param origdata dataframe of the origonal data
#' @param syndata list of the different synthetic dataframes
#' @param known vector of the names of the columns in the dataset assumed to be known
#' @param syn vector of the names of the columns in the dataset that are synthetic
#' @export

IdentificationRiskModified <- function(origdata, syndata, known, syn){
  origdata = origdata
  syndata = syndata
  syn.vars = syn
  known.vars = known
  #syn.vars = rep(0,length(syn))
  #known.vars = rep(0,length(known))

  # for (i in 1:ncol(origdata)) {
  #   for (j in 1:length(syn)){
  #     if (colnames(origdata)[i] == syn[j]) {
  #       syn.vars[length(syn.vars) + 1] = i
  #     }
  #   }
  #   for (k in 1:length(known)) {
  #     if (colnames(origdata)[i] == known[k]) {
  #       known.vars[length(known.vars) + 1] = i
  #     }
  #   }
  # }

  m = length(syndata) ## number of synthetic populations
  n = nrow(origdata) ## number of records / individuals

  c_vector = matrix(rep(0, n*m), ncol = m)
  T_vector = matrix(rep(0, n*m), ncol = m)

  K_vector = matrix(rep(0, n*m), ncol = m)
  F_vector = matrix(rep(0, n*m), ncol = m)

  for (i in 1:n){
    for (k in 1:m){
      syndata_k = syndata[[k]]

      match_k<-(eval(parse(text=paste("origdata$",syn.vars,"[i]==
                                          syndata_k$",syn.vars,sep="",collapse="&")))&
                  eval(parse(text=paste("origdata$",known.vars,"[i]==
                                            syndata_k$",known.vars,sep="",collapse="&"))))
      match.prob_k<-ifelse(match_k,1/sum(match_k),0)
      c_vector[i, k] = sum(match_k)

      T_vector[i, k] = is.element(i,rownames(origdata)[match.prob_k==max(match.prob_k)])

      K_vector[i, k] = (c_vector[i, k]*T_vector[i, k]==1)
      F_vector[i, k] = (c_vector[i, k]*(1 - T_vector[i, k])==1)
    }
  }

  s_vector = rep(0, m)
  exp.risk_vector = rep(0, m)
  true.rate_vector = rep(0, m)
  false.rate_vector = rep(0, m)

  for (k in 1:m){
    #### s is the number of c_i == 1, unique matches
    s_vector[k] <- length(c_vector[c_vector[, k]==1, k])
    ####expected match risk
    nonzero_c_index = which(c_vector[, k]>0)
    exp.risk_vector[k] <- sum(1/c_vector[nonzero_c_index, k]*T_vector[nonzero_c_index, k])
    ###true match rate
    true.rate_vector[k] <- sum(K_vector[, k])/n
    ###false match rate
    false.rate_vector[k] <- sum(F_vector[, k])/s_vector[k]
  }


  res_r <- list(s_vector = s_vector,
                exp.risk_vector = exp.risk_vector,
                true.rate_vector = true.rate_vector,
                false.rate_vector = false.rate_vector,
                c_vector = c_vector,
                T_vector = T_vector,
                K_vector = K_vector,
                F_vector = F_vector
  )
  return(res_r)
}
