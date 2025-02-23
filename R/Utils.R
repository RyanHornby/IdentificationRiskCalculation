#' Convert dataframe to matrix while preserving factor level values
#' @param df a dataframe
#' @noRd
dfToMatrix = function(df) {
  return(lapply(df, as.character) |> 
    lapply(as.numeric) |> 
    as.data.frame() |> 
    as.matrix())
}