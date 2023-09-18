#' Andrew Custom Package
#'
#' Provides a convenient function to view tables quickly in excel
#'
#' @export
ViewExcel <- function(df = .Last.value, file = tempfile(fileext = ".csv")) {
  df <- try(as.data.frame(df))
  stopifnot(is.data.frame(df))
  utils::write.csv(df, file = file)
  base::shell.exec(file)
}
