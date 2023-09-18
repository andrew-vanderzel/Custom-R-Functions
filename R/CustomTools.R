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

#' Provides a janitor distribution table but with formatting and ordering by default
#' @export
f_tbyl <- function(data, vbl){
  data %>%
    tabyl({{vbl}}) %>%
    adorn_pct_formatting() %>%
    arrange(desc(n))
}
