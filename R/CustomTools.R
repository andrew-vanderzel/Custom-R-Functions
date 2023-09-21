#' Andrew Custom Package
#'
#' Provides a convenient function to view tables quickly in excel
#'
#' @export
ViewExcel <- function(df = .Last.value, file = tempfile(fileext = ".csv")) {
  df <- try(as.data.frame(df))
  stopifnot(is.data.frame(df))
  rio::export(df, file = file)
  base::shell.exec(file)
}

#' Provides a janitor distribution table but with formatting and ordering by default
#' @export
f_tbyl <- function(data, vbl) {
  library(dplyr)
  library(janitor)
  library(magrittr)
  data %>%
    janitor::tabyl({{ vbl }}) %>%
    janitor::adorn_pct_formatting() %>%
    dplyr::arrange(dplyr::desc(n))
}

#' Function to import github package with updated download setting
#' @export
import_github <- function(path) {
  library(devtools)
  options(download.file.method = "wininet")
  devtools::install_github(path)
}

#' Function to provide flexibility in import excel files
#' @export
import_excel <- function(path, password = "NONE", sheet = "NONE", skip = 0) {
  library(dplyr)
  library(excel.link)
  library(readxl)
  if (password != "NONE") {
    if (sheet != "NONE") {
      f <- excel.link::xl.read.file(path, password = password, xl.sheet = sheet)
      return(f)
    } else {
      f <- excel.link::xl.read.file(path, password = password)
      return(f)
    }
  } else {
    if (sheet != "NONE") {
      f <- readxl::read_excel(path, sheet = sheet, skip = skip)
      return(f)
    } else {
      f <- readxl::read_excel(path, skip = skip)
      return(f)
    }
  }
}

#' Provides a light coherent color palette. The provided colors are: lime, mint, teal, blue, purple, magenta, pink, red, orange, and yellow
#' @export
light_palatte <- function(color = NULL) {
  palatte <- c(
    lime = "#BAE86F",
    mint = "#6FE8AA",
    teal = "#6FD8E8",
    blue = "##6FB8E8",
    purple = "#B06FE8",
    magenta = "#E26FE8",
    pink = "#E86FAA",
    red = "#E86F6F",
    orange = "#E8B86F",
    yellow = "#E8DA6F"
  )

  if (is.null(color)) {
    print("Returning All Colors")
    return(palatte)
  }else{
    print("Returning Select Colors")
    return(palatte[color])
  }
}
