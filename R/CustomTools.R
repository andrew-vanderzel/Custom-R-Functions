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
  library(dplyr)
  library(janitor)
  data %>%
    tabyl({{vbl}}) %>%
    adorn_pct_formatting() %>%
    arrange(desc(n))
}

#' Function to import github package with updated download setting
#' @export
import_github <- function(path){
  library(devtools)
  options(download.file.method = "wininet")
  devtools::install_github(path)
}

#' Function to provide flexibility in import excel files
#' @export
import_excel <- function(path, password = "NONE", sheet = "NONE", skip = 0){

  if(password != "NONE"){

    if(sheet != "NONE"){
      f <- excel.link::xl.read.file(path, password = password, xl.sheet = sheet)
      return(f)
    }else{
      f <- excel.link::xl.read.file(path, password = password)
      return(f)
    }

  }else{
    if(sheet != "NONE"){
      f <- readxl::read_excel(path, sheet=sheet, skip = skip)
      return(f)
    }else{
      f <- readxl::read_excel(path, skip = skip)
      return(f)
    }
  }
}
