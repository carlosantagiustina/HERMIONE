#' is_rstudio
#'
#' @description A function to test if the R session is running in RStudio and if yes sets the working directory to the current path of the active document
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
is.rstudio = function(){
  .Platform$GUI == "RStudio"
}
if(is.rstudio()){
  current_path = rstudioapi::getActiveDocumentContext()$path
#  setwd(dirname(current_path))
}
