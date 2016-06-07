#' Runs shiny example
#' 
#' @param appName App name
#' @export
runExample <- function(appName = c("testApp", "regression1")) {
  appName <- appName[1]
  appDir <- system.file("shiny-examples", appName, package = "introStatML")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}