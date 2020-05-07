#' Check if Line-us python library is installed
#'
#' @return Bolean if the python package is available or not.
#' @export
check_py_setup <- function() {
  if(!reticulate::py_available(initialize = TRUE)) stop("Python not configured.")
  reticulate::py_module_available("lineus")
}
#' Install line-us package
#'
#' @return nothing.
#' @export
#'
#' @examples
install_py_lineus <- function() {
  if(!check_py_setup()){
    reticulate::conda_install(packages = "lineus", pip = TRUE)
  }  else {
    message("lineus package already installed.")
  }
  return(invisible())
}
import_plotter <- function(delay = 2L) {
  py_lineus <- reticulate::import("lineus")
  plotter <- py_lineus$LineUs()
  connection <- plotter$connect(wait = delay)
  if(connection) {
    message("Connected")
  } else {
    stop("Cannot connect to plotter.")
  }
  return(plotter)
}
load_plotter <- function() {
  py_lineus <- reticulate::import("lineus")
  plotter <- py_lineus$LineUs()
  return(plotter)
}
render_canvas <- function( plotter,
                           x_lim = c(650,1775),
                           y_lim = c(-1000, 1000),
                           keep_con = FALSE
){
  if(!plotter$connected()) plotter$connect()
  z <- fetch_z_map(plotter)
  out <- interpolate_z_matrix(x_lim, y_lim, z)
  if(!keep_con) plotter$disconnect()
  return(out)
}




