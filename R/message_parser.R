#' @title parser for zmap response
#'
#' @description converts response from plotter
#'
#' @param string
#'
#' @return 2x2 matrix
#' @export
#'
#' @examples
parse_zmap <- function(string){
  if(substr(string,1,2)!="ok") stop("Error receiving zmap")
  numbers <- substr(string,10,nchar(string)-1)
  v_z_map <- strsplit(numbers, ";")[[1]]
  num_z_map <- as.numeric(v_z_map)
  matrix(num_z_map, nrow = 2)
}
