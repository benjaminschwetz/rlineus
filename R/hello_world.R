#' Testing the plotter
#'
#' @param what string, either boxplot or polyline
#'
#' @return
#' @export
hello_world <- function(what = c("boxplot", "polyline")){
  what <- match.arg(what)
  lineus_dev()
  switch(
    what,
    "boxplot" =   boxplot(data = iris, Sepal.Length ~Species),
    "polyline" = {
      x <- seq(0,8*pi,length.out=100)
      y <- sin(x)
      plot(x,y,type="l")
    }
  )
  dev.off()
}
