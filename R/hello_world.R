#' Test your plotter functionality
#'
#' Try plotting a simple boxplot or sinus curve to check the plotter.
#'
#' @param what string, either boxplot or polyline
#' @param portrait should the plot be in portrait (TRUE), or landscape (FALSE)?
#'
#' @return
#' @export
hello_world <- function(what = c("boxplot", "polyline"), portrait = TRUE){
  what <- match.arg(what)
  lineus_dev(portrait = portrait)
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
