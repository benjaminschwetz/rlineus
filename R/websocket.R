LineUsPlotter <- R6::R6Class("LineUs Plotter",
                             inherit = websocket::WebSocket,
                             public = list(
                               g01 = function(x=NULL,y=NULL, z=NULL) {
                                  vec <- unlist(list(
                                   X = x,
                                   Y = y,
                                   Z = z
                                 ))
                                 string <- paste0(names(vec), vec, collapse = " ")
                                 browser()
                                 super$send(paste("G01", string))
                               }
                             )
)
load_plotter <- function(url = "ws://line-us.local"){
  lineus <- LineUsPlotter$new(url)
  lineus$onOpen(function(event) {
    cat("Connection opened\n")
  })
  lineus$onMessage(function(event) {
    cat(event$data)
  })
  lineus$onClose(function(event) {
    cat("Plotter disconnected")
  })
  lineus$onError(function(event) {
    cat("Client failed to connect: ", event$message, "\n")
  })
  return(lineus)
}
