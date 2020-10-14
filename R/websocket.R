load_plotter <- function(url = "ws://line-us.local"){
  lineus <- websocket::WebSocket$new(url)
  lineus$onOpen(function(event) {
    cat("Connection opened\n")
  })
  lineus$onMessage(function(event) {
    cat("Client got msg: ", event$data, "\n")
  })
  lineus$onClose(function(event) {
    cat("Client disconnected with code ", event$code,
        " and reason ", event$reason, "\n", sep = "")
  })
  lineus$onError(function(event) {
    cat("Client failed to connect: ", event$message, "\n")
  })
  return(lineus)
}
