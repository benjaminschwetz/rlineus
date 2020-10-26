LineUsPlotter <- R6::R6Class("LineUsPlotter",
                             public = list(
                               initialize = function(url = "ws://line-us.local",
                                                     autoConnect = FALSE){
                                 private$websocket <- websocket::WebSocket$new(url,
                                                                               autoConnect)
                                 private$websocket$onOpen(function(event) {
                                   cat("Connection opened\n")
                                 })
                                 private$websocket$onMessage(function(event) {
                                   self$catch_message(event$data)
                                 })
                                 private$websocket$onClose(function(event) {
                                   self$clear_log()
                                   cat("Plotter disconnected")
                                 })
                                 private$websocket$onError(function(event) {
                                   cat("Client failed to connect: ", event$message, "\n")
                                 })
                               },
                               catch_message = function(message){
                                 private$pending_message <- message
                               },
                               retrieve_message = function(timeout=Inf){
                                 later::run_now(timeoutSecs = timeout)
                                 message <- private$pending_message
                                 private$pending_message <- NULL
                                 return(message)
                               },
                               g01 = function(x=NULL,y=NULL, z=NULL, async = TRUE) {
                                 vec <- unlist(list(
                                   X = x,
                                   Y = y,
                                   Z = z
                                 ))
                                 string <- paste0(names(vec), vec, collapse = " ")
                                 private$websocket$send(paste("G01", string))
                                 out <- self$retrieve_message()
                                 return(out)
                               },
                               zmap = function(timeout = Inf){
                                 private$websocket$send("G31")
                                 response <- self$retrieve_message(timeout)
                                 parse_zmap(response)
                               },
                               clear_log = function(){
                                 private$log <- NULL
                               },
                               connect = function() {
                                 private$websocket$connect()
                               }
                             ),
                             private = list(
                               websocket = NULL,
                               pending_message = NULL
                             )
)

