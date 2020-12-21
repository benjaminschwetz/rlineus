LineUsPlotter <- R6::R6Class("LineUsPlotter",
                             public = list(
                               initialize = function(url = "ws://line-us.local",
                                                     portrait = FALSE){
                                 private$portrait <- portrait
                                 private$websocket <- websocket::WebSocket$new(url)
                                 private$websocket$onOpen(function(event) {
                                 })
                                 private$websocket$onMessage(function(event) {
                                   self$catch_message(event$data)
                                 })
                                 private$websocket$onClose(function(event) {
                                   cat("Plotter disconnected")
                                 })
                                 private$websocket$onError(function(event) {
                                   cat("Client failed to connect: ", event$message, "\n")
                                 })
                                 self$retrieve_message()
                                 self$retrieve_message()
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
                               g01 = function(x=NULL,y=NULL, z=NULL) {
                                 if(private$portrait) {
                                   x_draw <- x
                                   y_draw <- y
                                 } else {
                                   x_draw <- y
                                   y_draw <- x
                                 }
                                 if(!is.null(x_draw)) {
                                   if(x_draw < 650) x_draw <- 650 else if(x_draw>1775) x_draw <- 1750
                                 }
                                 if(!is.null(y_draw)) {
                                   if(y_draw < -1000) y_draw <- -1000 else if(y_draw>1000) y_draw <- 1000
                                 }
                                 if(!is.null(z)) {
                                   if(z < 0) z <- 0 else if(z>2000) z <- 2000
                                 }
                                 vec <- unlist(list(
                                   X = x_draw,
                                   Y = y_draw,
                                   Z = z
                                 ))
                                 string <- paste0(names(vec), vec, collapse = " ")
                                 if(!self$connected()) Sys.sleep(.5);later::run_now()
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
                               },
                               disconnect = function() {
                                 private$websocket$close()
                               },
                               connected = function() {
                                 private$websocket$readyState() == 1
                               }
                             ),
                             private = list(
                               websocket = NULL,
                               pending_message = NULL,
                               portrait = NULL
                             )
)

