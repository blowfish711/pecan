# plumber.R

#' Another test the plumber api
#' @get /test
#' @html
function() {
  "<html>The API is working</html>"
}

#' Test the plumber api
#' @get /test2
function() {
  list(
    success = "Plumber API is running successfully"
  )
}

#' Return a message
#' @param msg "Message to return"
#' @get /echo
#' @html
function(msg) {
  sprintf("<html>You have entered the following: %s </html>", msg)
}
