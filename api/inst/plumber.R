#' @get /test
function() {
  list(
    success = "Plumber API is running successfully",
    session_info = sessionInfo()
  )
}

#' @param msg "Message to return"
#' @get /echo
#' @html
function(msg) {
  sprintf("<html>You have entered the following: %s </html>", msg)
}
