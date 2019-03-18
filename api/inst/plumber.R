# plumber.R

# Setup
connect <- function() {
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    user = "bety",
    password = "bety",
    host = "postgres"
  )
}

#' Another test the plumber api
#' @get /test
#' @html
function() {
  "<html>The API is working</html>"
}

#' List tables
#' @get /tables
function() {
  DBI::dbListTables(connect())
}

#' Return a list of models
#' @get /models
function(name = "", revision = "", modeltype = "") {
  pecanapi::search_models(connect(), name, revision, modeltype)
}
