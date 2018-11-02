db_query <- function(con, template, params) {
  need_rpostgres(con)
  query <- DBI::dbSendQuery(con, template)
  DBI::dbBind(query, params)
  DBI::dbFetch(query)
}

need_rpostgres <- function(con) {
  if (class(con) != "PqConnection") {
    PEcAn.logger::logger.severe(
      "This function only supports connections made with RPostgres package. ",
      "The older RPostgreSQL is not supported."
    )
  }
  invisible(TRUE)
}
