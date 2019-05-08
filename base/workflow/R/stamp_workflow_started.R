#' Timestamp the start time of a workflow
#'
#' @param workflow_id ID of workflow to mark as started
#' @param con Database connection object. See [PEcAn.DB::db.open()].
#' @param overwrite Logical. If `FALSE` (default), only timestamp if
#'   there is no existing time stamp. If `TRUE`, ignore the existing
#'   timestamp.
#' @return (Invisibly) `data.frame` of corresponding row from
#'   `workflows` table
#' @author Alexey Shiklomanov
#' @export
stamp_workflow_started <- function(workflow_id, con, overwrite = FALSE) {
  ## current_workflow <- PEcAn.DB::db.query(
  ##   "SELECT * FROM workflows WHERE id = $1"
  ## )
  ## result <- PEcAn.DB::db.query(
  ##   ""
  ## )
  message("Not really stamping the workflow...")
}
