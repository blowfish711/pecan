library(helpecan)
library(pecanapi)

# Establish database connection
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  user = "bety",
  password = "bety",
  host = "localhost",
  port = getOption("pecanapi.docker_db_port")
)

docker_machine_id <- con %>%
  dplyr::tbl("machines") %>%
  dplyr::filter(hostname == "docker") %>%
  dplyr::pull(id)

s <- PEcAn.settings::read.settings("~/Projects/pecan_project/pecan/tests/pecan.biocro.xml")

con %>%
  dplyr::tbl("dbfiles") %>%
  dplyr::filter(machine_id == docker_machine_id)


con %>%
  dplyr::tbl("inputs") %>%
  dplyr::filter(parent_id == 1000000705) %>%
  dplyr::select(name, start_date, end_date, id)

iid <- 99000000000

for (x in iid + c(2, 3, 4, 6, 8)) {
  purge_input(con, x)
}



id <- con %>%
  dplyr::tbl("dbfiles") %>%
  dplyr::filter(
    machine_id == docker_machine_id,
    file_path %like% "%CRUNCEP%SIPNET%",
    file_name %like% "%2004%"
  ) %>%
  dplyr::pull(id)

con %>%
  dplyr::tbl("inputs") %>%
  dplyr::filter(site_id == !!site_id, name %like% "CRUNCEP_site%") %>%
  dplyr::select(id, name, start_date, end_date)


param_statement(con, (
  "DELETE FROM inputs WHERE id = $1"
), list(99000000007))


sipnet_in <- dbfile_url("CRUNCEP_SIPNET_site_1-33/CRUNCEP.2004-01-01.2004-12-31.clim") %>%
  readLines()

head(sipnet_in)
