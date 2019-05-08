library(pecanapi)
import::from(magrittr, "%>%")

options(
  pecanapi.docker_host = "localhost",
  pecanapi.docker_port = 7999,
  pecanapi.docker_db_port = 7990
)

# Establish database connection
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  user = "bety",
  password = "bety",
  host = "localhost",
  port = getOption("pecanapi.docker_db_port")
)

site_id <- 1000000033
model_id <- get_model_id(con, "ED2", "develop")

workflow <- insert_new_workflow(con, site_id, model_id,
                                start_date = "2004-07-01",
                                end_date = "2004-07-31")

settings <- list() %>%
  add_workflow(workflow) %>%
  add_pft("Optics.Temperate_Early_Hardwood", num = 9) %>%
  add_pft("Optics.Temperate_Mid_Hardwood", num = 10) %>%
  add_pft("Optics.Temperate_Late_Hardwood", num = 11) %>%
  add_database() %>%
  add_rabbitmq(model_queue = "ED2_develop") %>%
  modifyList(list(
    meta.analysis = list(iter = 3000, random.effects = FALSE),
    run = list(inputs = list(met = list(source = "CRUNCEP", output = "ED2", method = "ncss"))),
    ensemble = list(size = 1, variable = "NPP")
  )) %>%
  modifyList(list(
    run = list(inputs = list(
      lu = list(id = 294),
      soil = list(id = 297),
      thsum = list(id = 295),
      veg = list(id = 296)
    )),
    model = list(
      phenol.scheme = 0,
      edin = "ED2IN.rgit",
      prerun = "ulimit -s unlimited",
      barebones_ed2in = "true",
      ed2in_tags = list(
        IOOUTPUT = 0,
        PLANT_HYDRO_SCHEME = 0,
        ISTOMATA_SCHEME = 0,
        ISTRUCT_GROWTH_SCHEME = 0,
        TRAIT_PLASTICITY_SCHEME = 0
      )
    )
  ))

submit_workflow(settings)
watch_workflow(workflow[["id"]])

if (FALSE) {

  # Follow logfile.txt
  while (TRUE) {
    readLines(run_url(workflow[["id"]], "logfile.txt")) %>% tail(2) %>% writeLines()
    writeLines("---------------")
    Sys.sleep(2)
  }

  # Inspect complete workflow
  readLines(output_url(workflow[["id"]], "workflow.Rout")) %>% writeLines()
  readLines(output_url(workflow[["id"]], "pecan.xml")) %>% writeLines()

  # Inspect complete logfile
  readLines(run_url(workflow[["id"]], "logfile.txt")) %>% writeLines()

  # Inspect ED2IN
  run_id <- list_runs(con, workflow[["id"]])[["id"]]
  readLines(output_url(workflow[["id"]], file.path("run", run_id, "ED2IN"))) %>% writeLines()
}

nc <- ncdf4::nc_open(run_dap(workflow[["id"]], "2004.nc"))
if (interactive()) time <- ncdf4::ncvar_get(nc, "time")
npp <- ncdf4::ncvar_get(nc, "NPP")
ncdf4::nc_close(nc)
if (interactive()) plot(npp ~ time, type = "l")
message("Summary of NPP output:")
summary(npp)
stopifnot(any(npp > 0))
