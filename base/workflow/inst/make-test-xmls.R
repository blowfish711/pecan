stopifnot(
  requireNamespace("XML", quietly = TRUE),
  requireNamespace("httr", quietly = TRUE)
)

listToXml <- function(item, tag) {
  if (typeof(item) != "list") {
    if (length(item) > 1) {
      xml <- XML::xmlNode(tag)
      for (name in names(item)) {
        XML::xmlAttrs(xml)[[name]] <- item[[name]]
      }
      return(xml)
    } else {
      return(XML::xmlNode(tag, item))
    }
  }
  if (identical(names(item), c("text", ".attrs"))) {
    xml <- XML::xmlNode(tag, item[["text"]])
  } else {
    xml <- XML::xmlNode(tag)
    for (i in seq_along(item)) {
      if (is.null(names(item)) || names(item)[i] != ".attrs") {
        xml <- XML::append.xmlNode(xml, listToXml(item[[i]], names(item)[i]))
      }
    }
  }
  attrs <- item[[".attrs"]]
  for (name in names(attrs)) {
    XML::xmlAttrs(xml)[[name]] <- attrs[[name]]
  }
  return(xml)
}

infile <- commandArgs(trailingOnly = TRUE)[1]
dat <- read.csv(infile, stringsAsFactors = FALSE)

dat2 <- dat
dat2$workflow_id <- NA_character_

for (i in seq_len(nrow(dat))) {
  pftlist <- lapply(
    strsplit(dat[i, "pft"], "\\|")[[1]],
    function(x) list(pft = list(name = x))
  )

  # Get the model ID. Currently, this doesn't work correctly in PEcAn itself.
  ## model_raw <- httr::GET(
  ##   "http://localhost:8000/api/models/",
  ##   httr::authenticate("carya", "illinois"),
  ##   query = list(model_name = dat[i, "model"],
  ##                revision = dat[i, "revision"])
  ## )
  ## model_info <- jsonlite::fromJSON(httr::content(model_id, "text"))
  ## model_id <- subset(model_info$models,
  ##                    model_name == dat[i, "model"] &
  ##                      revision == dat[i, "revision"])[["model_id"]]
  settings <- list(
    pfts = do.call(c, pftlist),
    meta.analysis = list(
      iter = 3000,
      random.effects = "FALSE",
      threshold = 1.2,
      update = "AUTO"
    ),
    model = list(type = dat[i, "model"],
                 revision = dat[i, "revision"]),
    ensemble = list(variable = "NPP",
                    size = dat[i, "ensemble_size"]),
    run = list(
      site = list(id = dat[i, "site_id"],
                  met.start = dat[i, "start_date"],
                  met.end = dat[i, "end_date"]),
      inputs = list(met = list(source = dat[i, "met"])),
      start.date = dat[i, "start_date"],
      end.date = dat[i, "end_date"],
      dbfiles = "pecan/dbfiles"
    )
  )
  if (dat[i, "sensitivity"]) {
    settings$sensitivity.analysis <- list(
      quantiles = list(sigma = -1, sigma = 1),
      variable = "NPP"
    )
  }
  if (dat[i, "model"] == "ED2") {
    settings$run$inputs <- modifyList(settings$run$inputs, list(
      lu = list(id = 294),
      thsum = list(id = 295),
      veg = list(id = 296),
      soil = list(id = 297)
    ))
    settings$model <- modifyList(settings$model, list(
      phenol.scheme = 0,
      edin = "ED2IN.r2.2.0",  # HACK: Shouldn't be hard-coded
      config.header = list(ed_misc = list(output_month = 12))
    ))
  }
  xml_string <- paste0(
    '<?xml version="1.0"?>\n',
    toString(listToXml(settings, "pecan"))
  )
  dir.create(file.path("tests", "api"), showWarnings = FALSE)
  write(xml_string, sprintf("tests/api/test_%03d.xml", i))
  # Submit run via API
  ## res <- httr::POST(
  ##   "http://localhost:8000/api/workflows/",
  ##   httr::authenticate("ashiklom", "admin"),
  ##   httr::content_type("application/xml"),
  ##   body = xml_string
  ## )
  ## cont <- httr::content(res)
  ## if ("error" %in% names(cont)) {
  ##   warning(
  ##     "Submission failed with the following error:\n",
  ##     "error: ", cont$error, "\n",
  ##     "message: ", cont$message
  ##   )
  ## } else {
  ##   dat2[i, "workflow_id"] <- cont$workflow_id
  ## }
}
## write.csv(dat2, stdout())
