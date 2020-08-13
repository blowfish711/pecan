stopifnot(
  requireNamespace("XML", quietly = TRUE)
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
  xml_string <- paste0(
    '<?xml version="1.0"?>\n',
    toString(listToXml(settings, "pecan"))
  )
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
