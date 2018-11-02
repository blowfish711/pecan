insert_model <- function(con, model_name, revision, model_type,
                         binary_path = NULL,
                         binary_machine_id = NULL,
                         new_model_type = FALSE) {
  # Check if model ID already exists
  model_id <- get_model_id(con, model_name, model_type, revision)
  if (length(model_id) > 0) {
    PEcAn.logger::logger.info(glue::glue(
      "Model already exists with id = {model_id}"
    ))
    return(invisible(model_id))
  }

  modeltype_id <- get_modeltype_id(con, model_type)
  if (!length(modeltype_id) > 0) {
    if (!new_model_type) {
      PEcAn.logger::logger.severe(glue::glue(
        "Model type {model_type} does not exist and `new_model_type` is FALSE."
      ))
    }
    PEcAn.logger::logger.info(glue::glue(
      "Model type {model_type} does not exist. Adding as new model type."
    ))
    modeltype_id <- insert_modeltype(con, model_type)
  }

  db_query(
    con,
    paste(
      "INSERT INTO models (model_name, revision, created_at, updated_at, modeltype_id)",
      "VALUES ($1, $2, NOW(), NOW(), $3)"
    ),
    list(model_name, revision, modeltype_id)
  )

  model_id <- get_model_id(con, model_name, model_type, revision)
  if (!is.null(binary_path) && !is.null(binary_machine_id)) {
    # Register model binary in dbfiles
    insert_dbfile(file_path, machine_id, container_id, container_type)
  }
  return(model_id)
}

insert_dbfile <- function(file_path, machine_id, container_id, container_type) {
  id <- get_dbfile_id(file_path, machine_id, container_id, container_type)
  if (length(id) > 0) {
    PEcAn.logger::logger.warn(glue::glue(
      "File already registered with id {id}."
    ))
    return(id)
  }
  db_query(
    con,
    paste(
      "INSERT INTO dbfiles",
      "(file_name, file_path, machine_id, created_at, updated_at, container_id, container_type)",
      "VALUES ($1, $2, $3, NOW(), NOW(), $4, 'Model')"
    ),
    list(basename(file_path), dirname(file_path), binary_machine_id, model_id)
  )
  get_dbfile_id(file_path, machine_id, container_id, container_type)
}

get_dbfile_id <- function(file_path, machine_id, container_id, container_type) {
  dplyr::tbl(con, "dbfiles") %>%
    dplyr::filter(
      file_name == !!basename(file_path),
      file_path == !!dirname(file_path),
      machine_id == !!machine_id,
      container_id == !!container_id,
      container_type == !!container_type
    ) %>%
    dplyr::pull(id)
}

get_model_id <- function(con, model_name, model_type, revision) {
  dplyr::tbl(con, "models") %>%
    dplyr::filter(
      model_name == !!model_name,
      revision == !!revision,
      model_type == !!model_type
    ) %>%
    dplyr::pull(id)
}

get_modeltype_id <- function(con, model_type) {
  dplyr::tbl(con, "modeltypes") %>%
    dplyr::filter(name == !!model_type) %>%
    dplyr::pull(id)
}

insert_modeltype <- function(con, model_type) {
  db_query(
    con,
    paste(
      "INSERT INTO modeltypes (name, created_at, updated_at)",
      "VALUES ($1, NOW(), NOW())"
    ),
    list(model_type)
  )
  get_modeltype_id(con, model_type)
}
