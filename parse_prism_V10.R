prism_tables <- function(path) {
  require(jsonlite)
  getExtension <- function(file){ 
    ex <- strsplit(basename(file), split="\\.")[[1]]
    return(ex[-1])
  }
  stopifnot(getExtension(path) == "prism")
  temp_dir <- tempdir()
  unzip(zipfile = path, exdir = paste0(temp_dir, "/prism_unzip"))
  top_level_json <- fromJSON(paste0(temp_dir, "/prism_unzip/document.json"))
  sheet_ids <- top_level_json$sheets$data
  sheet_names <- unlist(top_level_json$sheetAttributesMap[sheet_ids])
  return(as.vector(sheet_names))
}

extract_y_colnames <- function(path, table = 1) {
  require(jsonlite)
  require(vroom)
  getExtension <- function(file){ 
    ex <- strsplit(basename(file), split="\\.")[[1]]
    return(ex[-1])
  }
  stopifnot(getExtension(path) == "prism")
  temp_dir <- tempdir()
  unzip(zipfile = path, exdir = paste0(temp_dir, "/prism_unzip"))
  top_level_json <- fromJSON(paste0(temp_dir, "/prism_unzip/document.json"))
  sheet_ids <- top_level_json$sheets$data
  sheet_names <- unlist(top_level_json$sheetAttributesMap[sheet_ids])
  if(is.numeric(table)) table <- unname(sheet_names[table])
  stopifnot(table %in% sheet_names)
  table_sheet_id <- names(top_level_json$sheetAttributesMap[sheet_ids])[sheet_names %in% table]
  sheet_json <- list.files(temp_dir, recursive = TRUE, full.names = TRUE) |> 
    grep(pattern = "data", value = TRUE) |> 
    grep(pattern = "sheets", value = TRUE) |> 
    grep(pattern = table_sheet_id, value = TRUE) |> 
    grep(pattern = "sheet.json", value = TRUE) |> 
    fromJSON()
  
  stopifnot(identical(sheet_json$title, table))
  yDataSets <- sheet_json$table$dataSets
  search_y <- function(x) {
    list.files(temp_dir, recursive = TRUE, full.names = TRUE) |> 
      grep(pattern = "data", value = TRUE) |> 
      grep(pattern = "sets", value = TRUE) |> 
      grep(pattern = x, value = TRUE)
  }
  extract_title <- function(set_json) {
    if("string" %in% names(set_json$title)) {
      set_json$title$string  
    } else set_json$title
  }
  
  y_colnames <- lapply(yDataSets, search_y) |> 
    lapply(FUN = fromJSON) |> 
    lapply(FUN = extract_title) |> 
    unlist()
  
  y_has_replicates <- sheet_json$table$dataFormat == "y_replicates"
  
  y_replicates <- sheet_json$table$replicatesCount
  
  if(y_has_replicates) {
    y_colnames <- make.unique(rep(y_colnames, each = y_replicates))
  }
  return(y_colnames)
}

read_prism <- function(path, table = 1, use_colnames = FALSE, ...) {
  require(jsonlite)
  require(vroom)
  getExtension <- function(file){ 
    ex <- strsplit(basename(file), split="\\.")[[1]]
    return(ex[-1])
  }
  stopifnot(getExtension(path) == "prism")
  temp_dir <- tempdir()
  unzip(zipfile = path, exdir = paste0(temp_dir, "/prism_unzip"))
  top_level_json <- fromJSON(paste0(temp_dir, "/prism_unzip/document.json"))
  sheet_ids <- top_level_json$sheets$data
  sheet_names <- unlist(top_level_json$sheetAttributesMap[sheet_ids])
  if(is.numeric(table)) table <- unname(sheet_names[table])
  stopifnot(table %in% sheet_names)
  table_sheet_id <- names(top_level_json$sheetAttributesMap[sheet_ids])[sheet_names %in% table]
  sheet_json <- list.files(temp_dir, recursive = TRUE, full.names = TRUE) |> 
    grep(pattern = "data", value = TRUE) |> 
    grep(pattern = "sheets", value = TRUE) |> 
    grep(pattern = table_sheet_id, value = TRUE) |> 
    grep(pattern = "sheet.json", value = TRUE) |> 
    fromJSON()
  
  stopifnot(identical(sheet_json$title, table))
  table_id <- sheet_json$table$uid
  xDataSet <- sheet_json$table$xDataSet
  x_colname <- list.files(temp_dir, recursive = TRUE, full.names = TRUE) |> 
    grep(pattern = "data", value = TRUE) |> 
    grep(pattern = "sets", value = TRUE) |> 
    grep(pattern = xDataSet, value = TRUE) |> 
    fromJSON() |> 
    _$title
  
  yDataSets <- sheet_json$table$dataSets
  search_y <- function(x) {
    list.files(temp_dir, recursive = TRUE, full.names = TRUE) |> 
      grep(pattern = "data", value = TRUE) |> 
      grep(pattern = "sets", value = TRUE) |> 
      grep(pattern = x, value = TRUE)
  }
  extract_title <- function(set_json) {
    if("string" %in% names(set_json$title)) {
      set_json$title$string  
    } else set_json$title
  }
  
  y_colnames <- lapply(yDataSets, search_y) |> 
    lapply(FUN = fromJSON) |> 
    lapply(FUN = extract_title) |> 
    unlist()
  
  y_has_replicates <- sheet_json$table$dataFormat == "y_replicates"
  
  y_replicates <- sheet_json$table$replicatesCount
  
  if(y_has_replicates) {
    y_colnames <- make.unique(rep(y_colnames, each = y_replicates))
    }
  
  table_data <- list.files(temp_dir, recursive = TRUE, full.names = TRUE) |> 
    grep(pattern = "data", value = TRUE) |> 
    grep(pattern = "tables", value = TRUE) |> 
    grep(pattern = table_id, value = TRUE) |> 
    grep(pattern = "data.csv", value = TRUE) |> 
    vroom(col_names = FALSE, ...)
  
  dims_json <- list.files(temp_dir, recursive = TRUE, full.names = TRUE) |> 
    grep(pattern = "data", value = TRUE) |> 
    grep(pattern = "tables", value = TRUE) |> 
    grep(pattern = table_id, value = TRUE) |> 
    grep(pattern = "content.json", value = TRUE) |> 
    fromJSON()
  
  dims_json <- c(dims_json$numberOfRows, dims_json$numberOfColumns)
  stopifnot(identical(dim(table_data), dims_json))
  
  
  if(use_colnames) {
    new_colnames <- c(x_colname, y_colnames)
    colnames(table_data) <- c(setdiff(colnames(table_data), tail(colnames(table_data), n = length(new_colnames))), new_colnames)
  }
  return(table_data)
}
