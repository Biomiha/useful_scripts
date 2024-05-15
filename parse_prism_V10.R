pzfx_tables <- function(path) {
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

read_pzfx <- function(path, table, use_colnames = FALSE, ...) {
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
  
  extract_replicates <- function(set_json) {
    suppressWarnings(set_json$`replicate ranges`$range |> as.numeric() |> max(na.rm = TRUE)) + 1
  }
  
  y_colnames <- lapply(yDataSets, search_y) |> 
    lapply(FUN = fromJSON) |> 
    lapply(FUN = extract_title) |> 
    unlist()
  
  has_replicates <- lapply(yDataSets, search_y) |> 
    lapply(FUN = fromJSON) |> 
    lapply(function(x) {x$format}) |> 
    unlist()
  
  y_replicates <- lapply(yDataSets, search_y) |> 
    lapply(FUN = fromJSON) |> 
    lapply(FUN = extract_replicates) |> 
    unlist()
  
  if(any(has_replicates %in% "y_replicates")) {
    y_colnames <- make.unique(rep(y_colnames, y_replicates))
    }
  
  table_data <- list.files(temp_dir, recursive = TRUE, full.names = TRUE) |> 
    grep(pattern = "data", value = TRUE) |> 
    grep(pattern = "tables", value = TRUE) |> 
    grep(pattern = table_id, value = TRUE) |> 
    grep(pattern = "data.csv", value = TRUE) |> 
    vroom(col_names = FALSE, ...)
  if(use_colnames) {
    colnames(table_data) <- c(x_colname, y_colnames)
  }
  return(table_data)
}