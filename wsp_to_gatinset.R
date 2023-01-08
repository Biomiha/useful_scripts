wsp_to_gatingset <- function(wsp_file) {
  require(cytoverse)
  require(tidyverse)
  require(cytoqc)
  ws <- open_flowjo_xml(file = wsp_file, sample_names_from = "sampleNode")
  ws_output <- capture.output(ws) # an extremely hacky way of capturing the relevant details of the workspace file including the file location

  # Extract the parent directory:
  temp_var <- ws_output |>
    str_detect(pattern = "File location")

  Parent_dir <- ws_output[temp_var] |>
    str_remove(pattern = "File location:  ") |>
    dirname()

  Patient_id <- Parent_dir |> str_extract(pattern = "[A-Z][A-Z][0-9][0-9][0-9]-[A-Z][A-Z]-[0-9][0-9]-[0-9][0-9][0-9]")
  # List fcs files in the folder. Additional regex can be used to only read in the relevant .fcs files
  rawfiles <- list.files(Parent_dir, ".fcs$", full.names = TRUE) %>%
    grep(pattern = "QC", value = TRUE)

  # The new container class is now called cytoset. You load the cytoset into R from the list of .fcs files
  cs <- try(load_cytoset_from_fcs(rawfiles))
  if (inherits(cs, "try-error")) {
    # still need to add code to change offending wsp file
    cqc_data <- cqc_load_fcs(rawfiles)
    check_res <- cqc_check(cqc_data, type = "channel")
    groups_tbl <- as_tibble(check_res) |>
      distinct(group_id, nObject)
    ref_group <- groups_tbl |>
      dplyr::filter(nObject == max(nObject)) |>
      pull(group_id)

    match_res <- cqc_match(check_res, ref = ref_group)
    match_res_tbl <- print(match_res) |>
      as.data.frame() |>
      as_tibble(.name_repair = "unique") |>
      set_names(c("...1", "Ref", "Other"))

    value_to_replace <- match_res_tbl |>
      dplyr::filter(Ref == "") |>
      pull(Other)

    value_to_replace_with <- match_res_tbl |>
      dplyr::filter(is.na(Other)) |>
      pull(Ref)

    string_to_replace <- paste0("data-type:name=", '"', value_to_replace, '"')
    string_to_replace_with <- paste0("data-type:name=", '"', value_to_replace_with, '"')

    wsp_file_replaced <- read_lines(wsp_file) |>
      str_replace_all(pattern = string_to_replace, replacement = string_to_replace_with)
    wsp_file_replaced_name <- tempfile(fileext = ".wsp")

    # This will write a new wsp file in order to not replace the old wsp
    write_lines(x = wsp_file_replaced, file = wsp_file_replaced_name)
    ws <- open_flowjo_xml(file = wsp_file_replaced_name, sample_names_from = "sampleNode")

    ff_list <- map(.x = seq_along(rawfiles), .f = \(.number) read.FCS(rawfiles[[.number]]))
    cs <- map(.x = seq_along(ff_list), .f = \(.number) `colnames<-`(ff_list[[.number]], colnames(ff_list[[1]]))) |>
      flowSet() |>
      flowSet_to_cytoset()
  }

  colnames(cs) <- gsub(pattern = "/", replacement = "_", colnames(cs))

  gs_QC <- flowjo_to_gatingset(ws = ws, name = "Samples", cytoset = cs, skip_faulty_gate = TRUE)

  # Extract transformations
  trans <- cyto_transformer_extract(gs_QC)

  pData(gs_QC) <- pData(gs_QC) |>
    dplyr::mutate(
      cond = str_extract(string = name, pattern = "(?<=[0-9][0-9]-[0-9][0-9][0-9]_)[A-Z]+"),
      timepoint = str_extract(string = name, pattern = "[0-9]+h"),
      cond = case_when(
        cond == "MP" ~ "CMP",
        TRUE ~ cond
      ),
      cond = fct_relevel(cond, c("DMSO", "CMP", "CSC")),
      timepoint = fct_relevel(timepoint, c("3h", "16h")),
      control = if_else(condition = cond == "DMSO", true = TRUE, false = FALSE)
    )

  # Fix naming
  cyto_details(gs_QC)$name <- rownames(cyto_details(gs_QC))
  return(gs_QC)
}

wsp_files_to_gatingsetlist <- function(wsp_file_list) {
  safe_wsp_to_gs <- safely(wsp_to_gatingset)
  safe_catch <- map(.x = wsp_file_list, .f = \(.file) safe_wsp_to_gs(.file)) |>
    purrr::transpose()
  safe_catch$result
}
