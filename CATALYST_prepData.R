prepData <- function (x, panel = NULL, md = NULL, features = NULL, transform = TRUE, 
          cofactor = 5, panel_cols = list(channel = "fcs_colname", 
                                          antigen = "antigen", class = "marker_class"), 
          md_cols = list(file = "file_name", id = "sample_id", 
                         factors = c("condition", "patient_id")), 
          by_time = TRUE, FACS = FALSE) 
{
  fs <- CATALYST:::.read_fs(x)
  if (is.null(md) && by_time && length(fs) > 1) {
    ts <- keyword(fs, "$BTIM")
    if (any(vapply(ts, is.null, logical(1)))) {
      message("Not all samples contain information on their", 
              " acquisition time; ignoring argument 'by_time'.", 
              " Samples will be kept in their original order.")
    }
    else {
      fs <- fs[order(ts)]
    }
  }
  for (u in c("panel", "md")) if (!is.null(get(u))) 
    assign(u, data.frame(get(u), check.names = FALSE, stringsAsFactors = FALSE))
  stopifnot(is.list(panel_cols), is.list(md_cols))
  args <- as.list(environment())
  for (i in c("md_cols", "panel_cols")) {
    defs <- as.list(formals("prepData")[[i]][-1])
    miss <- !names(defs) %in% names(args[[i]])
    if (any(miss)) {
      fill <- lapply(defs[miss], eval)
      assign(i, c(args[[i]], fill)[names(defs)])
    }
  }
  if (is.null(panel)) {
    panel <- guessPanel(fs[[1]])
    panel$marker_class <- ifelse(panel$use_channel, "state", 
                                 "none")
  } else stopifnot(c("channel", "antigen") %in% names(panel_cols), 
                 all(setdiff(unlist(panel_cols), "marker_class") %in% 
                       names(panel)))
  if (is.null(md)) {
    ids <- fsApply(fs, identifier)
    md <- data.frame(file_name = ids, sample_id = basename(ids))
    md_cols$factors <- NULL
  } else stopifnot(all(unlist(md_cols) %in% names(md)), c("file", 
                                                        "id", "factors") %in% names(md_cols))
  stopifnot(panel[[panel_cols$channel]] %in% colnames(fs))
  if (is.null(features)) {
    features <- as.character(panel[[panel_cols$channel]])
  } else {
    chs <- colnames(fs)
    check1 <- is.logical(features) && length(features) == 
      length(chs)
    check2 <- is.integer(features) && all(features %in% seq_along(chs))
    check3 <- all(features %in% chs)
    if (!any(check1, check2, check3)) 
      stop("Invalid argument 'features'. Should be either", 
           " a logial vector,\n  a numeric vector of indices, or", 
           " a character vector of column names.")
    m <- match(panel[[panel_cols$channel]], features, nomatch = 0)
    panel <- panel[m != 0, , drop = FALSE]
    features <- features[m]
  }
  ids0 <- md[[md_cols$file]]
  ids1 <- fsApply(fs, identifier)
  ids2 <- keyword(fs, "FILENAME")
  if (length(unlist(ids2)) == length(fs)) 
    ids2 <- basename(ids2)
  check1 <- all(ids1 %in% ids0)
  check2 <- all(ids2 %in% ids0)
  ids_use <- which(c(check1, check2))[1]
  ids <- list(ids1, ids2)[[ids_use]]
  if (is.null(ids)) {
    stop("Couldn't match 'flowSet'/FCS filenames\n", 
         "with those listed in 'md[[md_cols$file]]'.")
  } else {
    fs <- fs[match(md[[md_cols$file]], ids)]
  }
  k <- c(md_cols$id, md_cols$factors)
  md <- md[, k, drop = FALSE] %>% mutate_all(factor) %>% rename(sample_id = md_cols$id)
  as <- panel[[panel_cols$antigen]]
  as[is.na(as)] <- panel[[panel_cols$channel]][is.na(as)]
  fs <- fs[, features]
  chs0 <- colnames(fs)
  m1 <- match(panel[[panel_cols$channel]], chs0, nomatch = 0)
  m2 <- match(chs0, panel[[panel_cols$channel]], nomatch = 0)
  as <- as[m2]
  ns <- table(as)
  for (a in names(ns)) if (ns[a] > 1) 
    as[as == a] <- paste(a, seq_len(ns[a]), sep = ".")
  flowCore::colnames(fs)[m1] <- as
  chs <- colnames(fs)
  es <- matrix(fsApply(fs, exprs), byrow = TRUE, nrow = length(chs), 
               dimnames = list(chs, NULL))
  t <- grep("time", colnames(fs), ignore.case = TRUE)
  if (length(t) != 0) {
    ns <- fsApply(fs, nrow)
    t0 <- c(1, cumsum(ns) + 1)
    tx <- t0[-1] - 1
    for (i in seq_along(fs)[-1]) {
      idx <- seq(t0[i], tx[i])
      es[t, idx] <- es[t, idx] + es[t, tx[i - 1]]
    }
  }
  mcs <- c("type", "state", "none")
  if (is.null(panel_cols$class) || is.null(panel[[panel_cols$class]])) {
    mcs <- factor("none", levels = mcs)
  } else {
    mcs <- factor(panel[[panel_cols$class]], levels = mcs)
    if (any(is.na(mcs))) 
      stop("Invalid marker classes detected;", " valid classes are 'type', 'state', and 'none'.")
  }
  rd <- DataFrame(row.names = chs, channel_name = chs0, marker_name = chs, 
                  marker_class = mcs)
  m <- match(chs0, panel[[panel_cols$channel]], nomatch = 0)
  rd$use_channel <- panel$use_channel
  md$n_cells <- as.numeric(fsApply(fs, nrow))
  k <- setdiff(names(md), "n_cells")
  cd <- DataFrame(lapply(md[k], function(u) {
    v <- as.character(rep(u, md$n_cells))
    factor(v, levels = levels(u))
  }), row.names = NULL)
  sce <- SingleCellExperiment(assays = list(counts = es), rowData = rd, 
                              colData = cd, metadata = list(experiment_info = md))
  ds <- keyword(fs[[1]])
  l <- list(cyt = "\\$CYT$", sn = "\\$CYTSN$")
  keep <- lapply(l, grep, names(ds))
  int_metadata(sce)$description <- ds[unlist(keep)]
  if (!FACS && (length(keep$cyt) == 0 || !grepl("FACS", 
                                                ds[[keep$cyt]]))) {
    is_mass <- !is.na(CATALYST:::.get_ms_from_chs(chs0))
    foo <- DataFrame(matrix(vector(), nrow = ncol(sce)))
    icd <- DataFrame(t(es[!is_mass, , drop = FALSE]), check.names = FALSE)
    colnames(icd) <- rownames(es)[!is_mass]
    icd$reducedDims <- icd$altExps <- foo
    colData(sce) <- as(cbind(colData(sce), icd), Class = "DataFrame")
    sce <- sce[is_mass, ]
  }
  if (transform) 
    CATALYST:::.transform(sce, cofactor)
  else sce
}
