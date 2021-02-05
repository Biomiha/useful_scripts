normCytof <- function (x, beads, k = 500, trim = 5, remove_beads = TRUE, norm_to = NULL, 
          assays = c("counts", "exprs"), overwrite = TRUE, 
          transform = TRUE, cofactor = NULL, plot = TRUE, verbose = TRUE) 
{
  args <- as.list(environment())
  CATALYST:::.check_args_normCytof(args)
  if (is.null(cofactor)) 
    cofactor <- int_metadata(x)$cofactor
  icd <- colData(x)
  chs <- channels(x)
  chs0 <- rownames(x)
  rownames(x) <- chs
  stopifnot(any(grepl("Ir191|Ir193", chs, ignore.case = TRUE)), 
            any(grepl("time", names(icd), ignore.case = TRUE)))
  ts <- icd[[grep("time", names(icd), ignore.case = TRUE)]]
  dna_chs <- grep("Ir191|Ir193", chs, ignore.case = TRUE, 
                  value = TRUE)
  bead_chs <- chs[CATALYST:::.get_bead_cols(chs, beads)]
  rowData(x)$bead_ch <- chs %in% bead_chs
  n_beads <- length(bead_chs)
  if (verbose) 
    message("Identifying beads...")
  ms <- CATALYST:::.get_ms_from_chs(chs)
  m <- match(c(dna_chs, bead_chs), chs)
  key <- matrix(c(0, 0, rep(1, n_beads)), ncol = n_beads + 
                  2, dimnames = list("is_bead", ms[m]))
  key <- data.frame(key, check.names = FALSE)
  is_bead <- CATALYST:::.get_bead_inds(x, key, assays[2])
  cs <- assay(x, assays[1])
  es <- assay(x, assays[2])
  ths <- rowMins(es[bead_chs, is_bead])
  rmv <- colSums(es[bead_chs, ] > ths) == n_beads
  x$remove <- rmv
  y <- es[bead_chs, is_bead]
  meds <- rowMedians(y)
  mads <- rowMads(y) * trim
  diff <- abs(y - meds)
  ex <- colAnys(diff > mads)
  is_bead[which(is_bead)[ex]] <- FALSE
  x$is_bead <- is_bead
  if (is.null(norm_to)) {
    bl <- rowMeans(cs[bead_chs, is_bead])
  } else {
    if (is.character(norm_to)) {
      ref <- read.FCS(norm_to, transformation = FALSE, 
                      truncate_max_range = FALSE)
    } else {
       ref <- norm_to }
    bl <- colMeans(exprs(ref)[, .get_bead_cols(colnames(ref), 
                                               beads)])
  }
  if (verbose) 
    message("Computing normalization factors...")
  if (k%%2 == 0) 
    k <- k + 1
  smooth0 <- t(apply(cs[bead_chs, is_bead], 1, runmed, k, "constant"))
  slopes <- colSums(smooth0 * bl)/colSums(smooth0^2)
  slopes <- approx(ts[is_bead], slopes, ts, rule = 2)$y
  cs <- sweep(cs, 2, slopes, "*")
  c <- ifelse(overwrite, assays[1], "normcounts")
  assay(x, c, FALSE) <- cs
  y <- cs[bead_chs, is_bead]
  smooth <- t(apply(y, 1, runmed, k, "constant"))
  ps <- NULL
  if (plot) {
    ps <- list(scatter = CATALYST:::.plot_bead_scatter(x, dna_chs, bead_chs, 
                                            assays[2]), lines = CATALYST:::.plot_smooth_beads(smooth0, smooth, 
                                                                                   ts[is_bead]))
  }
  if (transform) {
    e <- ifelse(overwrite, assays[2], "normexprs")
    x <- CATALYST:::.transform(x, cofactor, ain = c, aout = e)
  }
  rownames(x) <- chs0
  if (remove_beads) {
    z <- list(data = x[, !(is_bead | rmv)], beads = x[, is_bead], 
              removed = x[, is_bead | rmv])
  } else {
    z <- list(data = x)
  }
  return(c(z, ps))
}