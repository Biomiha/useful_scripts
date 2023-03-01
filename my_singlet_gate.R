my_singlet_gate_fun <- function(fr, pp_res = NULL, channels = c("FSC-A", "FSC-H"), ampl_factor = 1.3){
  require(flowCore)
  exprs_mat <- exprs(fr[, channels])
  indexes <- exprs_mat[, grep(pattern = "-A", channels)] <= ampl_factor * exprs_mat[, grep(pattern = "-A", channels, invert = TRUE)]
  exprs_mat <- exprs_mat[indexes,]
  hpts <- chull(exprs_mat)
  hpts <- c(hpts, hpts[1])
  polygonGate(.gate = exprs_mat[hpts,], filterId = "singlets")
}
require(openCyto)
register_plugins(my_singlet_gate_fun, "my_singlet_gate")
