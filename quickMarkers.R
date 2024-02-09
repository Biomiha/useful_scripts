# This is a function that I exported from Matt Young's SoupX package because Matrix package versions were becoming incompatible
quickMarkers <- function (toc, clusters, N = 10, FDR = 0.01, expressCut = 0.9) 
{
  toc = as(as(as(toc, "dMatrix"), "generalMatrix"), "TsparseMatrix")
  w = which(toc@x > expressCut)
  clCnts = table(clusters)
  nObs = split(factor(rownames(toc))[toc@i[w] + 1], clusters[toc@j[w] + 
                                                               1])
  nObs = sapply(nObs, table)
  nTot = rowSums(nObs)
  tf = t(t(nObs)/as.integer(clCnts[colnames(nObs)]))
  ntf = t(t(nTot - nObs)/as.integer(ncol(toc) - clCnts[colnames(nObs)]))
  idf = log(ncol(toc)/nTot)
  score = tf * idf
  qvals = lapply(seq_len(ncol(nObs)), function(e) p.adjust(phyper(nObs[, 
                                                                       e] - 1, nTot, ncol(toc) - nTot, clCnts[colnames(nObs)[e]], 
                                                                  lower.tail = FALSE), method = "BH"))
  qvals = do.call(cbind, qvals)
  colnames(qvals) = colnames(nObs)
  sndBest = lapply(seq_len(ncol(tf)), function(e) apply(tf[, 
                                                           -e, drop = FALSE], 1, max))
  sndBest = do.call(cbind, sndBest)
  colnames(sndBest) = colnames(tf)
  sndBestName = lapply(seq_len(ncol(tf)), function(e) (colnames(tf)[-e])[apply(tf[, 
                                                                                  -e, drop = FALSE], 1, which.max)])
  sndBestName = do.call(cbind, sndBestName)
  colnames(sndBestName) = colnames(tf)
  rownames(sndBestName) = rownames(tf)
  w = lapply(seq_len(ncol(nObs)), function(e) {
    o = order(score[, e], decreasing = TRUE)
    if (sum(qvals[, e] < FDR) >= N) {
      o[seq(N)]
    }
    else {
      o[qvals[o, e] < FDR]
    }
  })
  ww = cbind(unlist(w, use.names = FALSE), rep(seq_len(ncol(nObs)), 
                                               lengths(w)))
  out = data.frame(gene = rownames(nObs)[ww[, 1]], cluster = colnames(nObs)[ww[, 
                                                                               2]], geneFrequency = tf[ww], geneFrequencyOutsideCluster = ntf[ww], 
                   geneFrequencySecondBest = sndBest[ww], geneFrequencyGlobal = nTot[ww[, 
                                                                                        1]]/ncol(toc), secondBestClusterName = sndBestName[ww], 
                   tfidf = score[ww], idf = idf[ww[, 1]], qval = qvals[ww], 
                   stringsAsFactors = FALSE)
  return(out)
}
