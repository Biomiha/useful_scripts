alra.low.memory <- function( A_norm, k=0,q=10, quantile.prob = 0.001, use.mkl = F, mkl.seed=-1) {
  # Computes the k-rank approximation to A_norm and adjusts it according to the
  # error distribution learned from the negative values.
  #
  # Args:
  #   A_norm: The log-transformed expression matrix of cells (rows) vs. genes (columns)
  #   k : the rank of the rank-k approximation. Set to 0 for automated choice of k.
  #   q : the number of additional power iterations in randomized SVD
  #   use.mkl : Use the Intel MKL based implementation of SVD. Needs to be
  #             installed from https://github.com/KlugerLab/rpca-mkl.
  #   mkl.seed : Only relevant if use.mkl=T. Set the seed for the random
  #   generator for the Intel MKL implementation of SVD. Any number <0 will
  #   use the current timestamp. If use.mkl=F, set the seed using
  #   set.seed() function as usual.
  #
  # Returns:
  #   A list with three items
  #       1) The rank k approximation of A_norm.
  #       2) The rank k approximation of A_norm, adaptively thresholded
  #       3) The rank k approximation of A_norm, adaptively thresholded and
  #       with the first two moments of the non-zero values matched to the
  #       first two moments of the non-zeros of A_norm. This is the completed
  #       matrix most people will want to work with
  # Example:
  #     result.completed <- adjusted_svd(A_norm,15)
  #     A_norm_rank15 <- result.completed[[1]]     # The low rank approximation for reference purposes...not suggested for matrix completion
  #     A_norm_rank15_cor <- result.completed[[3]] # The actual adjusted, completed matrix
  
  cat(sprintf("Read matrix with %d cells and %d genes\n", nrow(A_norm), ncol(A_norm)))
  if(!("matrix" %in% class(A_norm))) {
    stop(sprintf("A_norm is of class %s, but it should be of class matrix. Did you forget to run as.matrix()?",class(A_norm)))
  }
  
  if (k ==0 ) {
    k_choice <- choose_k(A_norm)
    k <-  k_choice$k
    cat(sprintf("Chose k=%d\n",k))
  }
  
  cat("Getting nonzeros\n")
  originally_nonzero <- A_norm >0 
  
  cat("Randomized SVD\n")
  if (!use.mkl) {
    fastDecomp_noc <- randomized.svd(A_norm,k,q=q)
  }else {
    fastDecomp_noc <- randomized.svd(A_norm,k,q=q, method='rsvd-mkl', mkl.seed=mkl.seed)
  }
  A_norm_rank_k <- fastDecomp_noc$u[,1:k]%*%diag(fastDecomp_noc$d[1:k])%*% t(fastDecomp_noc$v[,1:k])
  
  
  cat(sprintf("Find the %f quantile of each gene\n", quantile.prob))
  #A_norm_rank_k_mins <- abs(apply(A_norm_rank_k,2,min))
  A_norm_rank_k_mins <- abs(apply(A_norm_rank_k,2,FUN=function(x) quantile(x,quantile.prob)))
  cat("Sweep\n")
  A_norm_rank_k <- replace(A_norm_rank_k, A_norm_rank_k <= A_norm_rank_k_mins[col(A_norm_rank_k)], 0)
  
  
  sd_nonzero <- function(x) sd(x[!x == 0])
  sigma_1 <- apply(A_norm_rank_k, 2, sd_nonzero)
  sigma_2 <- apply(A_norm, 2, sd_nonzero)
  mu_1 <- colSums(A_norm_rank_k)/colSums(!!A_norm_rank_k)
  mu_2 <- colSums(A_norm)/colSums(!!A_norm)
  
  toscale <- !is.na(sigma_1) & !is.na(sigma_2) & !(sigma_1 == 0 & sigma_2 == 0) & !(sigma_1 == 0)
  
  cat(sprintf("Scaling all except for %d columns\n", sum(!toscale)))
  
  sigma_1_2 <- sigma_2/sigma_1
  toadd  <- -1*mu_1*sigma_2/sigma_1 + mu_2
  
  A_norm_rank_k_temp <- A_norm_rank_k[,toscale]
  A_norm_rank_k_temp <- sweep(A_norm_rank_k_temp,2, sigma_1_2[toscale],FUN = "*")
  A_norm_rank_k_temp <- sweep(A_norm_rank_k_temp,2, toadd[toscale],FUN = "+")
  
  A_norm_rank_k_cor_sc <- A_norm_rank_k
  A_norm_rank_k_cor_sc[,toscale] <- A_norm_rank_k_temp
  A_norm_rank_k_cor_sc[A_norm_rank_k==0] = 0
  rm(A_norm_rank_k_temp, A_norm_rank_k)
  
  lt0 <- A_norm_rank_k_cor_sc  <0
  A_norm_rank_k_cor_sc[lt0] <- 0 
  cat(sprintf("%.2f%% of the values became negative in the scaling process and were set to zero\n", 100*sum(lt0)/(nrow(A_norm)*ncol(A_norm))))
  
  A_norm_rank_k_cor_sc[originally_nonzero & A_norm_rank_k_cor_sc ==0] <- A_norm[originally_nonzero & A_norm_rank_k_cor_sc ==0]
  
  colnames(A_norm_rank_k_cor_sc) <- colnames(A_norm)
  
  original_nz <- sum(A_norm>0)/(nrow(A_norm)*ncol(A_norm))
  completed_nz <- sum(A_norm_rank_k_cor_sc>0)/(nrow(A_norm)*ncol(A_norm))
  cat(sprintf("The matrix went from %.2f%% nonzero to %.2f%% nonzero\n", 100*original_nz, 100*completed_nz))
  
  list(A_norm_rank_k=NULL,A_norm_rank_k_cor =NULL, A_norm_rank_k_cor_sc=A_norm_rank_k_cor_sc)
}