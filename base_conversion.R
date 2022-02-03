base <- function(b, base = 10)
{
  base <- as.integer(base)
  if(base > 36 | base < 2) stop("'base' must be between 2 and 36.")
  
  structure(lapply(b, function(x) 
  {
    n   <- ceiling(log(x, base))
    vec <- numeric()
    val <- x
    
    while(n >= 0)
    {
      rem <- val %/% base^n
      val <- val - rem * base^n
      vec <- c(vec, rem)
      n <- n - 1
    }
    
    while(vec[1] == 0 & length(vec) > 1) vec <- vec[-1]
    structure(x, base = base, representation = vec) 
  }), class = "base")
}

format.base <- function(b, ...) 
{
  sapply(b, function(x) 
  {
    glyphs <- c(0:9, LETTERS)
    base   <- attr(x, "base")
    vec    <- attr(x, "representation")
    paste0(glyphs[vec + 1], collapse = "")
  })
}

print.base <- function(b, ...) print(format(b), quote = FALSE)

Ops.base <- function(e1, e2) {
  base <- attr(e1[[1]], "base")
  e1   <- unlist(e1)
  e2   <- unlist(e2)
  base(NextMethod(.Generic), base)
}

Math.base <- function(e1, e2) {
  base <- attr(e1[[1]], "base")
  e1   <- unlist(e1)
  e2   <- unlist(e2)
  base(NextMethod(.Generic), base)
}

as.data.frame.base <- function(b, ...) 
{
  structure(list(b),  
            class = "data.frame", 
            row.names = seq_along(b))
}