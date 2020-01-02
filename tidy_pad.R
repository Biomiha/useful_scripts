padding_fun <- function(df, col_to_split, col_to_pad = c(1, 2), width, side, pad, separator, ...) {
  require(tidyverse)
  tmp_col_to_pad = ifelse(test = col_to_pad == 1, yes = "tmp_col1", no = "tmp_col2")
  split_sep = paste0("\\", separator)
  n_cols = ncol(df)
  df %>% 
    separate(sep = split_sep, col = {{col_to_split}}, into = c("tmp_col1", "tmp_col2"), ...) %>% 
    dplyr::mutate({{tmp_col_to_pad}} := str_pad(string = .data[[tmp_col_to_pad]], width = width, side = side, pad = pad)) %>% 
    unite(col = {{col_to_split}}, n_cols:(n_cols + 1), sep = separator)
}
