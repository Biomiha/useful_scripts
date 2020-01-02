padding_fun <- function(df, separator, sep_into, col_to_pad, new_col_name, width, side, pad, ...) {
  require(dplyr)
  require(tidyr)
  require(stringr)
  split_sep = paste0("\\", separator)
  n_cols = ncol(df)
  df %>% 
    separate(sep = split_sep, ...) %>% 
    dplyr::mutate({{col_to_pad}} := str_pad(string = {{col_to_pad}}, width = width, side = side, pad = pad)) %>% 
    unite(col = {{new_col_name}}, n_cols:(n_cols + 1), sep = separator)
}
