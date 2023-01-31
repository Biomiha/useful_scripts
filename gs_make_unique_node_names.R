gs_make_unique_node_names <- function(gs) {
  require(cytoverse)
  require(tidyverse)
  old_node_names <- gs_get_pop_paths(gs)
  last_node_names <- old_node_names |> 
    str_split(pattern = "/") |> 
    map_chr(.f = \(.vec) tail(.vec, n = 1))
  while(any(duplicated(last_node_names))) {
    old_node_names <- gs_get_pop_paths(gs)
    last_node_names <- old_node_names |> 
      str_split(pattern = "/") |> 
      map_chr(.f = \(.vec) tail(.vec, n = 1))
    if(any(duplicated(last_node_names))) {
      to_change <- min(which(duplicated(last_node_names)))
      new_node_name <- make.unique(last_node_names)[to_change]
      try(gs_pop_set_name(x = gs, y = old_node_names[to_change], value = new_node_name))
    } else break
  }
}