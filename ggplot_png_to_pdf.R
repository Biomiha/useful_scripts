ggplot_png_to_pdf <- function(ggplot_object, out_name = "plot.pdf", device = "png", height = 210, width = 297, units = "mm", dpi = 300, ...){
  require(tidyverse)
  require(magick)
  stopifnot(is.list(ggplot_object))
  stopifnot(any(class(ggplot_object[[1]]) %in% "ggplot"))
  tmp_dir <- tempdir(check = TRUE)
  fs::file_delete(list.files(path = tmp_dir, pattern = ".png", full.names = TRUE))
  if(is.null(names(ggplot_object))) {
    num_files <- length(ggplot_object)
    file_names <- seq_len(num_files) %>% 
      str_pad(string = ., width = nchar(as.character(num_files)), side = "left", pad = "0") %>% 
      str_c(., ".png")
  } else {
    file_names <- names(ggplot_object) %>% 
      str_c(., ".png")
  }
  walk(.x = seq_along(file_names), .f = ~ggsave(filename = file_names[[.x]], path = tmp_dir, plot = ggplot_object[[.x]], device = device, height = height, width = width, units = units, dpi = dpi))
  image_write(image_read(list.files(tmp_dir, pattern = ".png", full.names = TRUE)), format = "pdf", out_name)
  try(fs::file_delete(list.files(path = tmp_dir, pattern = ".png", full.names = TRUE)), silent = TRUE)
}
