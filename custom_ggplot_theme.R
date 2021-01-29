# set ggplot theme
theme_set(theme_bw() +
            theme(axis.line = element_blank(),
                  axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size = 14),
                  axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 14),
                  axis.text.x = element_text(size = 12),
                  axis.text.y = element_text(size = 12),
                  legend.title = element_text(size = 14), 
                  legend.text = element_text(size = 12),
                  strip.text.x = element_text(size = 14),
                  plot.title = element_text(hjust = 0, size = 16),
                  plot.subtitle = element_text(hjust = 0, size = 12),
                  legend.justification="left"))
