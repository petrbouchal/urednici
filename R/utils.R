draw_key_pointrange_h <- function (data, params, size)
{
  grid::grobTree(draw_key_path(data, params, size), draw_key_point(transform(data,
                                                                        size = (data$size %||% 1.5) * 4), params))
}

gir_tooltip_css <- "font-family: 'IBM Plex Sans';
                    background-color: white;
                    color: black;
                    padding: 5px;
                    border-radius: 5px"
