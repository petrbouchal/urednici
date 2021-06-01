GetColorTable <- function(col) {
  # Convert all colors into format "#rrggbb"
  rgb <- col2rgb(col);
  col <- rgb2col(rgb);
  sort(unique(col))
}
