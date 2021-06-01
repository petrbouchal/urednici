TintShade <- function(colors, tints=c(), shades=c(), hexin=TRUE) {
  if(hexin) {
    rgbcols <- col2rgb(colors)
  } else {
    rgbcols <- colors
  }
  rgbout <- matrix(colors,nrow=1)
  if(length(tints)>0) {
    for(i in tints) {
      rgbtint <- rgbcols+(255-rgbcols)*(1-i)
      coltint <- rgb2col(rgbtint)
      rgbout <- rbind(rgbout, coltint)
    }
  }
  if(length(shades)>0) {
    for(i in shades) {
      rgbshade <- rgbcols-(rgbcols)*(1-i)
      colshade <- rgb2col(rgbshade)
      rgbout <- rbind(rgbout, colshade)
    }
  }
  return(t(rgbout))
}