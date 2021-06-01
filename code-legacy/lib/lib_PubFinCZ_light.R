source('./src/lib/load_packages_light.R')

# Design elements to be picked up by libraries ----------------------------
source('./src/lib/rgb2col.R')
source('./src/lib/TintShade.R')

rgbidcols <- c(c(247,150,70),c(79,129,189),c(192,80,77),
               c(75,172,198),c(0,0,0),c(155,187,89))
idcols <- rgb2col(rgbidcols)
idcols_ordered <- idcols[c(6,5,4,1,3,2)]

fontfamily='Helvetica'
basecols <- idcols
mycols <- TintShade(basecols,tints=c(.5,.25),hexin=T,)
dimnames(colours) <- NULL
rm(basecols)
# font_import()
loadfonts(device='postscript',quiet=TRUE)
loadfonts(quiet=TRUE)
if(Sys.info()[['sysname']]=='Darwin') {mysys='mac'} else {mysys='win'}
if(mysys=='win') {loadfonts(device='win',quiet=TRUE)}


# Load functions / libraries ----------------------------------------------

source('./src/lib/LoadDataforPlotting.R')
source('./src/lib/SavePlot.R')
source('./src/lib/SortGroups.R')
source('./src/lib/GetColorTable.R')
source('./src/lib/custom_themes.R')
source('./src/lib/AddBaseValue.R')
source('./src/lib/AddEconIndicators.R')
source('./src/lib/CalcAvgsAndIndices.R')
source('./src/lib/psum.R')
source('./src/lib/MarkSumGroups.R')
source('./src/lib/SaveCSVwithEncoding.R')
source('./src/lib/RelabelGroups.R')
source('./src/lib/space.R')
source('./src/lib/DataFinalPrep.R')

