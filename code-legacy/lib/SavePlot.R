SavePlot <- function (plotname='Plot', plotformat='eps', ffamily='Helvetica',
                      splot=last_plot() ,ploth=210/2, plotw=14) {
  try(dev.off(),silent=TRUE)
  plotobjdir <- './charts-output/charts-objects/'
  plotimagedir <- './charts-output/charts-images/'
  plotdatadir <- './charts-output/charts-data//'
  plotimagepath = paste0(plotimagedir,plotname,'.',plotformat)
  plotobjpath = paste0(plotobjdir,plotname,'.','ggp')
  plotdatapath = paste0(plotdatadir,plotname,'.','ggp')
  if(plotformat=='pdf') {
    ggsave(plotimagepath, plot=splot, family=ffamily, device=cairo_pdf,
           height=ph, width=pw, units='cm')  
  } else if(plotformat=='eps') {
    ggsave(plotimagepath, plot=splot, family=ffamily, device=cairo_ps,
           height=ph, width=pw, units='cm')
  } else {
    ggsave(plotimagepath, plot=splot, family=ffamily,
           height=ploth, width=plotw, units='cm')
  }
  save(splot,file=plotobjpath)
  write.csv(splot$data,file=paste0(plotdatadir,plotname,'_data.csv'))
  splot
}