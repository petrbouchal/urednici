DataFinalPrep <- function (exclusion = T) {
  library(data.table)
  if(exclusion){
    load('./data-output/groups_bezMVaMZV.RData')
  } else {
    load('./data-output/groups_all.RData')
  }
  
  hh <- RelabelGroups(hh)
  hh$grp <- factor(hh$grp, levels=c("Ministerstva","Ostatní ústřední",
                                    "Neústřední st. správa","Státní správa",
                                    "Sbory","Ostatní vč. armády","Příspěvkové",
                                    "Vše","Státní správa se sbory","Ústřední úřady",
                                    "StatniSpravabezSOBCPO"))
  
  mingrpset <- c('Ministerstva','Ostatní ústřední','Neústřední st. správa',
                 'Státní správa')
  hh$mingrpset <- FALSE
  hh$mingrpset[hh$grp %in% mingrpset] <- TRUE
  
  maxgrpset <- c('Ministerstva','Ostatní ústřední','Neústřední st. správa',
                 'Státní správa','Příspěvkové','Ostatní vč. armády','Sbory','Vše')
  hh$maxgrpset <- FALSE
  hh$maxgrpset[hh$grp %in% maxgrpset] <- TRUE
  
  # add colors for each layer of civil service
  groupcolours <- read.csv('./data-input/groupcolours.csv')
  uu <- merge(hh,groupcolours,all.x=T)
  
  return(uu)
}