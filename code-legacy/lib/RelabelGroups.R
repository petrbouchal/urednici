RelabelGroups <- function(dataset){
  levels(dataset$grp)[levels(dataset$grp)=="UO"] <- "Ústřední úřady"
  levels(dataset$grp)[levels(dataset$grp)=="UO - Ministerstva"] <- "Ministerstva"
  levels(dataset$grp)[levels(dataset$grp)=="UO - Ostatní"] <- "Ostatní ústřední"
  levels(dataset$grp)[levels(dataset$grp)=="OSS-SS"] <- "Neústřední st. správa"
  levels(dataset$grp)[levels(dataset$grp)=="SOBCPO"] <- "Sbory"
  levels(dataset$grp)[levels(dataset$grp)=="OOSS"] <- "Ostatní vč. armády"
  levels(dataset$grp)[levels(dataset$grp)=="PO"] <- "Příspěvkové"
  levels(dataset$grp)[levels(dataset$grp)=="Exekutiva"] <- "Státní správa"
  levels(dataset$grp)[levels(dataset$grp)=="St. sprava se SOBCPO"] <- "Státní správa se sbory"
  levels(dataset$grp)[levels(dataset$grp)=="OSS-RO"] <- "Rozpočtové organizace"
  levels(dataset$grp)[levels(dataset$grp)=="ROPO celkem"] <- "Vše"
  return(dataset)
}