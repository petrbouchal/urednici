MarkSumGroups <- function(data) {
  data$sgrp <- TRUE
  data$sgrp[data$grp=='UO - Ministerstva' | data$grp=='UO - Ostatní' |
            data$grp=='PO' | data$grp=='OOSS' | data$grp=='OSS-SS' |
            data$grp=='SOBCPO'] <- FALSE
  return(data)
}