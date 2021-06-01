# Load and prep -----------------------------------------------------------

library(pbtools)
source('./src/lib/lib_PubFinCZ_light.R')

# Create new layers & sums of layers of pub admin ------------------------

uu$grp <- as.character(uu$sheetname)
uu$grp[uu$sheetname=='UO' & uu$Ministerstvo==TRUE]  <- 'UO - Ministerstva'
uu$grp[uu$sheetname=='UO' & uu$Ministerstvo==FALSE]  <- 'UO - Ostatní'
uu$grp[uu$sheetname=='StatniSprava']  <- 'St. sprava se SOBCPO'
uu$Year <- as.Date(uu$Year)

# turn into wide - expand grps
uu <- uu[uu$Udaj=='PlatyOPPP' | uu$Udaj=='Platy' | uu$Udaj=='OPPP' | uu$Udaj=='Zam',]
uuw1 <- cast(uu,Udaj+BudgetStage+Year+KapAbb+KapNum+KapName+Ministerstvo~grp)

# create new grps
uuw1$UO <- psum(uuw1$`UO - Ministerstva`,uuw1$`UO - Ostatní`,na.rm=T)
uuw1$Exekutiva <- psum(uuw1$UO,uuw1$`OSS-SS`,na.rm=T)

# turn back to long
uuw1 <- as.data.frame(uuw1)
uu <- melt(uuw1,id.vars=c('KapAbb','KapName','KapNum','Year','BudgetStage','Udaj','Ministerstvo'))
uu$grp <- uu$variable
uu$variable <- NULL

# create marker for which grps are sums of others
uu <- MarkSumGroups(uu)

# reorder grps:
grps <- unique(uu$grp) # for easy access as aide de memoire

uu$grp <- factor(uu$grp,
                       levels=c('UO - Ministerstva',
                                'UO - Ostatní',
                                'UO',
                                'OSS-SS',
                                'SOBCPO',
                                'OOSS',
                                'PO',
                                'Exekutiva',
                                'St. sprava se SOBCPO',
                                'StatniSpravabezSOBCPO',
                                'OSS-RO',
                                'ROPO celkem'))