source('./src/lib/lib_PubFinCZ_light.R')

uu <- LoadDataforPlotting('orgs')
uu <- LoadDataforPlotting('chapters')
uu$type <- 'Ministerstvo HQ'
uu$OrgName <- 'Ministerstvo XX'
uu <- ee[ee$Ministerstvo==TRUE,]
uu <- uu[uu$sheetname=='UO' & uu$Year=='2013-01-01' & ee$BudgetStage=='schvaleny',]

ee <- ee[ee$Year=='2013-01-01',]
ee <- ee[(ee$Udaj=='AvgSal' | ee$Udaj=='Zam') & ee$BudgetStage=='schvaleny',]
ee <- ee[ee$Ministerstvo==TRUE,]
ee$Ministerstvo <- NULL
ee <- cast(ee, OrgName + ... ~ Udaj)

ee$type <- ifelse(ee$OrgName=='Celkem','Celá kapitola','Jednotlivé organizace')

bplot <- ggplot(ee,aes(KapAbb, AvgSal)) +
  geom_point(aes(colour=type, size=Zam))
bplot