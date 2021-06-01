source('./src/lib/lib_PubFinCZ.R')
  
#source('./src/SuperData_ReadXLS.R')

#uu0 <- LoadDataforPlotting('chapters')

uu <- uu0
uu <- uu[uu$Udaj=='AvgSalIndexSchv2schv' | uu$Udaj=='AvgSalIndexSkut2skut',]
uu <- uu[uu$Udaj=='Zam',]
uu <- uu[uu$sheetname=='UO' & uu$BudgetStage=='skutecnost',]
uu <- uu[uu$Year=='2012-01-01',]

uu <- uu0

#This should equal total in ROPO table in XLS
uu <- uu[uu$BudgetStage=='schvaleny',]
sum(uu$value[uu$Year=='2013-01-01' & uu$Udaj=='PlatyOPPP' & uu$sheetname=='OOSS'])
sum(uu$value[uu$Year=='2013-01-01' & uu$Udaj=='PlatyOPPP' & uu$sheetname=='ROPO celkem'])
sum(uu$value[uu$Year=='2013-01-01' & uu$Udaj=='PlatyOPPP' & uu$sheetname=='OSS-SS'])
sum(uu$value[uu$Year=='2013-01-01' & uu$Udaj=='PlatyOPPP' & uu$sheetname=='UO'])
sum(uu$value[uu$Year=='2004-01-01' & uu$Udaj=='PlatyOPPP' & uu$sheetname=='PO'],na.rm=T)
sum(uu$value[uu$Year=='2004-01-01' & uu$Udaj=='PlatyOPPP' & uu$sheetname=='ROPO celkem'],na.rm=T)