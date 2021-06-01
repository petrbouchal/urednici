source('./src/lib/lib_PubFinCZ.R')

# source('./src/SuperData_ReadXLS.R')
# uu0 <- LoadDataforPlotting()

uu <- uu0
uu <- uu[uu$Udaj=='AvgSal' & uu$Year!='2013-01-01',]
#uu <- uu[uu$sheetname=='ROPO celkem' | uu$sheetname=='UO'| uu$sheetname=='StatniSprava',]
uu <- uu[uu$sheetname=='UO' & uu$Ministerstvo==TRUE,]

#deflate
uu$value <- uu$value/uu$Infl2003Base

# order legend
uu$BudgetStage <- droplevels(uu$BudgetStage)
uu$BudgetStage <- factor(uu$BudgetStage,levels(uu$BudgetStage)[c(1,3,2)])

uu$value[uu$value==0] <- NA
plot <- ggplot(uu, aes(Year, value, colour=BudgetStage)) +
  geom_line(size=1) +
  #scale_y_continuous(limits=c(20000,45000))+
  scale_x_date(breaks='1 year',labels=date_format('%Y'))+
  facet_wrap( ~ KapAbb,ncol=4,scales='fixed') +
  theme_economist() +
  scale_y_continuous(labels=comma) +
  theme(axis.text.x=element_text(angle=90))
plot