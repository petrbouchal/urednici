source('./src/lib/lib_PubFinCZ.R')

# source('./src/SuperData_ReadXLS.R')
uu0 <- LoadDataforPlotting()

uu <- uu0
uu <- uu[uu$Udaj=='AvgSalIndexSchv2schv' | uu$Udaj=='AvgSalIndexSkut2skut',]
uu <- uu[uu$sheetname=='UO' & uu$Ministerstvo==TRUE,]
uu <- uu[uu$Year=='2012-01-01']

#deflate
uu$value <- uu$value/uu$Inflation

# order legend
uu$BudgetStage <- droplevels(uu$BudgetStage)
uu$BudgetStage <- factor(uu$BudgetStage,levels(uu$BudgetStage)[c(1,3,2)])
uu <- uu[abs(uu$value-100)<25,]
uu$KapAbb <- droplevels(uu$KapAbb)
uu <- uu[!is.na(uu$KapAbb),]

uu$value[uu$value==0] <- NA
plot <- ggplot(uu, aes(Year)) +
  geom_bar(data=uu[uu$Udaj=='AvgSalIndexSkut2skut',],aes(y=value/100-1),
           stat='identity',position='identity',fill='red',colour='red') +
  geom_bar(data=uu[uu$Udaj=='AvgSalIndexSchv2schv',],aes(y=value/100-1),
           stat='identity',position='identity',fill=NA,colour='black',
           linetype='dashed',size=.5) +
#  geom_segment(x=-Inf,xend=Inf,y=1,yend=1,colour='gray50',size=.6) +
#  geom_line(aes(colour=Udaj),size=1) +
  scale_x_date(breaks='1 year',labels=date_format('%Y'))+
  scale_y_continuous(labels=percent) +
  facet_wrap( ~ KapAbb,ncol=4,scales='fixed') +
  theme_economist() +
  theme(axis.text.x=element_text(angle=90))
plot