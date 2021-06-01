source('./src/lib/lib_PubFinCZ.R')

# Load and subset data ----------------------------------------------------

#uu0 <- LoadDataforPlotting()
uu <- uu0[uu0$Udaj=='PlatyOPPP' | uu0$Udaj=='Zam',]

# Set up variables --------------------------------------------------------

uu2003 <- data.frame('skutecnost2003'=uu$value[uu$Year=='2003-01-01' & uu$BudgetStage=='skutecnost'],
                     'KapNum'=uu$KapNum[uu$Year=='2003-01-01' & uu$BudgetStage=='skutecnost'],
                     'Udaj'=uu$Udaj[uu$Year=='2003-01-01' & uu$BudgetStage=='skutecnost'],
                     'sheetname'=uu$sheetname[uu$Year=='2003-01-01' & uu$BudgetStage=='skutecnost'])
uu <- merge(uu,uu2003)
uu$value[uu$value==0] <- NA
uu$frombase <- uu$value/uu$skutecnost2003
uu$group <- paste0(uu$KapNum,uu$Udaj)
uu$group1 <- paste0(uu$KapNum,uu$Udaj,uu$BudgetStage)

uu$StageNum <- ifelse(uu$BudgetStage=='schvaleny',1,
                      ifelse(uu$BudgetStage=='upraveny',2,
                             ifelse(uu$BudgetStage=='skutecnost',3,4)))
uu$sorter <- paste0(uu$Year,' ',uu$StageNum)
uu$label <- ifelse(uu$Year=='2013-01-01',as.character(uu$Udaj),'')
uu$BudgetStage <- factor(uu$BudgetStage,levels(uu$BudgetStage)[c(2,4,3,1)])
uu <- uu[with(uu,order(sorter)),]
uu$StageYear <- paste0(uu$Year,' ',uu$BudgetStage)

#deflate
uu$frombase[uu$Udaj=='PlatyOPPP'] <- uu$frombase[uu$Udaj=='PlatyOPPP']/uu$Infl2003Base[uu$Udaj=='PlatyOPPP']

# Subset for plotting -----------------------------------------------------

uu <- uu[uu$sheetname=='UO' & uu$Ministerstvo==TRUE,]
uu$frombase[uu$Year=='2003-01-01'] <- 1

# Create lagged variable for waterfall plot -------------------------------

# previous <- ddply(uu,.(KapNum,Udaj),transform,
#                   previous = c(NA, uu$frombase[-length(uu)]))
# uu <- merge(uu,previous)

# Build plot --------------------------------------------------------------

plot <- ggplot(uu,aes(sorter,frombase,group=group,colour=BudgetStage)) +
  geom_line(data=uu,aes(size=Udaj))+
  geom_text(aes(label=label),colour='black') +
  scale_size_manual(values=c(1,2)) +
  facet_wrap( ~ KapAbb,scales='free_y') +
  theme(axis.text.x=element_text(angle=90))
plot

plot <- ggplot(uu,aes(Year,frombase,group=group1,colour=BudgetStage)) +
  geom_line(data=uu,aes(linetype=Udaj))+
  geom_text(aes(label=label),colour='black') +
  facet_wrap( ~ KapAbb,scales='free_y') +
  theme(axis.text.x=element_text(angle=90))
plot