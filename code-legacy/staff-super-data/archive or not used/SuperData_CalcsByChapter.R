# Load and prep -----------------------------------------------------------

source('./src/lib/lib_PubFinCZ.R')
uu0 <- LoadDataforPlotting('chapters')
uu <- uu0

source('./src/staff-super-data/SuperData_FirstReshapeAndCalcs.R')

# Calculate indices and changes between budget stages ---------------------

# reshape to wide - expand variables
uu$UdajStage <- paste0(uu$Udaj,'_',uu$BudgetStage)
uu$Udaj <- NULL
uu$BudgetStage <- NULL

uuw2 <- cast(uu,grp+Year+KapAbb+KapNum+KapName+Ministerstvo+sgrp~UdajStage)

# can restrict years here to speed up processing if fewer are needed
uuw2 <- uuw2[uuw2$Year=='2010-01-01' | uuw2$Year=='2011-01-01',]

# calculate average pay and indices
uuw2 <- CalcAvgsAndIndices(uuw2)

# turn back into long
uuw2 <- as.data.frame(uuw2)
uu <- melt(uuw2, id.vars=c('Year','grp','KapAbb','KapName','KapNum','sgrp','Ministerstvo'))

uu <- AddBaseValue(uu,'2010-01-01')
uu <- AddEconIndicators(uu)

# create markers for budget stages and type of data (budget x index)
# HERE

# create markers for groups of grps
uu$exekutiva <- FALSE
uu$exekutiva[uu$grp=='UO - Ministerstva' | uu$grp=='UO - Ostatní' | 
               uu$grp=='OSS-SS']  <- TRUE
uu$UO <- FALSE
uu$UO[uu$grp=='UO - Ministerstva' | uu$grp=='UO - Ostatní' | 
        uu$grp=='UO'] <- TRUE

# calculate real changes
uu$realchange <- uu$perc_base/uu$Infl2003Base

# Plots -------------------------------------------------------------------

# simple composition in terms of additive categories of organisation
title='Zaměstnanci kapitol podle typu organizace (rozpočty)'
ylab='Počet zaměstnanců'
xlab=''
uu2 <- uu[uu$sgrp==F & uu$variable=='Zam_schvaleny' & uu$value!=0,]
uu2$value[uu2$value==0] <- NA
plot <- ggplot(uu2,aes(y=value,x=Year,fill=grp,group=grp)) +
  geom_bar(stat='identity',position='stack') +
  scale_y_continuous(labels=comma) +
  facet_wrap(~KapAbb,scales='free_y') +
  labs(title=title, x=xlab, y=ylab)
plot

# did departments cut staff or pay?
title='Snižování platů, nebo propouštění? 2010-11'
xlab='Průměrný plat: změna v %'
ylab='Počet zaměstnanců: změna v %'
uu2 <- uu[uu$sgrp==F & (uu$variable=='Zam_skutecnost' | 
                          uu$variable=='AvgSal_skutecnost' |
                          uu$variable=='AvgSal_upr2skut') 
          & uu$value!=0,]
uu2 <- uu2[!is.na(uu2$variable),]
uu2 <- cast(uu2[c(1:8,12)], grp+Year+KapAbb+KapNum+KapName+Ministerstvo+sgrp~variable)
uu2$increasedsalarydif <- 'Žádná nebo malá změna'
uu2$increasedsalarydif[1/uu2$AvgSal_upr2skut<1] <- 'Pokles'
uu2$increasedsalarydif[(1/uu2$AvgSal_upr2skut>1)] <- 'Nárůst'
uu2 <- uu2[uu2$AvgSal_skutecnost!=1,]
plot <- ggplot(uu2,aes(x=AvgSal_skutecnost-1,y=Zam_skutecnost-1,colour=increasedsalarydif)) +
  geom_hline(y=0) +
  geom_vline(x=0) +
#   geom_point(stat='identity',aes(size=1/AvgSal_upr2skut-1)) +
  geom_text(aes(label=KapAbb)) +
  geom_line(data=data.frame(x=seq(-.2,.2,0.01)),aes(x=x,y=(-x-0.1)),colour='red') +
  geom_line(data=data.frame(x=seq(-.2,.2,0.01)),aes(x=x,y=x),colour='red',linetype='dotted') +
  scale_y_continuous(labels=percent, limits=c(-.2,.2)) +
  scale_x_continuous(labels=percent,limits=c(-.2,.2)) +
#  scale_color_continuous(low='red',high='blue') +
  scale_shape_manual(values=c(3,19)) +
  labs(title=title, x=xlab, y=ylab) + facet_wrap(~ grp)
plot

# real change in pay since base year
title='Změna průměrné hrubé mzdy očistěná o inflaci\nMinisterstva, skutečný stav, 2003=100%'
ylab='% změna od 2003, očištěno o inflaci'
xlab=''
uu2 <- uu[uu$variable=='AvgSal_skutecnost' & uu$grp=='UO - Ministerstva' & 
            uu$Ministerstvo,]
plot <- ggplot(uu2,aes(x=Year,y=realchange)) +
  geom_line(size=1) +
  scale_y_continuous(labels=percent) +
  facet_wrap(~KapAbb) + 
  labs(title=title, x=xlab, y=ylab)
plot

# differential between budgeted and actual salary, by layer and chapter in 2012
title='Průměrný plat: rozdíl mezi rozpočtem a výsledkem podle kapitoly a typu organizace'
ylab='Rozdíl v Kč'
xlab=''
uu2 <- uu[uu$Year=='2011-01-01' & uu$variable=='AvgSal_uprMinusskut' & !uu$sgrp,]
plot <- ggplot(uu2,aes(x=grp, fill=grp)) +
  geom_bar(stat='identity', aes(y=-value)) +
  scale_y_continuous(limits=c(-2000,10000)) +
  facet_wrap(~KapAbb) +
  labs(title=title, x=xlab, y=ylab)
plot

# same thing in percentage terms
title='Průměrný plat: rozdíl mezi rozpočtem a výsledkem podle kapitoly a typu organizace'
ylab='Rozdíl v %'
xlab=''
uu2 <- uu[uu$variable=='AvgSal_upr2skut' & uu$Year=='2011-01-01' & !uu$sgrp,]
plot <- ggplot(uu2,aes(x=grp, fill=grp)) +
  geom_bar(stat='identity', aes(y=1/value-1)) +
  scale_y_continuous(labels=percent) +
  facet_wrap(~KapAbb) +
  labs(title=title, x=xlab, y=ylab)
plot

# composition of jobs gap
title='Zaměstnanci: rozdíl mezi rozpočtem a výsledkem podle kapitoly a typu organizace'
ylab='Rozdíl v počtech zaměstnanců'
xlab=''
uu2 <- uu[uu$variable=='Zam_uprMinusskut' & uu$sgrp==F,]
uu2 <- uu2[with(uu2, order(grp)), ]
plot <- ggplot(uu2,aes(x=Year, fill=grp)) +
  geom_bar(stat='identity', aes(y=-value),position='stack') +
  facet_wrap(~KapAbb, scales='free_y') +
  labs(title=title, x=xlab, y=ylab)
plot

# central body pay = real growth, budget and reality
title='Reálný růst platů v ústředních orgánech státu: rozpočet a skutečnost\n
Srovnání s reálným růstem průměrného platu v Praze'
ylab='Změna od roku 2003 očištěná o inflaci'
xlab=''
uu2 <- uu[(uu$variable=='AvgSal_skutecnost' | uu$variable=='AvgSal_upraveny') &
            uu$grp=='UO' & uu$UO & !is.na(uu$value),]
uu2$ggroup <- paste0(uu2$variable,'_',uu2$KapAbb)
plot <- ggplot(uu2,aes(x=Year, y=realchange, colour=variable)) +
  geom_line(size=1) +
  geom_line(aes(y=phasal_all/phasal_all_2003/Infl2003Base)) +
  facet_wrap(~KapAbb) +
  scale_y_continuous(labels=percent) +
  labs(title=title, x=xlab, y=ylab)
plot

# central body pay - comparison with Prague average
title='Průměrný plat v centrálních orgánech státu: rozpočet a skutečnost\n
Srovnání s průměrným platem v Praze'
ylab='\'000 Kč'
xlab=''
uu2 <- uu[(uu$variable=='AvgSal_skutecnost' | uu$variable=='AvgSal_upraveny') &
            uu$grp=='UO' & uu$UO & !is.na(uu$value),]
uu2$ggroup <- paste0(uu2$variable,'_',uu2$KapAbb)
plot <- ggplot(uu2,aes(x=Year, y=value/1000, colour=variable)) +
  geom_line(size=1) +
  geom_line(aes(y=phasal_all/1000),colour='grey',linetype='dashed') +
  facet_wrap(~KapAbb) +
  scale_y_continuous(labels=comma) +
  labs(title=title, x=xlab, y=ylab)
plot

# central body staff numbers over time
title='Počet zaměstnanců v ústředních orgánech státu: rozpočet a skutečnost'
ylab='Počet zaměstnanců'
xlab=''
uu2 <- uu[(uu$variable=='Zam_skutecnost' | uu$variable=='Zam_upraveny') &
            uu$grp=='UO' & !is.na(uu$value) & uu$value!=0 & uu$UO,]
uu2$ggroup <- paste0(uu2$variable,'_',uu2$KapAbb)
plot <- ggplot(uu2,aes(x=Year, y=value, colour=variable)) +
  geom_line(size=1) +
  facet_wrap(~KapAbb, scales='free_y') +
  labs(title=title, x=xlab, y=ylab)
plot

# ministry staff numbers change
title='Počet zaměstnanců v ústředních orgánech státu: rozpočet a skutečnost'
ylab='Počet zaměstnanců'
xlab=''
uu2 <- uu[(uu$variable=='Zam_upraveny') &
            !uu$sgrp & !is.na(uu$value) & uu$value!=0,]
uu2$ggroup <- paste0(uu2$variable,'_',uu2$KapAbb)
plot <- ggplot(uu2,aes(x=Year, y=perc_base, col=grp)) +
  geom_line(size=1) +
  facet_wrap(~KapAbb) +
  scale_y_continuous(labels=percent) +
  labs(title=title, x=xlab, y=ylab)
plot

# exekutiva body staff numbers 
title='Počet zaměstnanců v ústředních orgánech státu: rozpočet a skutečnost'
ylab='Počet zaměstnanců'
xlab=''
uu2 <- uu[(uu$variable=='Zam_skutecnost' | uu$variable=='Zam_upraveny') &
            uu$grp=='Exekutiva' & !is.na(uu$value) & uu$value!=0,]
uu2$ggroup <- paste0(uu2$variable,'_',uu2$KapAbb)
plot <- ggplot(uu2,aes(x=Year, y=value, colour=variable)) +
  geom_line(size=1) +
  facet_wrap(~KapAbb, scales='free_y') +
  labs(title=title, x=xlab, y=ylab)
plot

# Exekutiva staff numbers 2009 & 2012 by chapter 
title='Počet zaměstnanců v exekutivě 2009 a 2012'
ylab='Počet zaměstnanců'
xlab=''
uu2 <- uu[(uu$variable=='Zam_skutecnost') &
            (uu$grp=='Exekutiva') & 
            !is.na(uu$value) & uu$value!=0 & (uu$Year=='2009-01-01' | uu$Year=='2012-01-01'),]
uu2$KapAbb <- factor(uu2$KapAbb, levels=uu2$KapAbb[order(-uu2$value)], ordered=TRUE)
plot <- ggplot(uu2,aes(x=Year, y=value, fill=Year)) +
  geom_bar(stat='identity', position='dodge') +
  facet_wrap(~KapAbb,nrow=2) +
  labs(title=title, x=xlab, y=ylab) +
  scale_x_date(breaks=as.Date(c('2009-01-01','2012-01-01')),labels=date_format('%Y'))
plot

# Exekutiva staff numbers 2009 & 2012 by chapter 
title='Počet zaměstnanců v exekutivě 2006 a 2012'
ylab='Počet zaměstnanců'
xlab=''
uu2 <- uu[(uu$variable=='Zam_skutecnost') &
            (uu$grp=='Exekutiva') & 
            !is.na(uu$value) & uu$value!=0 & (uu$Year=='2009-01-01' | uu$Year=='2012-01-01'),]
uu2$KapAbb <- factor(uu2$KapAbb, levels=uu2$KapAbb[order(-uu2$value)], ordered=TRUE)
plot <- ggplot(uu2,aes(x=Year, y=value, fill=Year)) +
  geom_bar(stat='identity', position='dodge') +
  facet_wrap(~KapAbb,nrow=2) +
  labs(title=title, x=xlab, y=ylab) +
  scale_x_date(breaks=as.Date(c('2009-01-01','2012-01-01')),labels=date_format('%Y'))
plot

# Exekutiva staff numbers 2012 by chapter and grp
title='Počet zaměstnanců v roce 2012: státní správa bez bezpečnostních sborů'
ylab='Počet zaměstnanců (x1000)'
xlab=''
uu2 <- uu[(uu$variable=='Zam_skutecnost') &
#            (uu$grp=='Exekutiva') & 
            (uu$grp=='UO - Ministerstva' | uu$grp=='UO - Ostatní' |
               uu$grp=='OSS-SS') &
            !is.na(uu$value) & uu$value!=0 & (uu$Year=='2012-01-01'),]
uu2$grp <- droplevels(uu2$grp)
uu2$KapAbb <- factor(uu2$KapAbb, levels=uu2$KapAbb[order(-uu2$value)], ordered=TRUE)
plot <- ggplot(uu2,aes(x=KapAbb, y=value/1000, fill=grp)) +
  geom_bar(stat='identity', position='stack') +
  scale_y_continuous()+
  labs(title=title, x=xlab, y=ylab) +
  theme(axis.text.x=element_text(angle=90,hjust=1,size=10,face='bold'),
        axis.ticks.x=element_blank())
plot

# All staff numbers 2012 by chapter and grp
title='Počet zaměstnanců v roce 2012 podle kapitol:
všichni zaměstnanci pod rozpočtovou regulací'
ylab='Počet zaměstnanců (x1000)'
xlab=''
uu2 <- uu[(uu$variable=='Zam_skutecnost') & !uu$sgrp &
            !is.na(uu$value) & uu$value!=0 & (uu$Year=='2012-01-01'),]
uu2$grp <- droplevels(uu2$grp)
uu2$KapAbb <- factor(uu2$KapAbb, levels=uu2$KapAbb[order(-uu2$value)], ordered=TRUE)
plot <- ggplot(uu2,aes(x=KapAbb, y=value/1000, fill=grp)) +
  geom_bar(stat='identity', position='stack') +
  scale_y_continuous()+
  labs(title=title, x=xlab, y=ylab) +
  theme(axis.text.x=element_text(angle=90,hjust=1,size=10,face='bold'),
        axis.ticks.x=element_blank())
plot