source('./src/lib/lib_PubFinCZ.R')

# Assumptions on top management salaries & staff numbers, estimating from LN data
toppocet <- 150
topplat <- 70000
toppersonplat <- 344000

onlyonedept <- F
ministerstvo <- 'ÚV'

# uu0 <- LoadDataforPlotting('chapters')
ff <- uu0
ff <- ff[ff$Udaj=='Zam' & ff$Year=='2011-01-01' & ff$BudgetStage=='rozpocet' &
           ff$Ministerstvo==TRUE & ff$sheetname=='UO',]
pocetlidi <- sum(ff$value, na.rm=T)
if(onlyonedept) {
  ff <- ff[ff$Udaj=='Zam' & ff$Year=='2011-01-01' & ff$BudgetStage=='skutecnost' &
             ff$Ministerstvo==TRUE & ff$sheetname=='UO' & ff$KapAbb==ministerstvo,]
  pocetlidi <- sum(ff$value, na.rm=T)
}

ff <- uu0
ff <- ff[ff$Udaj=='Platy' & ff$Year=='2011-01-01' & ff$BudgetStage=='skutecnost' &
           ff$Ministerstvo==TRUE & ff$sheetname=='UO',]
platysuma <- sum(ff$value, na.rm=T)
if(onlyonedept) {
  ff <- ff[ff$Udaj=='Platy' & ff$Year=='2011-01-01' & ff$BudgetStage=='skutecnost' &
             ff$Ministerstvo==TRUE & ff$sheetname=='UO' & ff$KapAbb==ministerstvo,]
  platysuma <- sum(ff$value, na.rm=T)
}

# checking the totals from departmental data
topplat <- 75000
if(onlyonedept) {
  topsals <- read.csv('./data-input/topplaty.csv')
  topplat <- mean(topsals$Plat[topsals$Min==ministerstvo]*1000)
  toppersonplat <- max(topsals$Plat[topsals$Min==ministerstvo]*1000)
  toppocet <- length(topsals$Plat[topsals$Min==ministerstvo]*1000)
}

prumernyplat <- platysuma/pocetlidi*1000/12

platysuma
pocetlidi
prumernyplat
topplat
toppersonplat

# Mean salary excluding top managers
prumplatbeztop <- (platysuma*1000-toppocet*topplat*12)/(pocetlidi-toppocet)/12
rozdil <- prumplatbeztop-prumernyplat
rozdilpc <- rozdil/prumernyplat
prumernyplat
prumplatbeztop
rozdil
rozdilpc

# top x% of people takes y% of the pot
y <- topplat*toppocet*12/1000/platysuma*100
x <- toppocet/pocetlidi*100

x
y

topplat/prumernyplat
toppersonplat/prumernyplat
