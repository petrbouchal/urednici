source('./src/lib/lib_PubFinCZ.R')

sheetnum <- 8
sheetname <- 'OSSSSjednot'

years <- c(2003:2013)

sdata0 <- read.xlsx('./data-input/MFCR_SuperData_OSSSSjednotlive.xls',sheetnum,
                    startRow=5,endRow=85,colIndex=c(3:248))
names(sdata0) <- paste0('var',1:ncol(sdata0))
sdata0$sheetname <- sheetname
str(sdata0)

sdata <- sdata0

varsrepeating <- c('PlatyOPPP','OPPP','Platy','Zam','AvgSal','AvGSalRank')
varsonceayear1 <- c('AvgSalIndexSchv2schv_None')
varsonceayear2 <- c('AvgSalIndexSkut2uprav_None','AvgSalIndexSkut2skut_None','Blank')

oneyearofnames <- c(paste0(varsrepeating,'_schvaleny'),varsonceayear1,
                    paste0(varsrepeating,'_upraveny'),
                    paste0(varsrepeating,'_skutecnost'),
                    varsonceayear2)
for(year in years) {
  allnames0 <- paste0(oneyearofnames,'.',year)
  if(year>2003) {
    allnames <- c(allnames, allnames0)
  } else {
    allnames <- allnames0
  }
}
leftheaders <- c('KapNum','KapAbb','OrgName','Kap_Blank')
allnames <- c(leftheaders,allnames,'sheetname')
names(sdata) <- allnames

# Create long dataset -----------------------------------------------------

sdatal <- melt(sdata,id=c('KapNum','KapAbb','OrgName','sheetname'))
sdatal <- sdatal[sdatal$variable!='Kap_Blank',]
for(year in years) {
  sdatal <- sdatal[sdatal$variable!=paste0('Blank.',year),]
}
splitvars <- colsplit(sdatal$variable,fixed('.'),c('Var','Year'))
sdatal <- cbind(sdatal,splitvars)
splitvars <- colsplit(sdatal$Var,'_',c('Udaj','BudgetStage'))
sdatal <- cbind(sdatal,splitvars)
table(sdatal$Udaj)
table(sdatal$Year)
table(sdatal$BudgetStage)
table(sdatal$sheetname)

# clean data
sdatal$Year <- as.Date(paste(sdatal$Year,'01','01',sep='-'),format='%Y-%m-%d')
sdatal$OrgName <- str_trim(sdatal$OrgName)
sdatal$KapAbb[sdatal$KapNum==353] <- 'ÚOHS'
sdatal$OrgName[sdatal$OrgName=='C e l k e m'] <- 'Celkem'

sdatal$value <- as.numeric(sdatal$value)
sdatal$variable <- NULL
sdatal$Var <- NULL

sdatal$emptychapter <- is.na(sdatal$KapNum)
sdatal <- sdatal[sdatal$emptychapter!=TRUE,]
unique(sdatal$OrgName)

# add TRUE/FALSE ministry marker - made in excel --------------------------

sdatal$KapAbb <- NULL
sdatal$emptychapter <- NULL
kapmin <- read.csv('./data-input/KapitolyMinisterstva.txt')
sdatal <- merge(sdatal,kapmin, all.x=TRUE)
unique(sdatal$OrgName)

# Write data and labels - separately to save space -------------------------

write.csv(sdatal,'./data-output/SuperData_orgs_ALL.csv',row.names=FALSE)

unique(sdatal$OrgName)
