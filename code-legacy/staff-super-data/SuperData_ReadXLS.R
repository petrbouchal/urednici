source('./src/lib/lib_PubFinCZ.R')

# create dataset of sheet names and numbers to loop through ---------------

sheetnums <- c(1:7,9)
sheetnames <- c('ROPO celkem',
                'PO',
                'OSS-RO',
                'OOSS',
                'StatniSprava',
                'UO',
                'OSS-SS',
                'SOBCPO')

sheetset <- data.frame('sheetnums'=sheetnums, 'sheetnames'=sheetnames)
years <- c(2003:2013)

# load data from excel file by looping through sheets and years -----------

for(i in sheetset$sheetnums){
  print(i)
  sdata0 <- read.xlsx('./data-input/MFCR_SuperData.xls',i,
                     startRow=5,endRow=46,colIndex=c(3:248))
  names(sdata0) <- paste0('var',1:ncol(sdata0))
  sdata0$sheetname <- sheetset$sheetname[sheetset$sheetnum==i]
  str(sdata0)
  if(i>1) {
    sdata <- rbind(sdata,sdata0)
  } else {
    sdata <- sdata0
  }
  str(sdata)
}

# create & apply variable names -------------------------------------------

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
leftheaders <- c('KapNum','KapAbb','KapName','Kap_Blank')
allnames <- c(leftheaders,allnames,'sheetname')
names(sdata) <- allnames

# Create long dataset -----------------------------------------------------

sdatal <- melt(sdata,id=c('KapNum','KapAbb','KapName','sheetname'))
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

# Clean data a bit --------------------------------------------------------

sdatal$Year <- as.Date(paste(sdatal$Year,'01','01',sep='-'),format='%Y-%m-%d')
sdatal$KapName <- str_trim(sdatal$KapName)
sdatal$KapAbb[sdatal$KapNum==353] <- 'ÚOHS'
sdatal$KapName[sdatal$KapNum==353] <- 'Úřad na ochranu hospodářské soutěže'
sdatal$KapName[sdatal$KapNum==376] <- 'Generální inspekce bezpečnostních sborů'
sdatal$KapName[sdatal$KapNum==355] <- 'Ústav pro studium totalitních režimů'
table(sdatal$KapAbb)
table(sdatal$KapName)
table(sdatal$KapNum)

# add TRUE/FALSE ministry marker - made in excel --------------------------

kapmin <- read.csv('./data-input/KapitolyMinisterstva.txt')
sdatal <- merge(sdatal,kapmin)

# Write data and labels - separately to save space -------------------------

#write.csv(sdatal,'./data-output/SuperData_ALL.csv')
namestable <- unique(sdatal[,c(1:3,11)])
sdatal <- sdatal[c(1,4,6,8:10)]

write.csv(sdatal,'./data-output/SuperData_chapters.csv',row.names=FALSE)
write.csv(namestable,'./data-output/KapitolyNames.txt',row.names=FALSE)