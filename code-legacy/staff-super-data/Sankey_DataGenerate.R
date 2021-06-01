# this code generates a tsv appropriate for plotting a Sankey diagram through

Ministerstva <- 200
OstatniUstredni <- 37
Neustredni <- 468
Ostatni <- 364
Bezpecnost <- 792
Prispevkove <- 2272

Ustredni <- Ministerstva + OstatniUstredni
SSbezSOBCPO <- Ustredni + Neustredni
SSseSOBCPO <- SSbezSOBCPO + Bezpecnost
RO <- SSseSOBCPO + Ostatni
ROPO <- RO + Prispevkove


jmena <- c('ROPO','RO','Neustredni',
           'Ostatni','Bezpecnost','Prispevkove')
parts <- matrix(nrow=6,ncol=7)
parts[1,] <- c('ROPO','PO','PO','PO','PO','PO',Prispevkove)
parts[2,] <- c('ROPO','RO','SSseSOBCPO','SS','UO','Min',Ministerstva)
parts[3,] <- c('ROPO','RO','SSseSOBCPO','SS','UO','OstUstr',OstatniUstredni)
parts[4,] <- c('ROPO','RO','SSseSOBCPO','SS','SOBCPO','SOBCPO',Bezpecnost)
parts[5,] <- c('ROPO','RO','SSseSOBCPO','SS','Neustr','Neustr',Neustredni)
parts[6,] <- c('ROPO','RO','SSseSOBCPO','Ostatni','Ostatni','Ostatni',Ostatni)


sankeydata <- data.frame(Layer1=character(),
                         Layer2=character(),
                         Layer3=character(),
                         Layer4=character(),
                         Layer5=character(),
                         Layer6=character())
                         
for(i in 1:dim(parts)[1]) {
  for(n in 1:parts[i,7]) {
    sankeyrow <- data.frame(Layer1=parts[i,1],
                            Layer2=parts[i,2],
                            Layer3=parts[i,3],
                            Layer4=parts[i,4],
                            Layer5=parts[i,5],
                            Layer6=parts[i,6])
    sankeydata <- rbind(sankeydata,sankeyrow)
  }
}

write.table(sankeydata,'./data-output/sankeydata_generated.tsv',sep='\t',row.names=F,quote=F)
