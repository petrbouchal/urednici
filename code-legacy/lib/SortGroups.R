SortGroups <- function (dataset, sortin, sortbycat, sortbyvar) {
  # orders factor (sortin) by how much of the staff (as counted by sortbyvar)
  # is in each category (sortbycat) - i.e. in effect by the mean grade / age / pay
  gradevalues <- data.frame('sortbyval'=c(1:length(levels(sortbycat))),
                            sortbycat=levels(sortbycat))
  dataset <- merge(dataset,gradevalues)
  xtot <- ddply(dataset,.(Group, Date, sortbycatname),
                summarise,sharewholegrp=sum(share, na.rm=TRUE))
  dataset <- merge(dataset,xtot)
  dataset <- merge(dataset,gradevalues)
  dataset$gradescore <- dataset$gradeval*dataset$sharewholegrp
  xtot <- ddply(dataset,.(Group,Date),summarise,meangradescore=mean(gradescore))
  dataset <- merge(dataset,xtot)
  dataset$sorter <- dataset$meangradescore
  #make Whole CS category go last
  dataset$sorter[dataset$Group=='Whole Civil Service'] <- max(dataset$sorter)*1.1
  #reorder grouping variable
  dataset$Group <- reorder(dataset$Group,dataset$sorter,mean)
  return(dataset)
}