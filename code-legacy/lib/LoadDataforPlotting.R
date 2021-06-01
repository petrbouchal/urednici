LoadDataforPlotting <- function (whatdata) {
  if(whatdata=='chapters') {
    sdatal <- read.csv('./data-output/SuperData_chapters.csv')
    data <- sdatal
    labels <- read.csv('./data-output/KapitolyNames.txt')
    uu <- merge(data,labels)
    data <- uu
  } else if(whatdata=='orgs') {
    sdatal <- read.csv('./data-output/SuperData_orgs_ALL.csv')
    data <- sdatal
    labels <- read.csv('./data-output/KapitolyNames.txt')
    uu <- merge(data,labels)
    data <- uu
  } else {
    stop('whatdata has to be either chapters or orgs')
    uu <- 'error'
  }
  return(uu)
}