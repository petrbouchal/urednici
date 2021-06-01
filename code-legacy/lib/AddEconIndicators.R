AddEconIndicators <- function (data) {
  data$Year <- as.Date(data$Year)
  deflators <- read.csv('./data-input/deflators.txt')
  deflators$Year <- as.Date(deflators$Year)
  czsal <- read.csv('./data-input/czsalaries.csv')
  czsal$Year <- as.Date(czsal$Year)
  dataout <- merge(data, deflators)
  dataout <- merge(dataout, czsal)
  return(dataout)
}