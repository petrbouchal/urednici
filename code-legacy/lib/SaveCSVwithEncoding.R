SaveCSVwithEncoding <- function (data, path, encoding='WINDOWS-1252') {
  dataenc <- data
  iconvlist()
  sapply( df, function(x) if("factor" %in% class(x) | "character" %in% class(x))  {
    iconv(x, to=encoding) } else {x})
  write.csv(dataenc,file=path,row.names=FALSE,fileEncoding=encoding)
  write.csv(dataenc,file=path,row.names=FALSE)
}
