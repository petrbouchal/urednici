CalcAvgsAndIndices <- function (data) {
  
  data$Platy_upr2skut <- data$Platy_upraveny/data$Platy_skutecnost
  data$Zam_upr2skut <- data$Zam_upraveny/data$Zam_skutecnost
  data$OPPP_upr2skut <- data$OPPP_upraveny/data$OPPP_skutecnost
  data$PlatyOPPP_upr2skut <- data$PlatyOPPP_upraveny/data$PlatyOPPP_skutecnost
  
  data$AvgSal_upraveny <- data$Platy_upraveny/data$Zam_upraveny*1000/12
  data$AvgSal_skutecnost <- data$Platy_skutecnost/data$Zam_skutecnost*1000/12
  data$AvgSal_schvaleny <- data$Platy_schvaleny/data$Zam_schvaleny*1000/12
  data$AvgSal_upr2skut <- data$AvgSal_upraveny/data$AvgSal_skutecnost
  data$AvgSal_uprMinusskut <- data$AvgSal_upraveny-data$AvgSal_skutecnost
  
  data$PlatyOPPP_uprMinusskut <- data$PlatyOPPP_upraveny-data$PlatyOPPP_skutecnost
  data$OPPP_uprMinusskut <- data$OPPP_upraveny-data$OPPP_skutecnost
  data$Platy_uprMinusskut <- data$Platy_upraveny-data$Platy_skutecnost
  data$Zam_uprMinusskut <- data$Zam_upraveny-data$Zam_skutecnost
  return(data)
}
