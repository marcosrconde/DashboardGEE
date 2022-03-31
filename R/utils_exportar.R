#' exportar
#'
#' @description
#' Exporta as tabelas úteis geradas pela execução da função 'calcula' para excel
#'
#' @return
#' Arquivo excel contendo as tabelas úteis organizadas
#' @noRd

library(openxlsx)
library(tidyverse)
library(markdown)
library(rmarkdown)
library(htmltools)

exportar <- function(){

resultados <- readRDS('resultado.rds')

for(i in 1:length(resultados)) assign(names(resultados)[i], resultados[[i]])

wb <- createWorkbook(title = 'Resultados - GEE Calc')

addWorksheet(wb = wb, sheetName = 'por Setor')
addWorksheet(wb = wb, sheetName = 'por Combustível')
addWorksheet(wb = wb, sheetName = 'Fugitivas')
addWorksheet(wb = wb, sheetName = 'Intensidade')
addWorksheet(wb = wb, sheetName = 'Indústria')

options("openxlsx.numFmt" = "0.00") # 2 decimal cases formating

n_colunas <- (ncol(total_fugitivas_CO2eq)+2)

setColWidths(wb, sheet = 1, cols = 2, widths = 22)
setColWidths(wb, sheet = 2, cols = 2, widths = 28)
setColWidths(wb, sheet = 3, cols = 2, widths = 22)
setColWidths(wb, sheet = 4, cols = 2:5, widths = 18)
setColWidths(wb, sheet = 5, cols = 2, widths = 26)

writeData(wb, sheet = 1, startRow = 2, startCol = 2, x = "TOTAL - CO2eq - Milhões de toneladas de CO2eq")
writeDataTable(wb, x = CO2eq_final,
               sheet = 1,
               startRow = 3,
               startCol = "B",
               colNames = TRUE,
               rowNames = FALSE,
               withFilter = FALSE,
               tableStyle = 'TableStyleLight2')

writeData(wb, sheet = 1, startRow = 17, startCol = 2, x = "CO2 - Milhões de toneladas de CO2")
writeDataTable(wb, x = CO2_final,
               sheet = 1,
               startRow = 18,
               startCol = "B",
               colNames = TRUE,
               rowNames = FALSE,
               withFilter = FALSE,
               tableStyle = 'TableStyleLight2')

writeData(wb, sheet = 1, startRow = 32, startCol = 2, x = "CH4 - Mil toneladas de CH4")
writeDataTable(wb, x = CH4_final,
               sheet = 1,
               startRow = 33,
               startCol = "B",
               colNames = TRUE,
               rowNames = FALSE,
               withFilter = FALSE,
               tableStyle = 'TableStyleLight2')

writeData(wb, sheet = 1, startRow = 47, startCol = 2, x = "N2O - Mil toneladas de N2O")
writeDataTable(wb, x = N2O_final,
               sheet = 1,
               startRow = 48,
               startCol = "B",
               colNames = TRUE,
               rowNames = FALSE,
               withFilter = FALSE,
               tableStyle = 'TableStyleLight2')

writeData(wb, sheet = 2, startRow = 2, startCol = 2, x = "TOTAL - CO2eq - Milhões de toneladas de CO2eq")
writeDataTable(wb, x = combustivel_CO2eq,
               sheet = 2,
               startRow = 3,
               startCol = "B",
               colNames = TRUE,
               rowNames = FALSE,
               withFilter = FALSE,
               tableStyle = 'TableStyleLight2')

writeData(wb, sheet = 2, startRow = 24, startCol = 2, x = "CO2 - Milhões de toneladas de CO2")
writeDataTable(wb, x = combustivel_CO2,
               sheet = 2,
               startRow = 25,
               startCol = "B",
               colNames = TRUE,
               rowNames = FALSE,
               withFilter = FALSE,
               tableStyle = 'TableStyleLight2')

writeData(wb, sheet = 2, startRow = 42, startCol = 2, x = "CH4 - Mil toneladas de CH4")
writeDataTable(wb, x = combustivel_CH4,
               sheet = 2,
               startRow = 43,
               startCol = "B",
               colNames = TRUE,
               rowNames = FALSE,
               withFilter = FALSE,
               tableStyle = 'TableStyleLight2')

writeData(wb, sheet = 2, startRow = 64, startCol = 2, x = "N2O - Mil toneladas de N2O")
writeDataTable(wb, x = combustivel_N2O,
               sheet = 2,
               startRow = 65,
               startCol = "B",
               colNames = TRUE,
               rowNames = FALSE,
               withFilter = FALSE,
               tableStyle = 'TableStyleLight2')

writeData(wb, sheet = 3, startRow = 2, startCol = 2, x = "TOTAL - CO2eq - Milhões de toneladas de CO2eq")
writeDataTable(wb, x = total_fugitivas_CO2eq,
               sheet = 3,
               startRow = 3,
               startCol = "B",
               colNames = TRUE,
               rowNames = FALSE,
               withFilter = FALSE,
               tableStyle = 'TableStyleLight2')

writeData(wb, sheet = 3, startRow = 10, startCol = 2, x = "CO2 - Milhões de toneladas de CO2")
writeDataTable(wb, x = fugitivas_co2_trans,
               sheet = 3,
               startRow = 11,
               startCol = "B",
               colNames = TRUE,
               rowNames = FALSE,
               withFilter = FALSE,
               tableStyle = 'TableStyleLight2')

writeData(wb, sheet = 3, startRow = 18, startCol = 2, x = "CH4 - Mil toneladas de CH4")
writeDataTable(wb, x = fugitivas_ch4_trans,
               sheet = 3,
               startRow = 19,
               startCol = "B",
               colNames = TRUE,
               rowNames = FALSE,
               withFilter = FALSE,
               tableStyle = 'TableStyleLight2')

writeData(wb, sheet = 3, startRow = 26, startCol = 2, x = "N2O - Mil toneladas de N2O")
writeDataTable(wb, x = fugitivas_n2o_trans,
               sheet = 3,
               startRow = 27,
               startCol = "B",
               colNames = TRUE,
               rowNames = FALSE,
               withFilter = FALSE,
               tableStyle = 'TableStyleLight2')

writeData(wb, sheet = 4, startRow = 2, startCol = 2, x = "Intensidade de emissoes na OIE - kgCO2eq/tep")
writeDataTable(wb, x = intensidade_co2eq,
               sheet = 4,
               startRow = 3,
               startCol = "B",
               colNames = TRUE,
               rowNames = FALSE,
               withFilter = FALSE,
               tableStyle = 'TableStyleLight2')

writeData(wb, sheet = 5, startRow = 2, startCol = 2, x = "TOTAL - CO2eq - Milhões de toneladas de CO2eq")
writeDataTable(wb, x = CO2eq_industrial_trans,
               sheet = 5,
               startRow = 3,
               startCol = "B",
               colNames = TRUE,
               rowNames = FALSE,
               withFilter = FALSE,
               tableStyle = 'TableStyleLight2')

writeData(wb, sheet = 5, startRow = 17, startCol = 2, x = "CO2 - Milhões de toneladas de CO2")
writeDataTable(wb, x = CO2_industrial_trans,
               sheet = 5,
               startRow = 18,
               startCol = "B",
               colNames = TRUE,
               rowNames = FALSE,
               withFilter = FALSE,
               tableStyle = 'TableStyleLight2')

writeData(wb, sheet = 5, startRow = 32, startCol = 2, x = "CH4 - Mil toneladas de CH4")
writeDataTable(wb, x = CH4_industrial_trans,
               sheet = 5,
               startRow = 33,
               startCol = "B",
               colNames = TRUE,
               rowNames = FALSE,
               withFilter = FALSE,
               tableStyle = 'TableStyleLight2')

writeData(wb, sheet = 5, startRow = 47, startCol = 2, x = "N2O - Mil toneladas de N2O")
writeDataTable(wb, x = N2O_industrial_trans,
               sheet = 5,
               startRow = 48,
               startCol = "B",
               colNames = TRUE,
               rowNames = FALSE,
               withFilter = FALSE,
               tableStyle = 'TableStyleLight2')

addStyle(wb, sheet = 4, style = createStyle(numFmt = "#,###.00"), rows = 4:(nrow(intensidade_co2eq)+3), cols = 3:5, gridExpand = TRUE)

arquivo <- saveWorkbook(wb, file = 'resultados.xlsx', overwrite = TRUE)

return(arquivo)
}

