#' Global Rscript
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#'
#' @description
#' Lê planilha de fatores de emissões e cria variáveis globais para uso no modelo.
#'
#' @import readxl
#'
#' @noRd


# Lê matriz de fatores de emissão e matriz com as células a calcular
# valores dos fatores de emissão em t.GEE/mil tep
# na planilha celulas o valor 1 ou -1 representa as células a calcular

fe <- read_excel(
  "fatores_emissao.xlsx",
  sheet = "FE"
)
gatilho <- read_excel(
  "fatores_emissao.xlsx",
  sheet = "celulas"
)


# vetores e variáveis importantes -----------------------------------------------------

nomes_colunas <- c("setores", "petroleo", "gas_natural", "carvao_vapor",
                   "carvao_met", "uranio_prim", "hidraulica", "lenha",
                   "produtos_cana", "outras_n_ren", "diesel", "oleo_comb",
                   "gasolina", "glp", "nafta", "querosene", "gas_coqueria",
                   "coque_carvao", "uranio_sec", "eletricidade",
                   "carvao_veg", "etanol", "gas_refinaria", "coque_petroleo",
                   "outros_petroleo", "nao_energeticos", "alcatrao", "total")

setores <- gatilho [ ,1]

GWP_AR5_CH4 <- 28
GWP_AR5_N2O <- 265

Setores_formatado <- tibble(setores = c("SIN", "Autoprodução","Setor Energético",
                                        "Residencial", "Comercial", "Público", "Agropecuário", "Transportes",
                                        "Industrial", "Emissões Fugitivas", "Total"))

formato <-  scales::label_comma(accuracy = 0.01, big.mark = '.', decimal.mark = ",")

values <- reactiveValues()

values$status <- 'inicio'
