#' auxiliar
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

atribui_dfs <- function(){

 if(tipo_gas == "CO2" & exists('resultado')){
  txt_unidade <- "Milhões de toneladas de CO2"
  tabela_setores <- resultado$CO2_final
  tabela_comb <- resultado$combustivel_CO2
  grafico_exibir <- resultado$CO2_grafico
  grafico_comb <- resultado$combustivel_CO2
  n_comb_outros <- 8}
else if (tipo_gas == "CH4" & exists('resultado')){
  txt_unidade <- "Mil toneladas de CH4"
  tabela_setores <- resultado$CH4_final
  tabela_comb <- resultado$combustivel_CH4
  grafico_exibir <- resultado$CH4_grafico
  grafico_comb <- resultado$combustivel_CH4
  n_comb_outros <- 12}
else if (tipo_gas == "N2O" & exists('resultado')){
  txt_unidade <- "Mil toneladas de N2O"
  tabela_setores <- resultado$N2O_final
  tabela_comb <- resultado$combustivel_N2O
  grafico_exibir <- resultado$N2O_grafico
  grafico_comb <- resultado$combustivel_N2O
  n_comb_outros <- 12}
else if (tipo_gas == "CO2eq - GWP AR5" & exists('resultado')){
  txt_unidade <- "Milhões de toneladas de CO2eq"
  tabela_setores <- resultado$CO2eq_final
  tabela_comb <- resultado$combustivel_CO2eq
  grafico_exibir <- resultado$CO2eq_grafico
  grafico_comb <- resultado$combustivel_CO2eq
  n_comb_outros <- 12}
else{
  txt_unidade <- NULL
  tabela_setores <- NULL
  tabela_comb <- NULL
  grafico_exibir <- NULL
  grafico_comb <- NULL
}

  objetos <- list(
    txt_unidade = txt_unidade,
    tabela_setores = tabela_setores,
    tabela_comb = tabela_comb,
    grafico_exibir = grafico_exibir,
    grafico_comb = grafico_comb
  )

  return(objetos)

}


