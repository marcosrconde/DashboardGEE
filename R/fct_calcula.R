#' calcula
#'
#' @description
#' Calcula as emissões de gases de efeito estufa a partir da matriz
#'consolidada produzida pela SEE em formato excel. A função primeiramente organiza
#'as informações da planilha em um dataframe e depois procede os cálculos, gerando
#'tabelas úteis para análises dos resultados.
#' O arquivo excel é importado após o usuário clicar no botão calcular - código
#'de importação no script app_server.R.
#' Os fatores de emissão são importados no script global.R
#' @return
#'Retorna uma lista com as tabelas úteis para análises das emissões de GEE nos
#'planos de energia
#' @noRd


calcula <- function(){

  matriz_todas <- values$matrizes %>%
    select(
      -energia_primaria_total,
      -energia_secundaria_total,
      -outras_secundarias_de_petroleo,
      -outras_fontes_primarias,
      -outras_renovaveis_mil_tep,
      -c(29,33)
    ) %>%
    relocate(
      "gas_de_refinaria_mil_tep",
      "coque_de_petroleo_mil_tep",
      "outros_energeticos_de_petroleo_mil_tep",
      .before = "produtos_nao_energeticos_de_petroleo"
    ) %>%
    relocate(
      "outras_nao_renovaveis_mil_tep",
      .before = "oleo_diesel"
    )

  names(matriz_todas) <- nomes_colunas

  matriz_todas <- matriz_todas %>%
    filter(.,
           petroleo != "PETRÓLEO",
           setores != "Ajustes"
    )

  matrizes_final <- matriz_todas[ ,2:28] %>%
    type.convert(as.is = TRUE, dec = ".") %>%
    round(digits=2)

  df_matrizes <- data.frame(setores, matrizes_final)

  n_linhas <- nrow(df_matrizes)
  n_matrizes <- n_linhas / 41
  n_colunas <- n_matrizes + 1

  tryCatch(if(nrow(values$anos_decenio) != n_matrizes) {

    message("número de matrizes não corresponde")

    showNotification("Preencha corretamente o ano inicio e ano fim!", duration = 10, type = "error",
                     action = a(href = "javascript:location.reload();", "Reiniciar o modelo"))

  })

  # constrói tabelas uteis -----------------------------------------

  # constrói vetor contendo a oferta interna de energia
  oferta_interna <- select(
    filter(df_matrizes, setores == "oferta_interna_bruta"),
    c(total)
  )

  oferta_interna <- data.frame(values$anos_decenio, oferta_interna)
  names(oferta_interna) <- c("ano", "oie")



  # constrói vetor contendo a produção de carvão vapor

  producao_carvao <- select(
    filter(df_matrizes, setores == "producao"),
    c(carvao_vapor)
  )

  producao_carvao <- data.frame(values$anos_decenio, producao_carvao)
  names(producao_carvao) <- c("ano", "carvao_vapor")

  # constrói vetor contendo a produção de petroleo

  producao_petroleo <- select(
    filter(df_matrizes, setores == "producao"),
    c(petroleo)
  )

  producao_petroleo <- data.frame(values$anos_decenio, producao_petroleo)
  names(producao_petroleo) <- c("ano", "petroleo")

  # constrói vetor contendo a produção de gás natural

  producao_gas <- select(
    filter(df_matrizes, setores == "producao"),
    c(gas_natural)
  )

  producao_gas <- data.frame(values$anos_decenio, producao_gas)
  names(producao_gas) <- c("ano", "gas")

  # constrói vetor contendo a o refino de petróleo

  refino_oleo <- select(
    filter(df_matrizes, setores == "refinarias_de_petroleo"),
    c(petroleo)
  ) %>%
    mutate(petroleo = petroleo * -1)

  refino_oleo <- data.frame(values$anos_decenio, refino_oleo)
  names(refino_oleo) <- c("ano", "refino")

  # Matrizes com os fatores de emissão --------------------------------------
  # Constrói tibbles com os fatores de emissão de GEE (CO2, CH4 e N2O)

  fe_co2 <- select(
    filter(fe, fatores_emissao == "co2"),
    everything()
  )

  fe_ch4 <- select(
    filter(fe, fatores_emissao == "ch4"),
    everything()
  )

  fe_n2o <- select(
    filter(fe, fatores_emissao == "n2o"),
    everything()
  )

  matriz_co2 <- fe_co2 %>%
    slice(rep(row_number(), n_linhas))
  matriz_ch4 <- fe_ch4 %>%
    slice(rep(row_number(), n_linhas))
  matriz_n2o <- fe_n2o %>%
    slice(rep(row_number(), n_linhas))



  # Cálculo das emissões fugitivas ------------------------------------------

  # fugitivas do petróleo e gás

  fugitivas <- read_excel(
    "fatores_emissao.xlsx",
    sheet = "fugitivas"
  )

  fugitivas_2010 <- fugitivas %>%
    subset(
      ano == 2010
    )

  oleo_2010 <- fugitivas_2010[1,6]
  gas_2010 <- fugitivas_2010[1,7]
  refino_2010 <- fugitivas_2010[1,8]
  transporte_2010 <- fugitivas_2010[1,9]
  co2_oleo_2010 <- fugitivas_2010[3,3]
  co2_refino_2010 <- fugitivas_2010[2,3]
  co2_transporte_2010 <- fugitivas_2010[1,3]
  ch4_oleo_2010 <- fugitivas_2010[3,4]
  ch4_refino_2010 <- fugitivas_2010[2,4]
  ch4_transporte_2010 <- fugitivas_2010[1,4]
  n2o_oleo_2010 <- fugitivas_2010[3,5]
  n2o_refino_2010 <- fugitivas_2010[2,5]
  n2o_transporte_2010 <- fugitivas_2010[1,5]


  producao_carvao$toneladas <- producao_carvao$carvao_vapor * 2850

  producao_carvao$ch4 <- (producao_carvao$toneladas * 0.46 * 0.00000023)+(producao_carvao$toneladas * 0.54 * 0.0000073)
  producao_carvao$co2 <- producao_carvao$toneladas * 0.00012 / 1000


  transporte_oleo <- producao_petroleo + refino_oleo
  transporte_oleo$ano <- values$anos_decenio

  producao_petroleo <- producao_petroleo %>%
    mutate(var2010 = producao_petroleo$petroleo / as.numeric(oleo_2010[1,1]))

  refino_oleo <- refino_oleo %>%
    mutate(var2010 = refino_oleo$refino / as.numeric(refino_2010[1,1]))

  f_co2_producao <- function(x){
    var_co2 = x * 0.9927 - 0.0249
  }

  producao_petroleo <- producao_petroleo %>%
    mutate(var_co2 = as.numeric(map(.f = f_co2_producao, .x = producao_petroleo$var2010)))

  f_co2_refino <- function(x){
    var_co2 = x * 1.3121 - 0.3341
  }

  refino_oleo <- refino_oleo %>%
    mutate(var_co2 = as.numeric(map(.f = f_co2_refino, .x = refino_oleo$var2010)))

  producao_petroleo$co2 <- producao_petroleo$var_co2 * as.numeric(co2_oleo_2010[1,1])/ 10^6
  producao_petroleo$ch4 <- producao_petroleo$var_co2 * as.numeric(ch4_oleo_2010[1,1]) / 1000
  producao_petroleo$n2o <- producao_petroleo$var_co2 * as.numeric(n2o_oleo_2010[1,1]) / 1000

  refino_oleo$co2 <- refino_oleo$var_co2 * as.numeric(co2_refino_2010[1,1]) / 10^6
  refino_oleo$ch4 <- refino_oleo$var_co2 * as.numeric(ch4_refino_2010[1,1]) / 1000
  refino_oleo$n2o <- refino_oleo$var_co2 * as.numeric(n2o_refino_2010[1,1]) / 1000

  fe_co2_transporte <- co2_transporte_2010 / transporte_2010
  fe_ch4_transporte <- ch4_transporte_2010 / transporte_2010
  fe_n2o_transporte <- n2o_transporte_2010 / transporte_2010

  transporte_oleo$co2 <- transporte_oleo$petroleo * as.numeric(fe_co2_transporte/10^6)
  transporte_oleo$ch4 <- transporte_oleo$petroleo * as.numeric(fe_ch4_transporte/1000)
  transporte_oleo$n2o <- transporte_oleo$petroleo * as.numeric(fe_n2o_transporte/1000)


  fugitivas_co2 <- cbind(values$anos_decenio, producao_petroleo$co2, refino_oleo$co2, transporte_oleo$co2, producao_carvao$co2) %>%
    adorn_totals(., c("col"))
  names(fugitivas_co2) <- c("ano", "E&P", "Refino", "Transporte", "Carvão", "Total")


  fugitivas_co2_trans <- fugitivas_co2 %>%
    transpose(., keep.names = "ano")
  names(fugitivas_co2_trans) <- c("setores", values$ano_inicio:values$ano_fim)

  fugitivas_co2_trans <- fugitivas_co2_trans[2:5,] %>%
    adorn_totals(where = c("row"), name = "Emissões Fugitivas")


  fugitivas_ch4 <- cbind(values$anos_decenio, producao_petroleo$ch4, refino_oleo$ch4, transporte_oleo$ch4, producao_carvao$ch4)%>%
    adorn_totals(., c("col"))
  names(fugitivas_ch4) <- c("ano", "E&P", "Refino", "Transporte", "Carvão", "Total")


  fugitivas_ch4_trans <- fugitivas_ch4 %>%
    transpose(., keep.names = "ano")
  names(fugitivas_ch4_trans) <- c("setores", values$ano_inicio:values$ano_fim)

  fugitivas_ch4_trans <- fugitivas_ch4_trans[2:5,] %>%
    adorn_totals(where = c("row"), name = "Emissões Fugitivas")


  fugitivas_n2o <- cbind(values$anos_decenio, producao_petroleo$n2o, refino_oleo$n2o, transporte_oleo$n2o)%>%
    adorn_totals(., c("col"))
  names(fugitivas_n2o) <- c("ano", "E&P", "Refino", "Transporte", "Total")


  fugitivas_n2o_trans <- fugitivas_n2o %>%
    transpose(., keep.names = "ano")

  names(fugitivas_n2o_trans) <- c("setores", values$ano_inicio:values$ano_fim)
  fugitivas_n2o_trans <- fugitivas_n2o_trans[2:4,] %>%
    adorn_totals(where = c("row"), name = "Emissões Fugitivas")

  fugitivas_ch4_GWP <- mutate_if(fugitivas_ch4_trans, is.numeric, ~ . * GWP_AR5_CH4/1000)
  fugitivas_n2o_GWP <- mutate_if(fugitivas_n2o_trans, is.numeric, ~ . * GWP_AR5_N2O/1000)

  total_fugitivas_CO2eq <- rbind(fugitivas_co2_trans, fugitivas_ch4_GWP, fugitivas_n2o_GWP) %>%
    group_by(setores) %>%
    summarise(., across(where(is.numeric), sum))

  total_fugitivas_CO2eq <- total_fugitivas_CO2eq[c(2,4,5,1,3),]

  fugitivas_co2_grafico <- fugitivas_co2 %>%
    select(., 'ano', 'Total') %>%
    mutate(setores = "Emissões Fugitivas") %>%
    relocate(., 'setores', .before = 'Total')

  fugitivas_ch4_grafico <- fugitivas_ch4 %>%
    select(., 'ano', 'Total') %>%
    mutate(setores = "Emissões Fugitivas") %>%
    relocate(., 'setores', .before = 'Total')

  fugitivas_n2o_grafico <- fugitivas_n2o %>%
    select(., 'ano', 'Total') %>%
    mutate(setores = "Emissões Fugitivas") %>%
    relocate(., 'setores', .before = 'Total')

  fugitivas_ch4_GWP_grafico <- fugitivas_ch4_grafico %>%
    mutate(Total_CH4_GWP = fugitivas_ch4_grafico[,3] * 28 / 1000) %>%
    select(., -Total) %>%
    rename(., 'Total' = 'Total_CH4_GWP')

  fugitivas_n2o_GWP_grafico <- fugitivas_n2o_grafico %>%
    mutate(Total_N2O_GWP = fugitivas_n2o_grafico[,3] * 265 / 1000)%>%
    select(., -Total) %>%
    rename(., 'Total' = 'Total_N2O_GWP')

  Fugitivas_co2eq_grafico <- cbind(fugitivas_co2_grafico, fugitivas_ch4_GWP_grafico[,3], fugitivas_n2o_GWP_grafico[,3]) %>%
    adorn_totals(., c("col"), name = 'Total_CO2eq')

  Fugitivas_co2eq_grafico <- Fugitivas_co2eq_grafico[,c(1,2,6)] %>%
    rename(., 'Total' = 'Total_CO2eq')


  # Cálculo de emissões de CO2 ----------------------------------------------
  # Calcula emissões de CO2 para cada ano do decenio, setor e combustível

  #replica a matriz com as celulas a calcular pelo número de matrizes
  celulas_calcular <- gatilho[ ,2:27] %>%
    slice(rep(row_number(), n_matrizes))

  #calcula as emissões de CO2, adiciona coluna de total por setor,
  #organiza os resultados e adiciona a coluna com os anos das matrizes

  calculo_co2_matrizes <- (df_matrizes[ , 2:27] * celulas_calcular * matriz_co2 [ ,2:27]) / 10^6
  calculo_co2_matrizes <- calculo_co2_matrizes %>%
    adorn_totals(c("col"))
  calculo_co2_matrizes <- data.frame(setores,calculo_co2_matrizes) %>%
    subset(., Total > 0) %>%
    group_by(setores) %>%
    arrange(setores)
  calculo_co2_matrizes <- data.frame(values$anos_decenio, calculo_co2_matrizes)

  CO2 <- calculo_co2_matrizes %>%
    group_by(ano, setores) %>%
    summarise(., Total = sum(Total), .groups = "rowwise")

  CO2_industrial <- CO2 %>%
    subset(.,
           setores == 'cimento'|
             setores == 'ferro_gusa_e_aco'|
             setores == 'ferroligas'|
             setores == 'mineracao_e_pelotizacao'|
             setores == 'nao_ferrosos'|
             setores == 'quimica'|
             setores == 'alimentos_e_bebidas'|
             setores == 'textil'|
             setores == 'papel_e_celulose'|
             setores == 'ceramica'|
             setores == 'outros'
    ) %>%
    group_by(ano) %>%
    arrange(ano) %>%
    summarise(., Total = sum(Total), .groups = "rowwise") %>%
    mutate(setores = "industrial") %>%
    relocate(., 'setores', .before = 'Total')

  # Prepara tabela com emissões do setor industrial para apresentação final

  CO2_industrial_trans <- CO2 %>%
    subset(.,setores == 'cimento'|
             setores == 'ferro_gusa_e_aco'|
             setores == 'ferroligas'|
             setores == 'mineracao_e_pelotizacao'|
             setores == 'nao_ferrosos'|
             setores == 'quimica'|
             setores == 'alimentos_e_bebidas'|
             setores == 'textil'|
             setores == 'papel_e_celulose'|
             setores == 'ceramica'|
             setores == 'outros'
             ) %>%
    group_by(ano, setores) %>%
    summarise(., Total = sum(Total)) %>%
    pivot_wider(., names_from = ano, values_from = Total) %>%
    adorn_totals(., where = 'row')

  CO2_industrial_trans[,1] <- c(
    'Alimentos e bebidas',
    'Cerâmica',
    'Cimento',
    'Ferro gusa e aço',
    'Ferroligas',
    'Mineração e pelotização',
    'Não ferrosos',
    'Outros',
    'Papel e celulose',
    'Química',
    'Têxtil',
    'Total'
  )

  CO2_industrial_trans <- CO2_industrial_trans[c(1:7,9:11,8,12),]

  CO2_parcial<- CO2 %>%
    subset(.,
           setores == 'comercial'|
             setores == 'residencial'|
             setores == 'publico'|
             setores == 'agropecuario'|
             setores == 'transportes'|
             setores == 'centrais_eletricas_sp'|
             setores == 'centrais_eletricas_ap'|
             setores == 'setor_energetico')

  CO2_grafico <- rbind(CO2_parcial, CO2_industrial,fugitivas_co2_grafico) %>%
    group_by(ano) %>%
    arrange(ano) %>%
    mutate(setores = str_replace(setores, "agropecuario", "Agropecuário"),
           setores = str_replace(setores, "centrais_eletricas_ap", "Eletricidade - Autoprodução"),
           setores = str_replace(setores, "centrais_eletricas_sp", "Eletricidade - SIN"),
           setores = str_replace(setores, "comercial", "Comercial"),
           setores = str_replace(setores, "industrial", "Industrial"),
           setores = str_replace(setores, "publico", "Público"),
           setores = str_replace(setores, "residencial", "Residencial"),
           setores = str_replace(setores, "setor_energetico", "Setor Energético"),
           setores = str_replace(setores, "transportes", "Transportes"))

  CO2_final <- CO2_grafico %>%
    pivot_wider(., names_from = ano, values_from = Total)

  CO2_final <- CO2_final[c(2,3,7,6,4,5,1,8,9,10),] %>%
    adorn_totals(c("row"))

  CO2_final_num <- round(CO2_final[ ,2:n_colunas], digits = 2)

  CO2_final <- cbind(Setores_formatado, CO2_final_num) %>%
    rename(
      Setores = setores
    )

  CO2_eletrico_total <- CO2_final[1:2, ] %>%
    adorn_totals(c("row"), name = "Setor Elétrico") %>%
    subset(., Setores == "Setor Elétrico")

  CO2_final <- rbind(CO2_eletrico_total, CO2_final)


  # Cálculo de emissões de CH4 ----------------------------------------------

  #calcula as emissões de CH4, adiciona coluna de total por setor,
  #organiza os resultados e adiciona a coluna com os anos das matrizes

  calculo_ch4_matrizes <- (df_matrizes[ , 2:27] * celulas_calcular * matriz_ch4 [ ,2:27]) / 10^6
  calculo_ch4_matrizes <- calculo_ch4_matrizes %>%
    adorn_totals(c("col"))
  calculo_ch4_matrizes <- data.frame(setores,calculo_ch4_matrizes) %>%
    subset(., Total > 0) %>%
    group_by(setores) %>%
    arrange(setores)
  calculo_ch4_matrizes <- data.frame(values$anos_decenio, calculo_ch4_matrizes)

  CH4 <- calculo_ch4_matrizes %>%
    group_by(ano, setores) %>%
    summarise(., Total = sum(Total)*1000, .groups = "rowwise")

  CH4_industrial <- CH4 %>%
    subset(.,
           setores == 'cimento'|
             setores == 'ferro_gusa_e_aco'|
             setores == 'ferroligas'|
             setores == 'mineracao_e_pelotizacao'|
             setores == 'nao_ferrosos'|
             setores == 'quimica'|
             setores == 'alimentos_e_bebidas'|
             setores == 'textil'|
             setores == 'papel_e_celulose'|
             setores == 'ceramica'|
             setores == 'outros'
    ) %>%
    group_by(ano) %>%
    arrange(ano) %>%
    summarise(., Total = sum(Total), .groups = "rowwise") %>%
    mutate(setores = "industrial") %>%
    relocate(., 'setores', .before = 'Total')

  # Prepara tabela com emissões do setor industrial para apresentação final

  CH4_industrial_trans <- CH4 %>%
    subset(.,setores == 'cimento'|
             setores == 'ferro_gusa_e_aco'|
             setores == 'ferroligas'|
             setores == 'mineracao_e_pelotizacao'|
             setores == 'nao_ferrosos'|
             setores == 'quimica'|
             setores == 'alimentos_e_bebidas'|
             setores == 'textil'|
             setores == 'papel_e_celulose'|
             setores == 'ceramica'|
             setores == 'outros'
    ) %>%
    group_by(ano, setores) %>%
    summarise(., Total = sum(Total)) %>%
    pivot_wider(., names_from = ano, values_from = Total) %>%
    adorn_totals(., where = 'row')

  CH4_industrial_trans[,1] <- c(
    'Alimentos e bebidas',
    'Cerâmica',
    'Cimento',
    'Ferro gusa e aço',
    'Ferroligas',
    'Mineração e pelotização',
    'Não ferrosos',
    'Outros',
    'Papel e celulose',
    'Química',
    'Têxtil',
    'Total'
  )

  CH4_industrial_trans <- CH4_industrial_trans[c(1:7,9:11,8,12),]

  CH4_energetico <- CH4 %>%
    subset(.,
           setores == 'setor_energetico'|
             setores == 'carvoarias'
    ) %>%
    group_by(ano) %>%
    arrange(ano) %>%
    summarise(., Total = sum(Total), .groups = "rowwise") %>%
    mutate(setores = "setor_energetico") %>%
    relocate(., 'setores', .before = 'Total')

  CH4_parcial<- CH4 %>%
    subset(.,
           setores == 'comercial'|
             setores == 'residencial'|
             setores == 'publico'|
             setores == 'agropecuario'|
             setores == 'transportes'|
             setores == 'centrais_eletricas_sp'|
             setores == 'centrais_eletricas_ap'
    )

  CH4_grafico <- rbind(CH4_parcial, CH4_energetico, CH4_industrial, fugitivas_ch4_grafico) %>%
    group_by(ano) %>%
    arrange(ano) %>%
    mutate(setores = str_replace(setores, "agropecuario", "Agropecuário"),
           setores = str_replace(setores, "centrais_eletricas_ap", "Eletricidade - Autoprodução"),
           setores = str_replace(setores, "centrais_eletricas_sp", "Eletricidade - SIN"),
           setores = str_replace(setores, "comercial", "Comercial"),
           setores = str_replace(setores, "industrial", "Industrial"),
           setores = str_replace(setores, "publico", "Público"),
           setores = str_replace(setores, "residencial", "Residencial"),
           setores = str_replace(setores, "setor_energetico", "Setor Energético"),
           setores = str_replace(setores, "transportes", "Transportes"))

  CH4_final <- CH4_grafico %>%
    pivot_wider(., names_from = ano, values_from = Total)

  CH4_final <- CH4_final[c(3,2,8,6,4,5,1,7,9,10),] %>%
    adorn_totals(c("row"))

  CH4_final_num <- round(CH4_final[ ,2:n_colunas], digits = 2)

  CH4_final <- cbind(Setores_formatado, CH4_final_num) %>%
    rename(
      Setores = setores
    )

  CH4_eletrico_total <- CH4_final[1:2, ] %>%
    adorn_totals(c("row"), name = "Setor Elétrico") %>%
    subset(., Setores == "Setor Elétrico")

  CH4_final <- rbind(CH4_eletrico_total, CH4_final) #resultados em mil t. CH4


  # Cálculo de emissões de N2O ----------------------------------------------

  #calcula as emissões de N2O, adiciona coluna de total por setor,
  #organiza os resultados e adiciona a coluna com os anos das matrizes

  calculo_n2o_matrizes <- (df_matrizes[ , 2:27] * celulas_calcular * matriz_n2o [ ,2:27]) / 10^6
  calculo_n2o_matrizes <- calculo_n2o_matrizes %>%
    adorn_totals(c("col"))
  calculo_n2o_matrizes <- data.frame(setores,calculo_n2o_matrizes) %>%
    subset(., Total > 0) %>%
    group_by(setores) %>%
    arrange(setores)
  calculo_n2o_matrizes <- data.frame(values$anos_decenio, calculo_n2o_matrizes)

  N2O <- calculo_n2o_matrizes %>%
    group_by(ano, setores) %>%
    summarise(., Total = sum(Total)*1000, .groups = "rowwise")

  N2O_industrial <- N2O %>%
    subset(.,
           setores == 'cimento'|
             setores == 'ferro_gusa_e_aco'|
             setores == 'ferroligas'|
             setores == 'mineracao_e_pelotizacao'|
             setores == 'nao_ferrosos'|
             setores == 'quimica'|
             setores == 'alimentos_e_bebidas'|
             setores == 'textil'|
             setores == 'papel_e_celulose'|
             setores == 'ceramica'|
             setores == 'outros'
    ) %>%
    group_by(ano) %>%
    arrange(ano) %>%
    summarise(., Total = sum(Total), .groups = "rowwise") %>%
    mutate(setores = "industrial") %>%
    relocate(., 'setores', .before = 'Total')

  # Prepara tabela com emissões do setor industrial para apresentação final

  N2O_industrial_trans <- N2O %>%
    subset(.,setores == 'cimento'|
             setores == 'ferro_gusa_e_aco'|
             setores == 'ferroligas'|
             setores == 'mineracao_e_pelotizacao'|
             setores == 'nao_ferrosos'|
             setores == 'quimica'|
             setores == 'alimentos_e_bebidas'|
             setores == 'textil'|
             setores == 'papel_e_celulose'|
             setores == 'ceramica'|
             setores == 'outros'
    ) %>%
    group_by(ano, setores) %>%
    summarise(., Total = sum(Total)) %>%
    pivot_wider(., names_from = ano, values_from = Total) %>%
    adorn_totals(., where = 'row')

  N2O_industrial_trans[,1] <- c(
    'Alimentos e bebidas',
    'Cerâmica',
    'Cimento',
    'Ferro gusa e aço',
    'Ferroligas',
    'Mineração e pelotização',
    'Não ferrosos',
    'Outros',
    'Papel e celulose',
    'Química',
    'Têxtil',
    'Total'
  )

  N2O_industrial_trans <- N2O_industrial_trans[c(1:7,9:11,8,12),]

  N2O_energetico <- N2O %>%
    subset(.,
           setores == 'setor_energetico'|
             setores == 'carvoarias'
    ) %>%
    group_by(ano) %>%
    arrange(ano) %>%
    summarise(., Total = sum(Total), .groups = "rowwise") %>%
    mutate(setores = "setor_energetico") %>%
    relocate(., 'setores', .before = 'Total')

  N2O_parcial<- N2O %>%
    subset(.,
           setores == 'comercial'|
             setores == 'residencial'|
             setores == 'publico'|
             setores == 'agropecuario'|
             setores == 'transportes'|
             setores == 'centrais_eletricas_sp'|
             setores == 'centrais_eletricas_ap'
    )

  N2O_grafico <- rbind(N2O_parcial, N2O_energetico, N2O_industrial, fugitivas_n2o_grafico) %>%
    group_by(ano) %>%
    arrange(ano) %>%
    mutate(setores = str_replace(setores, "agropecuario", "Agropecuário"),
           setores = str_replace(setores, "centrais_eletricas_ap", "Eletricidade - Autoprodução"),
           setores = str_replace(setores, "centrais_eletricas_sp", "Eletricidade - SIN"),
           setores = str_replace(setores, "comercial", "Comercial"),
           setores = str_replace(setores, "industrial", "Industrial"),
           setores = str_replace(setores, "publico", "Público"),
           setores = str_replace(setores, "residencial", "Residencial"),
           setores = str_replace(setores, "setor_energetico", "Setor Energético"),
           setores = str_replace(setores, "transportes", "Transportes"))

  N2O_final <- N2O_grafico %>%
    pivot_wider(., names_from = ano, values_from = Total)

  N2O_final <- N2O_final[c(3,2,8,6,4,5,1,7,9,10),] %>%
    adorn_totals(c("row"))

  N2O_final_num <- round(N2O_final[ ,2:n_colunas], digits = 2)

  N2O_final <- cbind(Setores_formatado, N2O_final_num) %>%
    rename(
      Setores = setores
    )

  N2O_eletrico_total <- N2O_final[1:2, ] %>%
    adorn_totals(c("row"), name = "Setor Elétrico") %>%
    subset(., Setores == "Setor Elétrico")

  N2O_final <- rbind(N2O_eletrico_total, N2O_final)



  # Cálculo de CH4 para CO2eq ----------------------------------------------------------

  CH4_GWP_AR5 <- mutate_if(calculo_ch4_matrizes, is.numeric, ~ . * GWP_AR5_CH4)
  CH4_GWP_AR5$ano <- NULL
  CH4_GWP_AR5 <- data.frame(values$anos_decenio,CH4_GWP_AR5)

  # Cálculo de N2O para CO2eq ----------------------------------------------------------

  N2O_GWP_AR5 <- mutate_if(calculo_n2o_matrizes, is.numeric, ~ . * GWP_AR5_N2O)
  N2O_GWP_AR5$ano <- NULL
  N2O_GWP_AR5 <- data.frame(values$anos_decenio,N2O_GWP_AR5)


  # Monta matriz de resultados por setor em CO2eq ---------------------------

  CO2eq <- bind_rows(calculo_co2_matrizes, CH4_GWP_AR5, N2O_GWP_AR5) %>%
    group_by(ano, setores) %>%
    summarise(., Total = sum(Total), .groups = "rowwise")

  # Gera tabela de emissões totais do setor industrial
  CO2eq_industrial <- CO2eq %>%
    subset(.,
           setores == 'cimento'|
             setores == 'ferro_gusa_e_aco'|
             setores == 'ferroligas'|
             setores == 'mineracao_e_pelotizacao'|
             setores == 'nao_ferrosos'|
             setores == 'quimica'|
             setores == 'alimentos_e_bebidas'|
             setores == 'textil'|
             setores == 'papel_e_celulose'|
             setores == 'ceramica'|
             setores == 'outros'
    ) %>%
    group_by(ano) %>%
    arrange(ano) %>%
    summarise(., Total = sum(Total), .groups = "rowwise") %>%
    mutate(setores = "industrial") %>%
    relocate(., 'setores', .before = 'Total')

  # Prepara tabela com emissões do setor industrial para apresentação final

  CO2eq_industrial_trans <- CO2eq %>%
    subset(.,setores == 'cimento'|
             setores == 'ferro_gusa_e_aco'|
             setores == 'ferroligas'|
             setores == 'mineracao_e_pelotizacao'|
             setores == 'nao_ferrosos'|
             setores == 'quimica'|
             setores == 'alimentos_e_bebidas'|
             setores == 'textil'|
             setores == 'papel_e_celulose'|
             setores == 'ceramica'|
             setores == 'outros'
    ) %>%
    group_by(ano, setores) %>%
    summarise(., Total = sum(Total)) %>%
    pivot_wider(., names_from = ano, values_from = Total) %>%
    adorn_totals(., where = 'row')

  CO2eq_industrial_trans[,1] <- c(
    'Alimentos e bebidas',
    'Cerâmica',
    'Cimento',
    'Ferro gusa e aço',
    'Ferroligas',
    'Mineração e pelotização',
    'Não ferrosos',
    'Outros',
    'Papel e celulose',
    'Química',
    'Têxtil',
    'Total'
  )

  CO2eq_industrial_trans <- CO2eq_industrial_trans[c(1:7,9:11,8,12),]

  CO2eq_energetico <- CO2eq %>%
    subset(.,
           setores == 'setor_energetico'|
             setores == 'carvoarias'
    ) %>%
    group_by(ano) %>%
    arrange(ano) %>%
    summarise(., Total = sum(Total), .groups = "rowwise") %>%
    mutate(setores = "setor_energetico") %>%
    relocate(., 'setores', .before = 'Total')

  CO2eq_parcial<- CO2eq %>%
    subset(.,
           setores == 'comercial'|
             setores == 'residencial'|
             setores == 'publico'|
             setores == 'agropecuario'|
             setores == 'transportes'|
             setores == 'centrais_eletricas_sp'|
             setores == 'centrais_eletricas_ap')

  CO2eq_grafico <- rbind(CO2eq_parcial, CO2eq_energetico, CO2eq_industrial, Fugitivas_co2eq_grafico) %>%
    group_by(ano) %>%
    arrange(ano) %>%
    mutate(setores = str_replace(setores, "agropecuario", "Agropecuário"),
           setores = str_replace(setores, "centrais_eletricas_ap", "Eletricidade - Autoprodução"),
           setores = str_replace(setores, "centrais_eletricas_sp", "Eletricidade - SIN"),
           setores = str_replace(setores, "comercial", "Comercial"),
           setores = str_replace(setores, "industrial", "Industrial"),
           setores = str_replace(setores, "publico", "Público"),
           setores = str_replace(setores, "residencial", "Residencial"),
           setores = str_replace(setores, "setor_energetico", "Setor Energético"),
           setores = str_replace(setores, "transportes", "Transportes"))

  # pivoteia matriz de resultados  para mostrar anos nas colunas, ajusta ordem dos
  # setores na tabela adicionando linha de total, muda formato do número para 1
  # casa decimal e adiciona o setor elétrico

  CO2eq_final <- CO2eq_grafico %>%
    pivot_wider(., names_from = ano, values_from = Total)

  CO2eq_final <- CO2eq_final[c(3,2,8,6,4,5,1,7,9,10),] %>%
    adorn_totals(c("row"))

  CO2_eq_final_num <- round(CO2eq_final[ ,2:n_colunas], digits = 2)

  CO2eq_final <- cbind(Setores_formatado, CO2_eq_final_num) %>%
    rename(
      Setores = setores
    )

  CO2eq_eletrico_total <- CO2eq_final[1:2, ] %>%
    adorn_totals(c("row"), name = "Setor Elétrico") %>%
    subset(., Setores == "Setor Elétrico")

  CO2eq_final <- rbind(CO2eq_eletrico_total, CO2eq_final)


  # Cria DF para grafico de intensidade de emissões -------------------------

  total_CO2 <- CO2 %>%
    group_by(ano) %>%
    summarise(Total_CO2 = sum(Total))

  total_CO2 <- cbind(total_CO2, fug_CO2 = fugitivas_co2[,6]) %>%
    mutate(., Total = Total_CO2 + fug_CO2) %>%
    select(., ano, total_CO2 = Total)

  total_CH4 <- CH4 %>%
    group_by(ano) %>%
    summarise(Total_CH4 = sum(Total))

  total_CH4 <- cbind(total_CH4, fug_CH4 = fugitivas_ch4[,6]) %>%
    mutate(., Total = Total_CH4 + fug_CH4) %>%
    select(., ano, total_CH4 = Total)

  total_N2O <- N2O %>%
    group_by(ano) %>%
    summarise(Total_N2O = sum(Total))

  total_N2O <- cbind(total_N2O, fug_N2O = fugitivas_n2o[,4]) %>%
    mutate(., Total = Total_N2O + fug_N2O) %>%
    select(., ano, total_N2O = Total)

  total_CO2eq <- CO2eq %>%
    group_by(ano) %>%
    summarise(Total_CO2eq = sum(Total))

  total_CO2eq <- cbind(total_CO2eq, fug_CO2eq = Fugitivas_co2eq_grafico[,3]) %>%
    mutate(., Total = Total_CO2eq + fug_CO2eq) %>%
    select(., ano, total_CO2eq = Total)

  intensidade_co2eq <- cbind(oferta_interna, emissao = total_CO2eq[,2]) %>%
    mutate(intensidade = (emissao/oie)*1000000)


    # Monta matriz de emissões por tipo de combustível ------------------------

  # Matriz de combustível para emissão de CO2

  combustivel_CO2 <- aggregate(calculo_co2_matrizes[,3:28], list(calculo_co2_matrizes$ano), FUN=sum) %>%
    transpose(keep.names = "Group.1")

  names(combustivel_CO2) <- c("Combustível", values$ano_inicio:values$ano_fim)

  combustivel_CO2 <- combustivel_CO2 [2:27,] %>%
    adorn_totals(c("row","col")) %>%
    subset(., Total > 0) %>%
    select(
      -Total
    )

  nomes_combustiveis <- c(
    "Gás Natural",
    "Carvão Vapor",
    "Outras não renováveis",
    "Diesel",
    "Óleo Combustível",
    "Gasolina",
    "GLP",
    "Querosene",
    "Gás de coqueria",
    "Coque de carvão mineral",
    "Gás de refinaria",
    "Coque de petróleo",
    "Outros derivados de petróleo",
    "Alcatrão")

  combustivel_CO2[,1] <- c(nomes_combustiveis, "Total")
  combustivel_CO2[,2:n_colunas]  <- combustivel_CO2[,2:n_colunas] %>%
    round(digits = 2)

  # Matriz de combustível para emissão de CH4

  combustivel_CH4 <- aggregate(calculo_ch4_matrizes[,3:28], list(calculo_ch4_matrizes$ano), FUN=sum) %>%
    transpose(keep.names = "Group.1")

  names(combustivel_CH4) <- c("Combustível", values$ano_inicio:values$ano_fim)

  combustivel_CH4 <- combustivel_CH4 [2:27,] %>%
    adorn_totals(c("row","col")) %>%
    subset(., Total > 0) %>%
    select(
      -Total
    )

  nomes_combustiveis_CH4 <- c(
    "Gás Natural",
    "Carvão Vapor",
    "Lenha",
    "Produtos da cana",
    "Outras não renováveis",
    "Diesel",
    "Óleo Combustível",
    "Gasolina",
    "GLP",
    "Querosene",
    "Gás de coqueria",
    "Coque de carvão",
    "Carvão vegetal",
    "Etanol",
    "Gás de refinaria",
    "Coque de petróleo",
    "Outros derivados de petróleo",
    "Alcatrão")

  combustivel_CH4[,1] <- c(nomes_combustiveis_CH4, "Total")
  combustivel_CH4[,2:n_colunas] <- combustivel_CH4[,2:n_colunas]*1000
  combustivel_CH4[,2:n_colunas]  <- combustivel_CH4[,2:n_colunas] %>%
    round(digits = 2)

  # Matriz de combustível para emissão de N2O

  combustivel_N2O <- aggregate(calculo_n2o_matrizes[,3:28], list(calculo_n2o_matrizes$ano), FUN=sum) %>%
    transpose(keep.names = "Group.1")

  names(combustivel_N2O) <- c("Combustível", values$ano_inicio:values$ano_fim)

  combustivel_N2O <- combustivel_N2O [2:27,] %>%
    adorn_totals(c("row","col")) %>%
    subset(., Total > 0) %>%
    select(
      -Total
    )

  nomes_combustiveis_N2O <- c(
    "Gás Natural",
    "Carvão Vapor",
    "Lenha",
    "Produtos da cana",
    "Outras não renováveis",
    "Diesel",
    "Óleo Combustível",
    "Gasolina",
    "GLP",
    "Querosene",
    "Gás de coqueria",
    "Coque de carvão",
    "Carvão vegetal",
    "Etanol",
    "Gás de refinaria",
    "Coque de petróleo",
    "Outros derivados de petróleo",
    "Alcatrão")

  combustivel_N2O[,1] <- c(nomes_combustiveis_N2O, "Total")
  combustivel_N2O[,2:n_colunas] <- combustivel_N2O[,2:n_colunas]*1000
  combustivel_N2O[,2:n_colunas]  <- combustivel_N2O[,2:n_colunas] %>%
    round(digits = 2)


  # Matriz de combustível para emissão em CO2eq - GWP AR5

  CO2eq_combustivel <- bind_rows(calculo_co2_matrizes, CH4_GWP_AR5, N2O_GWP_AR5) %>%
    group_by(ano)

  combustivel_CO2eq <- aggregate(CO2eq_combustivel[,3:28], list(CO2eq_combustivel$ano), FUN=sum) %>%
    transpose(keep.names = "Group.1")

  names(combustivel_CO2eq) <- c("Combustível", values$ano_inicio:values$ano_fim)

  combustivel_CO2eq <- combustivel_CO2eq [2:27,] %>%
    adorn_totals(c("row","col")) %>%
    subset(., Total > 0) %>%
    select(
      -Total
    )

  nomes_combustiveis_CO2eq <- c(
    "Gás Natural",
    "Carvão Vapor",
    "Lenha",
    "Produtos da cana",
    "Outras não renováveis",
    "Diesel",
    "Óleo Combustível",
    "Gasolina",
    "GLP",
    "Querosene",
    "Gás de coqueria",
    "Coque de carvão",
    "Carvão vegetal",
    "Etanol",
    "Gás de refinaria",
    "Coque de petróleo",
    "Outros derivados de petróleo",
    "Alcatrão")

  combustivel_CO2eq[,1] <- c(nomes_combustiveis_CO2eq, "Total")
  combustivel_CO2eq[,2:n_colunas]  <- combustivel_CO2eq[,2:n_colunas] %>%
    round(digits = 2)


# Emite mensagem de sucesso ---------------------------------------------------------

  showModal(modalDialog(
    title = "Sucesso!",
    footer = modalButton(label = "Ok"),
    size = "s",
    "As emissões foram calculadas. Acesse as páginas de resultado no menu à esquerda."
  ))


# Constrói lista de resultado final ---------------------------------------

  resultado <- list(
    CO2_final = CO2_final,
    combustivel_CO2 = combustivel_CO2,
    CO2_grafico = CO2_grafico,
    CH4_final = CH4_final,
    combustivel_CH4 = combustivel_CH4,
    CH4_grafico = CH4_grafico,
    N2O_final = N2O_final,
    combustivel_N2O = combustivel_N2O,
    N2O_grafico = N2O_grafico,
    CO2eq_final = CO2eq_final,
    combustivel_CO2eq = combustivel_CO2eq,
    CO2eq_grafico = CO2eq_grafico,
    intensidade_co2eq = intensidade_co2eq,
    fugitivas_ch4_GWP = fugitivas_ch4_GWP,
    fugitivas_ch4_trans = fugitivas_ch4_trans,
    fugitivas_co2_trans = fugitivas_co2_trans,
    fugitivas_n2o_GWP = fugitivas_n2o_GWP,
    fugitivas_n2o_trans = fugitivas_n2o_trans,
    total_fugitivas_CO2eq = total_fugitivas_CO2eq,
    CO2_industrial_trans = CO2_industrial_trans,
    CH4_industrial_trans = CH4_industrial_trans,
    N2O_industrial_trans = N2O_industrial_trans,
    CO2eq_industrial_trans = CO2eq_industrial_trans
  )

  values$status <- 'rodado'

  saveRDS(object = resultado, file = 'resultado.rds')

  exportar()

  return(resultado)

}
