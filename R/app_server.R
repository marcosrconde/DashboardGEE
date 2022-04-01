#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
  # Your application server logic


    observeEvent(input$calcula, {

    arquivo <- input$entrada

    values$matrizes <-  read_xlsx(arquivo$datapath) %>%
      remove_empty(., which = c("rows")) %>%
      clean_names()

    values$ano_inicio <- input$ano_inicio
    values$ano_fim <- input$ano_fim

    values$anos_decenio <- tibble(ano = values$ano_inicio:values$ano_fim)

    resultado <- calcula()

    observe({

    if(input$tipo_gas == "CO2"){
      txt_unidade <- "Milhões de toneladas de CO2"
      tabela_setores <- resultado$CO2_final
      tabela_comb <- resultado$combustivel_CO2
      tabela_fugitivas <- resultado$fugitivas_co2_trans
      tabela_intensidade <- resultado$intensidade_co2eq
      tabela_industrial <- resultado$CO2_industrial_trans
      grafico_exibir <- resultado$CO2_grafico
      grafico_comb <- resultado$combustivel_CO2
      n_comb_outros <- 8}
    else if (input$tipo_gas == "CH4"){
      txt_unidade <- "Mil toneladas de CH4"
      tabela_setores <- resultado$CH4_final
      tabela_comb <- resultado$combustivel_CH4
      tabela_fugitivas <- resultado$fugitivas_ch4_trans
      tabela_intensidade <- resultado$intensidade_co2eq
      tabela_industrial <- resultado$CH4_industrial_trans
      grafico_exibir <- resultado$CH4_grafico
      grafico_comb <- resultado$combustivel_CH4
      n_comb_outros <- 12}
    else if (input$tipo_gas == "N2O"){
      txt_unidade <- "Mil toneladas de N2O"
      tabela_setores <- resultado$N2O_final
      tabela_comb <- resultado$combustivel_N2O
      tabela_fugitivas <- resultado$fugitivas_n2o_trans
      tabela_intensidade <- resultado$intensidade_co2eq
      tabela_industrial <- resultado$N2O_industrial_trans
      grafico_exibir <- resultado$N2O_grafico
      grafico_comb <- resultado$combustivel_N2O
      n_comb_outros <- 12}
    else if (input$tipo_gas == "CO2eq - GWP AR5"){
      txt_unidade <- "Milhões de toneladas de CO2eq"
      tabela_setores <- resultado$CO2eq_final
      tabela_comb <- resultado$combustivel_CO2eq
      tabela_fugitivas <- resultado$total_fugitivas_CO2eq
      tabela_intensidade <- resultado$intensidade_co2eq
      tabela_industrial <- resultado$CO2eq_industrial_trans
      grafico_exibir <- resultado$CO2eq_grafico
      grafico_comb <- resultado$combustivel_CO2eq
      n_comb_outros <- 12}

      output$ano_selecao <- renderUI({
        sliderInput("ano_selecionado", "Selecione o ano a exibir:", min = values$ano_inicio, max = values$ano_fim, value = values$ano_inicio, step = 1, ticks = FALSE)
      })

    output$tabela_setores <- renderUI({

      selecao_tabelas <- input$select_tabelas

      if('Emissões por setor' %in% selecao_tabelas) {
      box(title = "Resultados por setor", width = 12,
          renderDT({
            datatable(tabela_setores, rownames = FALSE,
                                 options = list(
                                   pageLength = 12,
                                   dom = 'rtip',
                                   scrollX = TRUE,
                                   searching = FALSE)
                                 ) %>%
                         formatStyle(0, target = 'row', lineHeight = '70%', fontSize = '80%')
            }))}
      })

    output$tabela_combustiveis <- renderUI({

      selecao_tabelas <- input$select_tabelas

      if("Emissões por combustível" %in% selecao_tabelas){

      box(title = "Resultados por combustível", width = 12,
      renderDT({
         datatable(tabela_comb, rownames = FALSE,
                options = list(
                  pageLength = 19,
                  dom = 'tip',
                  scrollX = TRUE)
                ) %>%
        formatStyle(0, target = 'row', lineHeight = '70%', fontSize = '80%')
    }))
    }
  })

    output$tabela_fugitivas <- renderUI({

      selecao_tabelas <- input$select_tabelas

      n_colunas <- ncol(tabela_fugitivas)

      if("Emissões fugitivas" %in% selecao_tabelas){

        box(title = "Emissões fugitivas", width = 12,
            renderDT({
              datatable(tabela_fugitivas, rownames = FALSE,
                        options = list(
                          columnDefs = list(list(className = 'dt-right', targets = 1:(n_colunas-1))),
                          dom = 'tip',
                          scrollX = TRUE)
                        ) %>%
                formatStyle(0, target = 'row', lineHeight = '70%', fontSize = '80%') %>%
                formatRound(., columns = 2:n_colunas, digits = 2, mark = '.', dec.mark = ',', interval = 3)
            }))
      }
    })

    output$tabela_intensidade <- renderUI({

      selecao_tabelas <- input$select_tabelas

      names(tabela_intensidade) <- c('Ano', 'OIE (mil tep)', 'Emissão (t.CO2eq)', 'Intensidade (kg.CO2eq/tep)')

      if("Intensidade de emissões na OIE" %in% selecao_tabelas){

        box(title = "Intensidade de emissões na oferta interna de energia", width = 12,
            renderDT({
              datatable(tabela_intensidade, rownames = FALSE,
                        options = list(
                          columnDefs = list(list(className = "dt-left", targets = 0)),
                          paging = FALSE,
                          dom = 'tip',
                          scrollY = TRUE,
                          scrollX = TRUE)
                          ) %>%
                formatStyle(0, target = 'row', lineHeight = '70%', fontSize = '80%') %>%
                formatStyle(., columns = 1, `text-align` = "left") %>%
                formatRound(., columns = 2:4, digits = 2, mark = ".", dec.mark = ",", interval = 3)
            }))
      }
    })

    output$tabela_industrias <- renderUI({

      selecao_tabelas <- input$select_tabelas

      n_colunas <- ncol(tabela_industrial)

      if('Setor industrial' %in% selecao_tabelas) {
        box(title = "Detalhamento: Setor industrial", width = 12,
            renderDT({
              datatable(tabela_industrial, rownames = FALSE,
                        options = list(
                          pageLength = 12,
                          dom = 'tip',
                          scrollX = TRUE,
                          searching = FALSE)
                        ) %>%
                formatStyle(0, target = 'row', lineHeight = '70%', fontSize = '80%') %>%
                formatRound(., columns = 2:n_colunas, digits = 2, interval = 3, mark = '.', dec.mark = ',')
            }))}
    })

    output$unidade_setores <- renderText(txt_unidade)

    output$texto_unidade <- renderUI({

      selecao_tabelas <- input$select_tabelas

      if(!is.null(selecao_tabelas)){
        span("Exibindo: ", input$tipo_gas, br(), div(style = 'display: inline-block;', h5("Unidade: ")),
             div(style = 'display: inline-block;', h5(textOutput("unidade_setores"))))
      }
    })

    output$download_button <- renderUI({
      if (values$status == 'rodado') {
        downloadButton(outputId = 'download_excel', label = "Exportar resultados para Excel")
      }
    })

    output$download_excel <- downloadHandler(filename = 'resultados.xlsx',
                                             content = function(arquivo){file.copy('resultados.xlsx', arquivo)},
                                             contentType = 'xlsx')

    max_eixo_y <- tabela_setores %>%
      select(
        last_col()) %>%
      max()

    escala_eixo_y <- (max_eixo_y %/% 100 * 100)+200

    ggplot_grafico_comb <- grafico_exibir %>%
      ggplot(., aes(x = ano, y = Total, fill= setores)) +
      geom_col(width = 0.5) +
      theme_bw()+
      theme(legend.position = "bottom", legend.title = element_blank(),
            legend.text = element_text(size = 10), axis.text.x=element_text(size=10, angle = 45, vjust=.5),
            axis.text.y=element_text(size=10))+
      scale_x_continuous(breaks=seq(values$ano_inicio, values$ano_fim, 1))+
      scale_y_continuous(breaks=seq(0, escala_eixo_y, by=100))+
      scale_fill_brewer(palette="Paired", direction = -1) +
      labs(title="Emissões de gases de efeito estufa", subtitle=txt_unidade, y="Total", x="ano", caption="EPE")

    ggplotly_grafico_comb <- ggplotly(
      ggplot_grafico_comb,) %>%
      layout(title = list(text = paste0('Emissões de gases de efeito estufa',
                                        '<br>',
                                        '<sup>',
                                        txt_unidade)),
             legend =list(font = list(size = 10), title=list(text='<b>Setores</b>')))

    output$grafico_setores <- renderPlotly(ggplotly_grafico_comb)

    output$part_comb <- renderPlot({

      ano_selecionado <- input$ano_selecionado

      posicao <- (ano_selecionado - values$ano_inicio + 2)

      comb_grafico <- grafico_comb[,c(1,posicao)]

      names(comb_grafico) <- c("combustivel","escolha")

      comb_grafico <- comb_grafico %>%
        arrange(desc(escolha)) %>%
        slice_head(., n=7) %>%
        subset(
          combustivel != 'Total'
        )

      comb_outros <- grafico_comb[,c(1,posicao)]

      names(comb_outros) <- c("combustivel","escolha")

      comb_outros <- comb_outros %>%
        arrange(desc(escolha)) %>%
        slice_tail(n=n_comb_outros) %>%
        subset(combustivel != 'Total') %>%
        adorn_totals(c("row"), name = 'Outros') %>%
        subset(combustivel == 'Outros')

      comb_grafico <- rbind(comb_grafico, comb_outros) %>%
        mutate(part = percent((escolha / sum(escolha)), accuracy = 0.1, decimal.mark = ","))

      ggplot(comb_grafico, aes(x=2, y=escolha, fill=fct_inorder(combustivel)))+
        geom_col(position = 'stack', width = 1, color = "white")+
        coord_polar(theta = "y", start=200)+
        theme_minimal()+
        theme(
          axis.text = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          panel.border = element_blank(),
          panel.grid=element_blank(),
          axis.ticks = element_blank(),
          legend.text = element_text(size = 12)
        )+
        geom_text(aes(label = part, x = 2), size = 4, position = position_stack(vjust = 0.5)) +
        scale_fill_brewer(palette="Paired")+
        xlim(.2,2.5)
    })

    output$grafico_intensidade <- renderPlot({
      ggplot(resultado$intensidade_co2eq, aes(x=ano, y=intensidade))+
        geom_line(colour = "#4f77b8", size = 1)+
        theme_bw()+
        theme(axis.text.x=element_text(size=12, angle = 45, vjust=.5), axis.title.y=element_text(size = 14), plot.title = element_text(size = 14))+
        scale_x_continuous(breaks=seq(values$ano_inicio, values$ano_fim, 1))+
        labs(title="Intensidade de emissões no consumo de energia", y="kg.CO2eq / tep", x="ano", caption="EPE")
    })

    })
   })
}
