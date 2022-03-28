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

    matrizes <<- read_xlsx(arquivo$datapath) %>%
      remove_empty(., which = c("rows")) %>%
      clean_names()

    ano_inicio <<- input$ano_inicio
    ano_fim <<- input$ano_fim

    anos_decenio <<- tibble(ano = ano_inicio:ano_fim)

    resultado <- calcula()

    observe({

    if(input$tipo_gas == "CO2"){
      txt_unidade <- "Milhões de toneladas de CO2"
      tabela_setores <- resultado$CO2_final
      tabela_comb <- resultado$combustivel_CO2
      grafico_exibir <- resultado$CO2_grafico
      grafico_comb <- resultado$combustivel_CO2
      n_comb_outros <- 8}
    else if (input$tipo_gas == "CH4"){
      txt_unidade <- "Mil toneladas de CH4"
      tabela_setores <- resultado$CH4_final
      tabela_comb <- resultado$combustivel_CH4
      grafico_exibir <- resultado$CH4_grafico
      grafico_comb <- resultado$combustivel_CH4
      n_comb_outros <- 12}
    else if (input$tipo_gas == "N2O"){
      txt_unidade <- "Mil toneladas de N2O"
      tabela_setores <- resultado$N2O_final
      tabela_comb <- resultado$combustivel_N2O
      grafico_exibir <- resultado$N2O_grafico
      grafico_comb <- resultado$combustivel_N2O
      n_comb_outros <- 12}
    else if (input$tipo_gas == "CO2eq - GWP AR5"){
      txt_unidade <- "Milhões de toneladas de CO2eq"
      tabela_setores <- resultado$CO2eq_final
      tabela_comb <- resultado$combustivel_CO2eq
      grafico_exibir <- resultado$CO2eq_grafico
      grafico_comb <- resultado$combustivel_CO2eq
      n_comb_outros <- 12}

      output$ano_selecao <- renderUI({
        sliderInput("ano_selecionado", "Selecione o ano a exibir:", min = ano_inicio, max = ano_fim, value = ano_inicio, step = 1, ticks = FALSE)
      })

    output$unidade_setores <- renderText(txt_unidade)
    output$unidade_comb <- renderText(txt_unidade)

    output$tabela_DT_setores <- renderDT({

      datatable(tabela_setores, rownames = FALSE,
                options = list(
                  buttons = c('excel'),
                  pageLength = 12,
                  dom = 'Bfrtip',
                  scrollX = TRUE,
                  searching = FALSE),
                extensions = 'Buttons')%>%
        formatStyle(0, target = 'row', lineHeight = '70%', fontSize = '80%')
    })

    output$tabela_DT_combustiveis <- renderDT({

      datatable(tabela_comb, rownames = FALSE,
                options = list(
                  buttons = c('excel'),
                  pageLength = 19,
                  dom = 'Bfrtip',
                  scrollX = TRUE),
                extensions = 'Buttons') %>%
        formatStyle(0, target = 'row', lineHeight = '70%', fontSize = '80%')
    })

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
      scale_x_continuous(breaks=seq(ano_inicio, ano_fim, 1))+
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

      posicao <- (ano_selecionado - ano_inicio + 2)

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
        scale_x_continuous(breaks=seq(ano_inicio, ano_fim, 1))+
        labs(title="Intensidade de emissões no consumo de energia", y="kg.CO2eq / tep", x="ano", caption="EPE")
    })

    })
   })
}
