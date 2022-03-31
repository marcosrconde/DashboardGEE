#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    ui <- dashboardPage(

    header =  dashboardHeader(
      title = "GEE Calc",
      tags$li(class = "dropdown",
              tags$a(href="http://www.epe.gov.br", target="_blank",
                     tags$img(height = "20px", alt="Logo EPE", src="https://www.epe.gov.br/PublishingImages/Logos/logo-epe-site.png")
              )
      ),
      leftUi = tagList(
        dropdownButton(
          label = "Opções",
          icon = icon("filter"),
          status = "primary",
          circle = FALSE,
          size = "sm",
          selectInput("tipo_gas", "Escolha o gás a exibir", c("CO2eq - GWP AR5", "CO2", "CH4", "N2O"))
        ))),
    sidebar =  dashboardSidebar(
      sidebarMenu(
        menuItem("Inicializar modelo", tabName = "setup", icon = icon("gears")),
        menuItem("Tabelas de resultados", tabName = "tabelas", icon = icon("table")),
        menuItem("Gráficos", tabName = "graficos", icon = icon("bar-chart-o")),
        tags$hr(style="border-color: grey;")
      )),
    body =  dashboardBody(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "custom.css"
      ),
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Public+Sans&display=swap"
      ),
      tags$style(
        '
        @media (min-width: 768px){
          .sidebar-mini.sidebar-collapse .main-header .logo {
              width: 230px;
          }
          .sidebar-mini.sidebar-collapse .main-header .navbar {
              margin-left: 230px;
          }
        }
        '
      ),
      tabItems(
        tabItem(tabName = "setup",
                div(style="display: inline-block;vertical-align:top",fileInput("entrada", "Dados de entrada - escolha o arquivo", accept = ".xlsx")),
                div(style="display: inline-block;vertical-align:top",numericInput("ano_inicio", "Ano início", value = 2021, width = 100)),
                div(style="display: inline-block;vertical-align:top",numericInput("ano_fim", "Ano final", value = 2031, width = 100)),
                HTML('<br>'),
                actionButton("calcula", "Calcular"),
                tags$hr(style="border-color: grey;"),
                includeMarkdown("instrucoes.md")
        ),
        tabItem(tabName = "tabelas",
                awesomeCheckboxGroup(inputId = 'select_tabelas',
                                     label = 'Escolha as tabelas a exibir',
                                     choices = c('Emissões por setor',
                                                 'Emissões por combustível',
                                                 'Emissões fugitivas',
                                                 'Intensidade de emissões na OIE',
                                                 'Setor industrial'),
                                     selected = c('Emissões por setor', 'Emissões por combustível'),
                                     inline = TRUE
                                     ),
                uiOutput('download_button'),
                br(),
                uiOutput('texto_unidade'),
                br(),
                uiOutput(outputId = 'tabela_setores'),
                uiOutput(outputId = 'tabela_combustiveis'),
                uiOutput(outputId = 'tabela_fugitivas'),
                uiOutput(outputId = 'tabela_intensidade'),
                uiOutput(outputId = 'tabela_industrias')),
        tabItem(tabName = "graficos",
                box(title = "Resultados por setor",
                    width = 12,
                    plotlyOutput("grafico_setores", width = "100%", height = "420px")
                ),
                box(title = "Participação por combustível",
                    width = 6,
                    uiOutput("ano_selecao"),
                    plotOutput("part_comb")
                ),
                box(title = "Intensidade de emissões",
                    width = 6,
                    plotOutput("grafico_intensidade"))
        )
      )
    )
  )
)
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "DashboardGEE"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
