# librerías necesarias
library(shiny)
library(shinydashboard)

# cargar los módulos contenidos en R\
lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source)

# interfaz de usuario principal
ui <- dashboardPage(
  dashboardHeader(title = "PIA - DisExp"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Inicio", tabName = "home", icon = icon("home")),
      menuItem("Regresión lineal", tabName = "regresion", icon = icon("chart-line")),
      menuItem("Otros métodos", tabName = "otros_metodos", icon = icon("calculator"))
    )
  ),
  dashboardBody(
    tabItems(
      # pestaña de Inicio
      tabItem(
        tabName = "home",
        h2("Bienvenido al analizador de Métodos Estadísticos"),
        p("Seleccione un método del menú lateral para comenzar."),
        h4("Métodos disponibles"),
        tags$ul(
          tags$li("Regresión lineal: Análisis de relación entre dos variables cuantitativas"),
          tags$li("Más métodos próximamente")
        )
      ),

      #Pestaña de regresión lineal
      tabItem(
        tabName = "regresion",
        # mod_regresion_ui("regresion_1")
      ),

      #pestaña para próximos métodos
      tabItem(
        tabName = "otros_metodos",
        h3("Próximamente"),
        p("Se agregarán más métodos estadísticos a futuro")
      )
    )
  )
)

server <- function(input, output, session) {

}

# función para ejecutar la app
shinyApp(ui = ui, server = server)
