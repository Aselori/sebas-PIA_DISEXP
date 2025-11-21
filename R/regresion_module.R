# lógica del módulo de regresión lineal
#
# @param id Namespace del módulo
# @return Módulo Shiny

#' @importFrom shiny NS tagList numericInput textInput tableOutput verbatimTextOutput plotOutput
#' @importFrom shiny renderTable renderPrint renderPlot
#' @importFrom stats lm cor anova summary.lm coef

mod_regresion_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Regresión Lineal Simple"),

    # Paso 1: Ingreso de datos
    wellPanel(
      h4("Paso 1: Ingresar Datos"),
      textInput(ns("var_x"), "Nombre de la variable independiente (X):", ""),
      textInput(ns("var_y"), "Nombre de la variable dependiente (Y):", ""),
      textAreaInput(ns("datos_x"), "Valores de X (separados por comas o espacios):"),
      textAreaInput(ns("datos_y"), "Valores de Y (separados por comas o espacios):"),
      actionButton(ns("btn_calcular"), "Calcular Regresión")
    ),

    # Paso 2: Validación de datos
    conditionalPanel(
      condition = "input.btn_calcular > 0",
      ns = ns,
      wellPanel(
        h4("Paso 2: Validación de Datos"),
        tableOutput(ns("tabla_datos")),
        verbatimTextOutput(ns("validacion"))
      )
    ),

    # Paso 3: Resultados
    conditionalPanel(
      condition = "input.btn_calcular > 0",
      ns = ns,
      wellPanel(
        h4("Paso 3: Resultados"),

        ### NUEVO: Sección para el gráfico ###
        h5("Gráfico de Dispersión y Recta de Ajuste"),
        plotOutput(ns("grafico")),
        hr(),
        ######################################

        h5("Estadísticas Descriptivas"),
        verbatimTextOutput(ns("estadisticas")),

        h5("Coeficientes de Correlación"),
        verbatimTextOutput(ns("correlacion")),

        h5("Ecuación de Regresión"),
        verbatimTextOutput(ns("ecuacion")),

        h5("Análisis de Varianza (ANOVA)"),
        verbatimTextOutput(ns("anova"))
      )
    )
  )
}

# ui para el módulo de regresión

#' @rdname mod_regresion_ui

# conexión con el server y funcionalidad del módulo de regresión
