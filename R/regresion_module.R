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

#' @rdname mod_regresion_ui
mod_regresion_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Procesar datos de entrada
    datos_react <- eventReactive(input$btn_calcular, {
      req(input$datos_x, input$datos_y)
      
      # Convertir texto a vector numérico
      x <- as.numeric(unlist(strsplit(gsub(",", " ", input$datos_x), "\\s+")))
      y <- as.numeric(unlist(strsplit(gsub(",", " ", input$datos_y), "\\s+")))
      
      # Validar longitudes
      if (length(x) != length(y)) {
        return(list(error = "Error: Las variables deben tener la misma cantidad de observaciones."))
      }

      # Eliminar NA's
      valid <- !is.na(x) & !is.na(y)
      x <- x[valid]
      y <- y[valid]
      
      if (length(x) < 2) {
        return(list(error = "Error: Se requieren al menos 2 observaciones válidas."))
      }
      
      # Crear data.frame con los datos
      datos_df <- data.frame(x = x, y = y)
      
      # Ajustar modelo de regresión lineal
      modelo <- lm(y ~ x, data = datos_df)
      
      # Obtener resumen del modelo
      resumen <- summary(modelo)
      
      # Obtener ANOVA
      anova_res <- anova(modelo)

      # Calcular correlación
      correlacion <- cor(x, y)
      
      # Obtener coeficientes
      coefs <- coef(modelo)
      
      # Preparar resultados
      list(
        x = x,
        y = y,
        x_name = ifelse(input$var_x == "", "X", input$var_x),
        y_name = ifelse(input$var_y == "", "Y", input$var_y),
        n = length(x),
        modelo = modelo,
        resumen = resumen,
        anova = anova_res,
        coefs = coefs,
        correlacion = correlacion
      )
    })
    
    # Mostrar tabla de datos
    output$tabla_datos <- renderTable({
      datos <- datos_react()
      if (is.null(datos$error)) {
        data.frame(
          Observación = 1:datos$n,
          X = datos$x,
          Y = datos$y
        )
      }
    }, digits = 4)

# ui para el módulo de regresión

#' @rdname mod_regresion_ui

# conexión con el server y funcionalidad del módulo de regresión
