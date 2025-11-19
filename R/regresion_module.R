# lógica del módulo de regresión lineal
#
# @param id Namespace del módulo
# @return Módulo Shiny

#' @importFrom shiny NS tagList numericInput textInput tableOutput verbatimTextOutput plotOutput
#' @importFrom shiny renderTable renderPrint renderPlot
#' @importFrom stats lm cor anova summary.lm coef

# ui para el módulo de regresión

#' @rdname mod_regresion_ui

# conexión con el server y funcionalidad del módulo de regresión
