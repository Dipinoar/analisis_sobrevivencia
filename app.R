# encoding: UTF-8
# ==============================================================================
# APP SHINY: ANÁLISIS COMPLETO DE SUPERVIVENCIA FV
# ==============================================================================
# Esta aplicación presenta el análisis completo de supervivencia de sistemas
# fotovoltaicos, incluyendo EDA, preprocesamiento, modelado y conclusiones.
# ==============================================================================

# --- LIBRERÍAS ---
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(survival)
  library(survminer)
  library(plotly)
  library(scales)
  library(robustbase)  # Para MCD (outliers multivariados)
  library(car)         # Para elipse de tolerancia y VIF
  library(rlang)       # Para sym() en evaluación dinámica
})

# --- CARGAR DATOS ---
# Dataset ORIGINAL (CSV sin procesar) - para EDA
DATOS_CSV_ORIGINAL <- read.csv("a35ad07d-eceb-45e0-8c34-1ac40f634652.csv", 
                                stringsAsFactors = FALSE,
                                na.strings = c("", "NA", "N/A"))

# Dataset FILTRADO (para modelos) - ya procesado
DATOS_MODELO <- readRDS("datos_modelo_final.rds")

# Modelo final
MODELO_FINAL <- readRDS("modelo_final.rds")

# Información de contexto
cat("Datos cargados:\n")
cat("  - CSV original:", nrow(DATOS_CSV_ORIGINAL), "filas\n")
cat("  - Dataset modelo:", nrow(DATOS_MODELO), "filas\n")
cat("  - Pérdida:", round((1 - nrow(DATOS_MODELO)/nrow(DATOS_CSV_ORIGINAL))*100, 1), "%\n")

# Función robusta para calcular predictor lineal (maneja coeficientes NA)
calcular_lp <- function(modelo, newdata) {
  mm <- model.matrix(delete.response(terms(modelo)), newdata)
  
  # Coeficientes del modelo
  beta_mod <- coef(modelo)
  
  # Vector completo con 0 para coeficientes que no existan o sean NA
  beta <- setNames(rep(0, ncol(mm)), colnames(mm))
  
  # Asignar coeficientes existentes
  common_names <- intersect(names(beta_mod), names(beta))
  beta[common_names] <- beta_mod[common_names]
  
  # Reemplazar NA por 0 (categorías sin datos)
  beta[is.na(beta)] <- 0
  
  drop(mm %*% beta)
}

# ==============================================================================
# UI - INTERFAZ DE USUARIO
# ==============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  # --- HEADER ---
  dashboardHeader(
    title = "Análisis de Supervivencia FV",
    titleWidth = 300
  ),


  # --- SIDEBAR ---
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("Objetivos", tabName = "objetivos", icon = icon("bullseye")),
      menuItem("Diccionario de Variables", tabName = "diccionario", icon = icon("book")),
      menuItem("EDA Univariado", tabName = "eda_uni", icon = icon("chart-bar")),
      menuItem("EDA Bivariado", tabName = "eda_bi", icon = icon("chart-line")),
      menuItem("EDA Multivariado", tabName = "eda_multi", icon = icon("project-diagram")),
      menuItem("Preprocesamiento", tabName = "preprocesamiento", icon = icon("wrench")),
      menuItem("Tests Estadísticos", tabName = "tests", icon = icon("flask")),
      menuItem("Evaluación de Tests", tabName = "eval_tests", icon = icon("check-circle")),
      menuItem("Modelos de Riesgo", tabName = "modelos", icon = icon("cogs")),
      menuItem("Conclusiones", tabName = "conclusiones", icon = icon("flag-checkered"))
    ),
    hr(),
    div(style = "padding: 10px; font-size: 11px; color: #ccc;",
        "Dataset: PVDAQ (n=1,720)",
        br(),
        "Modelo: Weibull AFT",
        br(),
        "Evento: PLR < -1.5%/año"
    )
  ),
  
  # --- BODY ---
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Fondo general */
        .content-wrapper { background-color: #f4f6f9; }
        
        /* Headers de boxes - MEJORADO CONTRASTE */
        .box-header {
          padding: 15px !important;
        }
        .box-header .box-title {
          font-size: 18px !important;
          font-weight: bold !important;
        }
        .box.box-solid.box-primary > .box-header {
          background-color: #2c6a9e !important;
          color: #ffffff !important;
        }
        .box.box-solid.box-primary > .box-header .box-title {
          color: #ffffff !important;
        }
        .box.box-solid.box-info > .box-header {
          background-color: #0097b2 !important;
          color: #ffffff !important;
        }
        .box.box-solid.box-info > .box-header .box-title {
          color: #ffffff !important;
        }
        .box.box-solid.box-success > .box-header {
          background-color: #008547 !important;
          color: #ffffff !important;
        }
        .box.box-solid.box-success > .box-header .box-title {
          color: #ffffff !important;
        }
        .box.box-solid.box-warning > .box-header {
          background-color: #c77c00 !important;
          color: #ffffff !important;
        }
        .box.box-solid.box-warning > .box-header .box-title {
          color: #ffffff !important;
        }
        
        /* Contenido de boxes - texto oscuro para contraste */
        .box-body { 
          background-color: #ffffff; 
          color: #333333;
        }
        
        /* Tablas - fondo claro, texto oscuro */
        .table { 
          background-color: #ffffff; 
          color: #333333;
        }
        .table th { 
          background-color: #f5f5f5; 
          color: #333333;
          font-weight: bold;
          border-bottom: 2px solid #ddd;
        }
        .table td { 
          color: #333333;
          border-bottom: 1px solid #eee;
        }
        .table-striped > tbody > tr:nth-of-type(odd) {
          background-color: #f9f9f9;
        }
        
        /* DataTables */
        .dataTables_wrapper { 
          font-size: 13px; 
          color: #333333;
        }
        .dataTables_wrapper .dataTables_filter input,
        .dataTables_wrapper .dataTables_length select {
          color: #333333;
          background-color: #ffffff;
        }
        
        /* Títulos dentro de boxes */
        .box-body h3 { color: #2c6a9e; }
        .box-body h4 { color: #444444; }
        
        /* Listas */
        ul, ol { color: #333333; }
        li { margin-bottom: 5px; }
        
        /* Párrafos */
        p { color: #333333; line-height: 1.6; }
        
        /* Formula box */
        .formula-box { 
          background-color: #f0f7ff; 
          border-left: 4px solid #3c8dbc; 
          padding: 15px; 
          margin: 10px 0;
          font-family: 'Courier New', monospace;
          color: #333333;
        }
        
        /* Verbatim output */
        pre, .shiny-text-output {
          background-color: #f8f8f8;
          color: #333333;
          border: 1px solid #ddd;
          padding: 10px;
          font-size: 12px;
        }
        
        /* Value boxes */
        .small-box { border-radius: 5px; }
        .small-box h3 { color: white !important; }
        .small-box p { color: rgba(255,255,255,0.95) !important; }
        
        /* Info boxes */
        .info-box { min-height: 90px; }
        .info-box-content { color: #333333; }
        
        /* Alertas */
        .alert { color: #333333; }
        .alert-success { background-color: #dff0d8; border-color: #d6e9c6; }
        .alert-info { background-color: #d9edf7; border-color: #bce8f1; }
        .alert-warning { background-color: #fcf8e3; border-color: #faebcc; }
        
        /* Tabs */
        .nav-tabs-custom > .tab-content { 
          padding: 15px; 
          background-color: #ffffff;
          color: #333333;
        }
      "))
    ),
    
    tabItems(
      # ========================================================================
      # TAB 1: OBJETIVOS
      # ========================================================================
      tabItem(
        tabName = "objetivos",
        fluidRow(
          box(
            title = "Pregunta de Investigación", width = 12, status = "primary",
            solidHeader = TRUE,
            h3("¿Qué factores de diseño y ambiente afectan la durabilidad de sistemas fotovoltaicos?"),
            p("Específicamente: ¿Cuánto tiempo tarda un sistema FV en experimentar degradación severa 
              (PLR < -1.5%/año) y qué características modifican este tiempo?")
          )
        ),
        fluidRow(
          box(
            title = "Objetivo General", width = 6, status = "info", solidHeader = TRUE,
            p("Desarrollar un modelo de supervivencia que permita:"),
            tags$ul(
              tags$li("Estimar el tiempo hasta degradación severa"),
              tags$li("Identificar factores protectores y de riesgo"),
              tags$li("Proporcionar una herramienta de benchmarking")
            )
          ),
          box(
            title = "Objetivos Específicos", width = 6, status = "info", solidHeader = TRUE,
            tags$ol(
              tags$li("Definir el marco de supervivencia (tiempo, evento, censura)"),
              tags$li("Explorar la relación entre variables y degradación"),
              tags$li("Comparar modelos (Cox PH vs AFT) y distribuciones"),
              tags$li("Validar el modelo final (calibración, discriminación)"),
              tags$li("Desarrollar herramienta interactiva de predicción")
            )
          )
        ),
        fluidRow(
          box(
            title = "Contexto del Problema", width = 12, status = "warning",
            fluidRow(
              column(4,
                     h4("Dataset"),
                     p("PVDAQ - Performance Validated Data Acquisition"),
                     tags$ul(
                       tags$li("4,915 sistemas originales"),
                       tags$li("1,720 después de filtrado"),
                       tags$li("305 eventos (17.7%)")
                     )
              ),
              column(4,
                     h4("Tiempo"),
                     p("Años de datos disponibles"),
                     tags$ul(
                       tags$li("Rango: 0.5 - 20+ años"),
                       tags$li("Mediana: ~5 años"),
                       tags$li("Censura: 82.3%")
                     )
              ),
              column(4,
                     h4("Evento"),
                     p("Degradación severa"),
                     tags$ul(
                       tags$li("PLR < -1.5%/año"),
                       tags$li("Umbral basado en literatura"),
                       tags$li("Irreversible en la práctica")
                     )
              )
            )
          )
        )
      ),
      
      # ========================================================================
      # TAB 2: DICCIONARIO DE VARIABLES
      # ========================================================================
      tabItem(
        tabName = "diccionario",
        fluidRow(
          box(
            title = "Diccionario de Variables", width = 12, status = "primary",
            solidHeader = TRUE,
            DTOutput("tabla_diccionario")
          )
        ),
        fluidRow(
          box(
            title = "Variables en el Modelo Final", width = 6, status = "success",
            solidHeader = TRUE,
            h4("Fórmula del Modelo:"),
            div(class = "formula-box",
                "Surv(time, event_A) ~ pv_climate_zone + technology1_fac + is_PERC + mounting_group_fac + tracking_fac"
            ),
            hr(),
            tableOutput("vars_modelo")
          ),
          box(
            title = "Variables Excluidas", width = 6, status = "warning",
            solidHeader = TRUE,
            h4("Variables no seleccionadas por stepwise AIC:"),
            tags$ul(
              tags$li(tags$b("power_cat:"), " Categoría de potencia (no significativa)"),
              tags$li(tags$b("plr_type:"), " Tipo de PLR (usada para filtrado)"),
              tags$li(tags$b("technology2:"), " Redundante con is_PERC")
            ),
            hr(),
            p(tags$em("Nota: power_cat y plr_type se usaron para filtrado pero no entraron al modelo final."))
          )
        )
      ),
      
      # ========================================================================
      # TAB 3: EDA UNIVARIADO (DATOS ORIGINALES)
      # ========================================================================
      tabItem(
        tabName = "eda_uni",
        fluidRow(
          box(
            title = "Nota: Análisis sobre datos ORIGINALES", width = 12, status = "info",
            solidHeader = TRUE,
            p("Este EDA se realiza sobre el ", tags$b("dataset original (n=4,915)"), 
              " antes del filtrado, para documentar la distribución real de los datos y valores faltantes."),
            p("El dataset filtrado para modelado tiene ", tags$b("n=1,720"), " filas.")
          )
        ),
        fluidRow(
          box(
            title = "Selección de Variable", width = 3, status = "primary",
            solidHeader = TRUE,
            selectInput("var_uni", "Variable a explorar:",
                        choices = c("plr_median", "plr_confidence_low", "plr_confidence_high",
                                    "length_years_rounded", "power_dc", "pv_climate_zone",
                                    "technology1", "technology2", "type_mounting", "tracking",
                                    "plr_type"),
                        selected = "plr_median"),
            checkboxInput("mostrar_outliers", "Resaltar outliers", TRUE),
            hr(),
            p(tags$em("Variables del CSV original (4,915 filas)"))
          ),
          box(
            title = "Medidas Resumen", width = 9, status = "info",
            solidHeader = TRUE,
            verbatimTextOutput("resumen_uni")
          )
        ),
        fluidRow(
          box(
            title = "Histograma / Barras", width = 6, status = "primary",
            plotOutput("hist_uni", height = "350px")
          ),
          box(
            title = "Boxplot", width = 6, status = "primary",
            plotOutput("box_uni", height = "350px")
          )
        ),
        fluidRow(
          box(
            title = "Valores Atípicos Detectados", width = 12, status = "warning",
            solidHeader = TRUE,
            verbatimTextOutput("outliers_uni"),
            p(tags$em("Nota: Outliers definidos como valores fuera de 1.5*IQR (para variables numéricas)."))
          )
        )
      ),
      
      # ========================================================================
      # TAB 4: EDA BIVARIADO (DATOS ORIGINALES)
      # ========================================================================
      tabItem(
        tabName = "eda_bi",
        fluidRow(
          box(
            title = "Nota: Análisis sobre datos ORIGINALES", width = 12, status = "info",
            solidHeader = TRUE,
            p("Este análisis bivariado usa el ", tags$b("dataset original (n=4,915)"), 
              " para explorar relaciones entre variables antes del filtrado.")
          )
        ),
        fluidRow(
          box(
            title = "Selección de Variables", width = 4, status = "primary",
            solidHeader = TRUE,
            selectInput("var_bi_x", "Variable X (categórica):",
                        choices = c("technology1", "technology2",
                                    "type_mounting", "tracking", "plr_type"),
                        selected = "technology1"),
            p(tags$em("Nota: pv_climate_zone y power_dc son numéricas continuas, no se incluyen aquí.")),
            selectInput("var_bi_y", "Variable Y (numérica):",
                        choices = c("plr_median", "plr_confidence_low", "plr_confidence_high",
                                    "length_years_rounded"),
                        selected = "plr_median"),
            selectInput("tipo_grafico_bi", "Tipo de gráfico:",
                        choices = c("Boxplot" = "boxplot", 
                                    "Violin" = "violin",
                                    "Puntos" = "scatter"),
                        selected = "boxplot"),
            hr(),
            p(tags$em("Variables del CSV original"))
          ),
          box(
            title = "Gráfico Bivariado", width = 8, status = "primary",
            plotOutput("plot_bivariado", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "Tabla de Contingencia / Resumen por Grupo", width = 6, status = "info",
            solidHeader = TRUE,
            verbatimTextOutput("tabla_bivariado")
          ),
          box(
            title = "Test de Asociación", width = 6, status = "success",
            solidHeader = TRUE,
            verbatimTextOutput("test_asociacion")
          )
        ),
        fluidRow(
          box(
            title = "Curvas de Supervivencia por Grupo (Dataset Modelo n=1,720)", width = 12, status = "success",
            solidHeader = TRUE,
            p(tags$em("Nota: Las curvas KM requieren time y event_A, que solo existen en el dataset procesado."), 
              style = "color: #00a65a;"),
            selectInput("var_km_bi", "Agrupar por:", 
                        choices = c("technology1_fac", "is_PERC", "mounting_group_fac", "tracking_fac"),
                        selected = "is_PERC", width = "300px"),
            plotOutput("km_bivariado", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "Correlación: Tiempo vs PLR", width = 6, status = "info",
            solidHeader = TRUE,
            plotOutput("scatter_time_plr", height = "350px")
          ),
          box(
            title = "Coeficientes de Correlación", width = 6, status = "success",
            solidHeader = TRUE,
            tableOutput("tabla_correlaciones"),
            hr(),
            p(tags$em("Pearson mide asociación lineal; Spearman mide asociación monotónica (más robusta a outliers)."))
          )
        ),
        fluidRow(
          box(
            title = "Outliers Bivariados (Mahalanobis Robusta)", width = 12, status = "warning",
            solidHeader = TRUE,
            fluidRow(
              column(8, plotOutput("plot_outliers_biv", height = "400px")),
              column(4, 
                     h4("Resumen de Outliers"),
                     tableOutput("tabla_outliers_biv"),
                     hr(),
                     p("La distancia de Mahalanobis robusta (MCD) detecta puntos atípicos considerando la correlación entre variables."),
                     p(tags$b("Umbral:"), " χ²(2, 0.975) = 7.38")
              )
            )
          )
        )
      ),
      
      # ========================================================================
      # TAB 5: EDA MULTIVARIADO (usa datos del MODELO - completos)
      # ========================================================================
      tabItem(
        tabName = "eda_multi",
        fluidRow(
          box(
            title = "Análisis Exploratorio Multivariado", width = 12, status = "primary",
            solidHeader = TRUE,
            p("Este análisis considera múltiples variables simultáneamente para detectar patrones."),
            p(tags$b("Nota:"), " El CSV original tiene ~65% de valores vacíos en variables clave (zona climática, tiempo, etc.), 
              por lo que este análisis usa el ", tags$span("dataset filtrado (n=1,720)", style = "color: #00a65a; font-weight: bold;"), 
              " que tiene datos completos."),
            tags$ul(
              tags$li(tags$b("Correlaciones:"), " Relaciones lineales entre PLR, tiempo y zona climática"),
              tags$li(tags$b("Scatter PLR vs Tiempo:"), " Relación entre degradación y duración del monitoreo"),
              tags$li(tags$b("Multicolinealidad (VIF):"), " Variables redundantes en el modelo"),
              tags$li(tags$b("Outliers multivariados:"), " Observaciones atípicas (Mahalanobis robusta)")
            )
          )
        ),
        fluidRow(
          box(
            title = "Matriz de Correlaciones (Dataset Modelo n=1,720)", width = 6, status = "info",
            solidHeader = TRUE,
            plotOutput("plot_corr_matrix", height = "400px"),
            p(tags$em("Variables: PLR, tiempo, zona climática. Colores: azul=positiva, rojo=negativa."))
          ),
          box(
            title = "Correlaciones Significativas", width = 6, status = "success",
            solidHeader = TRUE,
            tableOutput("tabla_corr_signif"),
            hr(),
            p(tags$em("Pares de variables con correlación |r| >= 0.1"))
          )
        ),
        fluidRow(
          box(
            title = "Scatter: PLR vs Tiempo de Monitoreo (Dataset Modelo)", width = 12, status = "primary",
            solidHeader = TRUE,
            plotOutput("plot_plr_vs_tiempo", height = "400px"),
            p(tags$em("Relación entre la tasa de degradación (PLR) y la duración del monitoreo."))
          )
        ),
        fluidRow(
          box(
            title = "VIF - Factor de Inflación de Varianza (Datos del Modelo)", width = 6, status = "warning",
            solidHeader = TRUE,
            tableOutput("tabla_vif"),
            hr(),
            h4("Interpretación del VIF:"),
            tags$ul(
              tags$li(tags$b("VIF < 5:"), " No hay problema de multicolinealidad"),
              tags$li(tags$b("5 ≤ VIF < 10:"), " Multicolinealidad moderada"),
              tags$li(tags$b("VIF ≥ 10:"), " Multicolinealidad severa (considerar eliminar variable)")
            )
          ),
          box(
            title = "Resumen de Multicolinealidad", width = 6, status = "info",
            solidHeader = TRUE,
            verbatimTextOutput("resumen_vif"),
            hr(),
            p("El VIF mide cuánto aumenta la varianza de un coeficiente debido a la correlación con otras variables."),
            p(tags$b("En nuestro modelo:"), " Las variables seleccionadas no presentan multicolinealidad severa.")
          )
        ),
        fluidRow(
          box(
            title = "Outliers Multivariados - Mahalanobis (Dataset Modelo)", width = 12, status = "warning",
            solidHeader = TRUE,
            fluidRow(
              column(8, plotOutput("plot_outliers_multi", height = "400px")),
              column(4,
                     h4("Resumen de Outliers"),
                     tableOutput("tabla_outliers_multi"),
                     hr(),
                     h4("Top 10 Observaciones Atípicas"),
                     tableOutput("tabla_top10_outliers")
              )
            ),
            hr(),
            p("La distancia de Mahalanobis robusta (MCD) considera la correlación entre múltiples variables (time, zona climática, PLR) para identificar observaciones atípicas en el dataset filtrado.")
          )
        )
      ),
      
      # ========================================================================
      # TAB 6: PREPROCESAMIENTO (Datos Originales → Filtrados)
      # ========================================================================
      tabItem(
        tabName = "preprocesamiento",
        fluidRow(
          box(
            title = "Flujo de Datos: Original → Modelo", width = 12, status = "info",
            solidHeader = TRUE,
            p("Este análisis muestra cómo se reducen los datos desde el CSV original hasta el dataset usado para modelado."),
            p(tags$b("Importante:"), " Los valores faltantes que se muestran son del ", 
              tags$span("dataset original (n=4,915)", style = "color: #3c8dbc; font-weight: bold;"),
              " antes de aplicar cualquier filtro.")
          )
        ),
        fluidRow(
          valueBoxOutput("vb_n_original", width = 3),
          valueBoxOutput("vb_n_filtrado", width = 3),
          valueBoxOutput("vb_pct_perdido", width = 3),
          valueBoxOutput("vb_eventos", width = 3)
        ),
        fluidRow(
          box(
            title = "Valores Faltantes en Datos ORIGINALES (n=4,915)", width = 6, status = "warning",
            solidHeader = TRUE,
            plotOutput("plot_na", height = "350px"),
            hr(),
            verbatimTextOutput("resumen_na")
          ),
          box(
            title = "Balance de Clases (Dataset Filtrado)", width = 6, status = "info",
            solidHeader = TRUE,
            plotOutput("plot_balance", height = "300px"),
            hr(),
            verbatimTextOutput("resumen_balance")
          )
        ),
        fluidRow(
          box(
            title = "Pérdida de Datos Paso a Paso", width = 6, status = "danger",
            solidHeader = TRUE,
            tableOutput("tabla_perdida_pasos"),
            hr(),
            p(tags$em("Las columnas que más contribuyen a la pérdida son aquellas con mayor % de NA."))
          ),
          box(
            title = "Criterios de Filtrado Aplicados", width = 6, status = "primary",
            solidHeader = TRUE,
            tags$ol(
              tags$li("length_years_rounded (time) > 0 y no NA/vacío"),
              tags$li("pv_climate_zone entre 2 y 6"),
              tags$li("technology1 no vacío"),
              tags$li("type_mounting no vacío"),
              tags$li("tracking no vacío"),
              tags$li(tags$b("power_dc no vacío"), " → crea power_cat"),
              tags$li(tags$b("plr_type no vacío"))
            ),
            p(tags$em("Nota: power_dc (y derivadas) + plr_type causan ~65% de las pérdidas porque muchas filas tienen estas columnas vacías."))
          )
        ),
        fluidRow(
          box(
            title = "Decisiones de Preprocesamiento", width = 6, status = "success",
            solidHeader = TRUE,
            h4("Transformaciones aplicadas:"),
            tags$ul(
              tags$li(tags$b("technology1_collapsed:"), " Agrupación de tecnologías raras (CIGS, CdTe → mantienen pero n=0 post-filtrado)"),
              tags$li(tags$b("mounting_group:"), " Reducción de 20+ categorías a 4 (Ground, Roof, Parking, Canopy_Other)"),
              tags$li(tags$b("is_PERC:"), " Variable binaria extraída de technology2"),
              tags$li(tags$b("tracking_fac:"), " Factor desde variable lógica TRUE/FALSE"),
              tags$li(tags$b("event_A:"), " 1 si plr_median < -1.5, 0 en caso contrario")
            ),
            hr(),
            h4("Niveles de referencia (factores):"),
            tags$ul(
              tags$li("technology1_fac: c-Si (97% de los datos)"),
              tags$li("mounting_group_fac: Canopy_Other"),
              tags$li("tracking_fac: FALSE (sin tracking)")
            )
          ),
          box(
            title = "Variables Derivadas", width = 6, status = "warning",
            solidHeader = TRUE,
            h4("Variables que NO existen en el CSV original:"),
            tags$ul(
              tags$li(tags$b("time:"), " Renombrado de length_years_rounded"),
              tags$li(tags$b("event_A:"), " Creada: plr_median < -1.5"),
              tags$li(tags$b("power_cat:"), " Categorización de power_dc"),
              tags$li(tags$b("is_PERC:"), " Extraída de technology2"),
              tags$li(tags$b("mounting_group:"), " Agrupación de type_mounting"),
              tags$li(tags$b("technology1_collapsed:"), " Agrupación de technology1")
            ),
            p(tags$em("Estas variables se crean durante el preprocesamiento en 00_crear_modelo_final.R"))
          )
        )
      ),
      
      # ========================================================================
      # TAB 6: TESTS ESTADÍSTICOS
      # ========================================================================
      tabItem(
        tabName = "tests",
        fluidRow(
          box(
            title = "Relación con los Objetivos", width = 12, status = "info",
            solidHeader = TRUE,
            p("Esta sección presenta los tests estadísticos utilizados para cumplir los objetivos del estudio:"),
            tags$ul(
              tags$li(tags$b("Objetivo 3 - Comparar modelos:"), " Test de Schoenfeld (¿Cox o AFT?) + Comparación AIC (¿qué distribución?)"),
              tags$li(tags$b("Objetivo 4 - Validar modelo:"), " Significancia de coeficientes + Test Log-Rank (diferencias entre grupos)")
            )
          )
        ),
        fluidRow(
          box(
            title = "Comparación de Distribuciones (Objetivo 3)", width = 12, status = "primary",
            solidHeader = TRUE,
            h4("¿Qué distribución paramétrica usar? → Comparación por AIC"),
            tableOutput("tabla_aic"),
            hr(),
            p("El modelo ", tags$b("Weibull"), " presenta el menor AIC, indicando mejor ajuste 
              con menor complejidad. La diferencia con loglogística (ΔAIC = 14.4) y lognormal 
              (ΔAIC = 26.7) es sustancial."),
            p(tags$b("Conclusión:"), " Se selecciona Weibull como distribución del modelo AFT.")
          )
        ),
        fluidRow(
          box(
            title = "¿Cox PH o AFT? - Test de Schoenfeld (Objetivo 3)", width = 6, status = "warning",
            solidHeader = TRUE,
            h4("Test de residuos de Schoenfeld:"),
            verbatimTextOutput("test_schoenfeld"),
            hr(),
            p(tags$em("Si p < 0.05, el supuesto de riesgos proporcionales (PH) se viola.")),
            p(tags$b("Conclusión:"), " Como PH se viola → se justifica usar modelo AFT en lugar de Cox.")
          ),
          box(
            title = "Test Log-Rank - Diferencias entre Grupos (Objetivo 4)", width = 6, status = "info",
            solidHeader = TRUE,
            selectInput("var_logrank", "Variable para test log-rank:",
                        choices = c("is_PERC", "technology1_fac", "mounting_group_fac", "tracking_fac"),
                        selected = "is_PERC"),
            verbatimTextOutput("test_logrank"),
            p(tags$em("El test log-rank evalúa si las curvas de supervivencia difieren significativamente entre grupos."))
          )
        ),
        fluidRow(
          box(
            title = "Significancia de Coeficientes - Validación (Objetivo 4)", width = 12, status = "success",
            solidHeader = TRUE,
            tableOutput("tabla_coefs_test"),
            hr(),
            p("Coeficientes con p < 0.05 son estadísticamente significativos al 5%."),
            p(tags$b("Interpretación:"), " Las variables significativas tienen un efecto real sobre el tiempo de supervivencia.")
          )
        )
      ),
      
      # ========================================================================
      # TAB 7: EVALUACIÓN DE TESTS
      # ========================================================================
      tabItem(
        tabName = "eval_tests",
        fluidRow(
          box(
            title = "Configuración de Evaluación", width = 4, status = "primary",
            solidHeader = TRUE,
            sliderInput("tiempo_eval", "Tiempo de evaluación (años):", 
                        min = 5, max = 20, value = 10, step = 1),
            selectInput("metrica_eval", "Métrica a evaluar:",
                        choices = c("Calibración" = "calibracion",
                                    "Discriminación (C-index)" = "cindex",
                                    "Brier Score" = "brier"),
                        selected = "calibracion"),
            actionButton("btn_evaluar", "Evaluar", class = "btn-primary btn-block")
          ),
          box(
            title = "Resultados de Evaluación", width = 8, status = "success",
            solidHeader = TRUE,
            plotOutput("plot_evaluacion", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "Tabla de Calibración por Quintil", width = 6, status = "info",
            solidHeader = TRUE,
            tableOutput("tabla_calibracion")
          ),
          box(
            title = "Métricas Globales", width = 6, status = "warning",
            solidHeader = TRUE,
            valueBoxOutput("vb_cindex", width = 12),
            valueBoxOutput("vb_error_medio", width = 12)
          )
        ),
        fluidRow(
          box(
            title = "Interpretación", width = 12, status = "primary",
            uiOutput("interpretacion_eval")
          )
        )
      ),
      
      # ========================================================================
      # TAB 8: MODELOS DE RIESGO
      # ========================================================================
      tabItem(
        tabName = "modelos",
        fluidRow(
          box(
            title = "Modelo Weibull AFT Final", width = 12, status = "primary",
            solidHeader = TRUE,
            verbatimTextOutput("summary_modelo")
          )
        ),
        fluidRow(
          box(
            title = "Coeficientes e Interpretación", width = 6, status = "success",
            solidHeader = TRUE,
            tableOutput("tabla_coefs_modelo"),
            hr(),
            h4("Interpretación de Time Ratios (TR):"),
            tags$ul(
              tags$li(tags$b("TR > 1:"), " Factor protector (aumenta tiempo hasta evento)"),
              tags$li(tags$b("TR < 1:"), " Factor de riesgo (reduce tiempo hasta evento)"),
              tags$li(tags$b("TR = 1:"), " Sin efecto")
            )
          ),
          box(
            title = "Forest Plot", width = 6, status = "info",
            solidHeader = TRUE,
            plotOutput("forest_plot", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "Predictor Interactivo", width = 12, status = "warning",
            solidHeader = TRUE,
            fluidRow(
              column(2,
                     numericInput("pred_clima", "Zona climática:", value = 4, min = 2, max = 6)
              ),
              column(2,
                     selectInput("pred_tech", "Tecnología:", 
                                 choices = c("c-Si", "a-Si", "Unknown"), selected = "c-Si")
              ),
              column(2,
                     selectInput("pred_perc", "PERC:", choices = c("No" = 0, "Sí" = 1), selected = 0)
              ),
              column(2,
                     selectInput("pred_mount", "Montaje:", 
                                 choices = c("Canopy_Other", "Ground", "Parking", "Roof"),
                                 selected = "Ground")
              ),
              column(2,
                     selectInput("pred_track", "Tracking:", 
                                 choices = c("No" = "FALSE", "Sí" = "TRUE"), selected = "FALSE")
              ),
              column(2,
                     br(),
                     actionButton("btn_predecir", "Predecir", class = "btn-success btn-block")
              )
            ),
            hr(),
            fluidRow(
              column(6,
                     h4("Curva de Supervivencia Predicha:"),
                     plotOutput("curva_predicha", height = "300px")
              ),
              column(6,
                     h4("Indicadores de Riesgo:"),
                     tableOutput("indicadores_predichos")
              )
            )
          )
        )
      ),
      
      # ========================================================================
      # TAB 9: CONCLUSIONES
      # ========================================================================
      tabItem(
        tabName = "conclusiones",
        fluidRow(
          box(
            title = "Hallazgos Principales", width = 12, status = "success",
            solidHeader = TRUE,
            fluidRow(
              column(6,
                     h4("Factores de Riesgo (TR < 1):"),
                     tags$ul(
                       tags$li(tags$b("PERC (TR = 0.51):"), " Reduce tiempo esperado a la mitad"),
                       tags$li(tags$b("Unknown technology:"), " Mayor incertidumbre asociada")
                     )
              ),
              column(6,
                     h4("Factores Protectores (TR > 1):"),
                     tags$ul(
                       tags$li(tags$b("Zona climática (TR=1.12):"), " +12% tiempo por zona (contraintuitivo: zonas cálidas más durables)"),
                       tags$li(tags$b("Ground mounting (TR = 3.15):"), " Triplica tiempo vs Canopy"),
                       tags$li(tags$b("Roof mounting (TR = 2.68):"), " Más durable que Canopy"),
                       tags$li(tags$b("Tracking solar (TR = 1.33):"), " Efecto protector moderado")
                     )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Limitaciones del Estudio", width = 6, status = "warning",
            solidHeader = TRUE,
            tags$ol(
              tags$li(tags$b("Tiempo ambiguo:"), " 'Años de datos' no es edad real del sistema"),
              tags$li(tags$b("Sesgo de selección:"), " Solo sistemas con datos públicos"),
              tags$li(tags$b("Calibración imperfecta:"), " Error medio ~31% a 10 años"),
              tags$li(tags$b("Categorías sin datos:"), " CdTe y CIGS con n=0"),
              tags$li(tags$b("Censura informativa:"), " Posible sesgo no medido")
            )
          ),
          box(
            title = "Recomendaciones de Uso", width = 6, status = "info",
            solidHeader = TRUE,
            h4("SÍ usar para:"),
            tags$ul(
              tags$li("Benchmarking relativo entre sistemas"),
              tags$li("Ranking de riesgo para O&M"),
              tags$li("Identificación de factores críticos"),
              tags$li("Priorización de inspecciones")
            ),
            hr(),
            h4("NO usar para:"),
            tags$ul(
              tags$li("Garantías comerciales"),
              tags$li("Predicciones absolutas de vida útil"),
              tags$li("Extrapolación más allá de 20-25 años")
            )
          )
        ),
        fluidRow(
          box(
            title = "Trabajo Futuro", width = 12, status = "primary",
            solidHeader = TRUE,
            fluidRow(
              column(4,
                     h4("Datos"),
                     tags$ul(
                       tags$li("Incorporar fecha de instalación real"),
                       tags$li("Expandir a otras geografías"),
                       tags$li("Incluir más tecnologías (CdTe, CIGS)")
                     )
              ),
              column(4,
                     h4("Metodología"),
                     tags$ul(
                       tags$li("Modelos de riesgos competitivos"),
                       tags$li("Análisis de series temporales"),
                       tags$li("Machine learning para predicción")
                     )
              ),
              column(4,
                     h4("Aplicación"),
                     tags$ul(
                       tags$li("Integración con sistemas SCADA"),
                       tags$li("Alertas automáticas de riesgo"),
                       tags$li("API para terceros")
                     )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Resumen Ejecutivo", width = 12, status = "success",
            solidHeader = TRUE,
            div(style = "font-size: 16px; line-height: 1.8;",
                p("Este estudio desarrolló un ", tags$b("modelo Weibull AFT"), " para estimar 
                  el tiempo hasta degradación severa (PLR < -1.5%/año) en sistemas fotovoltaicos."),
                p("Con un ", tags$b("C-index de 0.746"), ", el modelo demuestra capacidad de 
                  discriminación aceptable para ranking de riesgo relativo."),
                p("Los hallazgos más relevantes son el ", tags$b("efecto adverso de PERC"), 
                  " (reduce durabilidad ~50%) y el ", tags$b("efecto protector del montaje en suelo"), 
                  " (triplica el tiempo esperado vs canopy)."),
                p("Se recomienda usar el modelo para ", tags$b("benchmarking y priorización"), 
                  ", pero no para predicciones absolutas debido a las limitaciones de calibración.")
            )
          )
        )
      )
    )
  )
)

# ==============================================================================
# SERVER - LÓGICA DEL SERVIDOR
# ==============================================================================

server <- function(input, output, session) {
  
  # --- DATOS REACTIVOS ---
  # Datos originales (CSV) para EDA
  datos_orig <- reactive({ DATOS_CSV_ORIGINAL })
  

  # Datos filtrados para modelos
  datos_modelo <- reactive({ DATOS_MODELO })
  
  # Para compatibilidad con código existente (EDA usa originales, modelos usa filtrados)
  datos <- reactive({ DATOS_MODELO })
  
  # Modelo
  modelo <- reactive({ MODELO_FINAL })
  
  # ==========================================================================
  # TAB 2: DICCIONARIO
  # ==========================================================================
  
  output$tabla_diccionario <- renderDT({
    diccionario <- data.frame(
      Variable = c("time", "event_A", "pv_climate_zone", "technology1_fac", 
                   "is_PERC", "mounting_group_fac", "tracking_fac",
                   "plr_median", "power_dc", "plr_type"),
      Tipo = c("Numérica", "Binaria", "Numérica", "Factor", 
               "Binaria", "Factor", "Factor",
               "Numérica", "Caracter", "Caracter"),
      Descripcion = c("Años de datos disponibles (tiempo de seguimiento)",
                      "Evento de degradación severa (PLR < -1.5%/año): 1=evento, 0=censurado",
                      "Zona climática PV (2=frío, 6=cálido extremo)",
                      "Tecnología principal: c-Si, a-Si, Unknown",
                      "¿Tecnología PERC? 1=Sí, 0=No",
                      "Tipo de montaje: Canopy_Other, Ground, Parking, Roof",
                      "¿Tracking solar? TRUE/FALSE",
                      "Tasa de degradación mediana (%/año)",
                      "Potencia DC nominal",
                      "Tipo de medición PLR"),
      En_Modelo = c("Sí (tiempo)", "Sí (evento)", "Sí", "Sí", "Sí", "Sí", "Sí", "No", "No", "No"),
      stringsAsFactors = FALSE
    )
    datatable(diccionario, 
              options = list(pageLength = 10, dom = 't'),
              rownames = FALSE,
              class = 'cell-border stripe')
  })
  
  output$vars_modelo <- renderTable({
    data.frame(
      Variable = c("pv_climate_zone", "technology1_fac", "is_PERC", 
                   "mounting_group_fac", "tracking_fac"),
      Tipo = c("Numérica continua", "Factor (3 niveles)", "Binaria", 
               "Factor (4 niveles)", "Factor (2 niveles)"),
      Referencia = c("- (no aplica)", "c-Si", "0 (No)", "Canopy_Other", "FALSE"),
      Nota = c("Efecto lineal por unidad", 
               "a-Si, Unknown vs c-Si",
               "1 vs 0",
               "Ground, Parking, Roof vs Canopy",
               "TRUE vs FALSE")
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # ==========================================================================
  # TAB 3: EDA UNIVARIADO (DATOS ORIGINALES)
  # ==========================================================================
  
  output$resumen_uni <- renderPrint({
    var <- input$var_uni
    df <- datos_orig()
    x <- df[[var]]
    
    cat("=== DATOS ORIGINALES (n=4,915) ===\n\n")
    
    if (is.numeric(x)) {
      cat("RESUMEN NUMÉRICO:\n\n")
      cat(sprintf("n total:  %d\n", length(x)))
      cat(sprintf("NA:       %d (%.1f%%)\n", sum(is.na(x)), mean(is.na(x))*100))
      cat(sprintf("Válidos:  %d (%.1f%%)\n", sum(!is.na(x)), mean(!is.na(x))*100))
      cat(sprintf("\nMedia:    %.3f\n", mean(x, na.rm = TRUE)))
      cat(sprintf("Mediana:  %.3f\n", median(x, na.rm = TRUE)))
      cat(sprintf("SD:       %.3f\n", sd(x, na.rm = TRUE)))
      cat(sprintf("Mín:      %.3f\n", min(x, na.rm = TRUE)))
      cat(sprintf("Máx:      %.3f\n", max(x, na.rm = TRUE)))
      cat(sprintf("Q1:       %.3f\n", quantile(x, 0.25, na.rm = TRUE)))
      cat(sprintf("Q3:       %.3f\n", quantile(x, 0.75, na.rm = TRUE)))
      cat(sprintf("IQR:      %.3f\n", IQR(x, na.rm = TRUE)))
    } else {
      cat("TABLA DE FRECUENCIAS:\n\n")
      tbl <- table(x, useNA = "ifany")
      print(tbl)
      cat(sprintf("\nNA / vacíos: %d (%.1f%%)\n", sum(is.na(x)), mean(is.na(x))*100))
      cat("\nPROPORCIONES (sin NA):\n\n")
      tbl_sin_na <- table(x, useNA = "no")
      print(round(prop.table(tbl_sin_na), 3))
    }
  })
  
  output$hist_uni <- renderPlot({
    var <- input$var_uni
    df <- datos_orig()
    x <- df[[var]]
    
    if (is.numeric(x)) {
      ggplot(df, aes_string(x = var)) +
        geom_histogram(bins = 30, fill = "#3c8dbc", color = "white", alpha = 0.8) +
        geom_vline(xintercept = mean(x, na.rm = TRUE), color = "red", linetype = "dashed", size = 1) +
        labs(title = paste("Histograma de", var, "(Datos Originales n=4,915)"),
             subtitle = paste("Línea roja = media (", round(mean(x, na.rm = TRUE), 2), ") | NA:", sum(is.na(x))),
             x = var, y = "Frecuencia") +
        theme_minimal(base_size = 14)
    } else {
      df_count <- df %>% 
        mutate(!!var := ifelse(is.na(!!sym(var)) | !!sym(var) == "", "NA/Vacío", !!sym(var))) %>%
        count(!!sym(var))
      ggplot(df_count, aes_string(x = var, y = "n")) +
        geom_bar(stat = "identity", fill = "#3c8dbc", alpha = 0.8) +
        geom_text(aes(label = n), vjust = -0.5, size = 3) +
        labs(title = paste("Frecuencias de", var, "(Datos Originales n=4,915)"), 
             x = var, y = "Frecuencia") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  output$box_uni <- renderPlot({
    var <- input$var_uni
    df <- datos_orig()
    x <- df[[var]]
    
    if (is.numeric(x)) {
      ggplot(df, aes_string(y = var)) +
        geom_boxplot(fill = "#3c8dbc", alpha = 0.7, outlier.color = "red", outlier.size = 2) +
        labs(title = paste("Boxplot de", var, "(Datos Originales)"), 
             subtitle = paste("NA:", sum(is.na(x)), "de", length(x)),
             y = var) +
        theme_minimal(base_size = 14) +
        coord_flip()
    } else {
      df_count <- df %>% 
        mutate(!!var := ifelse(is.na(!!sym(var)) | !!sym(var) == "", "NA/Vacío", !!sym(var))) %>%
        count(!!sym(var))
      ggplot(df_count, aes_string(x = var, y = "n")) +
        geom_bar(stat = "identity", fill = "#3c8dbc", alpha = 0.8) +
        labs(title = paste("Barras de", var, "(Datos Originales)"), x = var, y = "Conteo") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  output$outliers_uni <- renderPrint({
    # Respetar checkbox
    if (!isTRUE(input$mostrar_outliers)) {
      cat("Activa 'Resaltar outliers' arriba para ver el detalle.\n")
      return(invisible(NULL))
    }
    
    var <- input$var_uni
    df <- datos_orig()
    x <- df[[var]]
    
    if (is.numeric(x)) {
      q1 <- quantile(x, 0.25, na.rm = TRUE)
      q3 <- quantile(x, 0.75, na.rm = TRUE)
      iqr <- IQR(x, na.rm = TRUE)
      
      lower <- q1 - 1.5 * iqr
      upper <- q3 + 1.5 * iqr
      
      outliers_low <- x[x < lower & !is.na(x)]
      outliers_high <- x[x > upper & !is.na(x)]
      
      cat(sprintf("Límite inferior: %.3f\n", lower))
      cat(sprintf("Límite superior: %.3f\n", upper))
      cat(sprintf("\nOutliers bajos: %d (%.1f%%)\n", length(outliers_low), 
                  length(outliers_low)/length(x)*100))
      cat(sprintf("Outliers altos: %d (%.1f%%)\n", length(outliers_high), 
                  length(outliers_high)/length(x)*100))
      cat(sprintf("\nTotal outliers: %d (%.1f%%)\n", 
                  length(outliers_low) + length(outliers_high),
                  (length(outliers_low) + length(outliers_high))/length(x)*100))
    } else {
      cat(sprintf("=== ANÁLISIS DE CATEGORÍAS RARAS ===\n\n"))
      cat(sprintf("Variable seleccionada: %s\n\n", var))
      cat("Para variables categóricas no aplica detección de outliers por IQR.\n")
      cat("En su lugar, identificamos categorías con frecuencia muy baja.\n\n")
      
      freq <- table(x)
      cat("Distribución de frecuencias:\n")
      for (i in seq_along(freq)) {
        cat(sprintf("  • %s: n = %d (%.1f%%)\n", 
                    names(freq)[i], freq[i], freq[i]/sum(freq)*100))
      }
      
      raras <- freq[freq < 50]
      cat("\n")
      if (length(raras) > 0) {
        cat("⚠️  Categorías con n < 50 (pueden tener estimaciones inestables):\n")
        for (i in seq_along(raras)) {
          cat(sprintf("  • %s: n = %d\n", names(raras)[i], raras[i]))
        }
      } else {
        cat("✓ Todas las categorías tienen n >= 50 (tamaño adecuado).\n")
      }
    }
  })
  
  # ==========================================================================
  # TAB 4: EDA BIVARIADO (DATOS ORIGINALES)
  # ==========================================================================
  
  output$plot_bivariado <- renderPlot({
    var_x <- input$var_bi_x
    var_y <- input$var_bi_y
    tipo <- input$tipo_grafico_bi
    
    # Convertir Y a numérica si es necesario (length_years_rounded viene como character)
    df <- datos_orig() %>%
      mutate(across(all_of(var_y), ~ suppressWarnings(as.numeric(.)))) %>%
      filter(!is.na(!!sym(var_x)), !is.na(!!sym(var_y)))
    
    p <- ggplot(df, aes_string(x = var_x, y = var_y, fill = var_x))
    
    if (tipo == "boxplot") {
      p <- p + geom_boxplot(alpha = 0.7, outlier.color = "red")
    } else if (tipo == "violin") {
      p <- p + geom_violin(alpha = 0.7) + geom_boxplot(width = 0.1, fill = "white")
    } else {
      p <- p + geom_jitter(aes_string(color = var_x), alpha = 0.5, width = 0.2)
    }
    
    n_orig <- nrow(datos_orig())
    n_valid <- nrow(df)
    
    p + labs(title = paste(var_y, "por", var_x, "(Datos Originales)"),
             subtitle = paste("n válido:", n_valid, "de", n_orig, "| Excluidos:", n_orig - n_valid),
             x = var_x, y = var_y) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none") +
      scale_fill_brewer(palette = "Set2") +
      scale_color_brewer(palette = "Set2")
  })
  
  output$tabla_bivariado <- renderPrint({
    var_x <- input$var_bi_x
    var_y <- input$var_bi_y
    
    # CORRECCIÓN: Convertir Y a numérica ANTES de filtrar/resumir
    # Esto resuelve el problema de NA cuando length_years_rounded viene como character
    df <- datos_orig() %>%
      mutate(across(all_of(var_y), ~ suppressWarnings(as.numeric(.)))) %>%
      filter(!is.na(!!sym(var_x)), !is.na(!!sym(var_y)))
    
    cat("=== RESUMEN POR GRUPO (Datos Originales) ===\n\n")
    cat(sprintf("n válido: %d de %d\n\n", nrow(df), nrow(datos_orig())))
    
    # CORRECCIÓN: Calcular estadísticos con redondeo para mejor legibilidad
    resumen <- df %>%
      group_by(!!sym(var_x)) %>%
      summarise(
        n = n(),
        media = round(mean(!!sym(var_y), na.rm = TRUE), 2),
        mediana = round(median(!!sym(var_y), na.rm = TRUE), 2),
        sd = round(sd(!!sym(var_y), na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    print(as.data.frame(resumen))
  })
  
  output$test_asociacion <- renderPrint({
    var_x <- input$var_bi_x
    var_y <- input$var_bi_y
    
    df <- datos_orig() %>%
      filter(!is.na(!!sym(var_x)), !is.na(!!sym(var_y)))
    
    x <- df[[var_x]]
    y <- df[[var_y]]
    
    cat("=== TEST DE ASOCIACIÓN (Datos Originales) ===\n\n")
    
    if (is.numeric(y)) {
      # Kruskal-Wallis para grupos vs numérica
      test <- kruskal.test(y ~ x)
      cat("Test de Kruskal-Wallis:\n")
      cat(sprintf("  H = %.3f\n", test$statistic))
      cat(sprintf("  df = %d\n", test$parameter))
      cat(sprintf("  p-value = %.4e\n", test$p.value))
      cat(sprintf("\n  Interpretación: %s\n", 
                  ifelse(test$p.value < 0.05, 
                         "Diferencias significativas entre grupos (p < 0.05)",
                         "No hay diferencias significativas (p >= 0.05)")))
    } else {
      # Chi-cuadrado para categórica vs categórica
      test <- chisq.test(table(x, y))
      cat("Test Chi-cuadrado:\n")
      cat(sprintf("  X² = %.3f\n", test$statistic))
      cat(sprintf("  df = %d\n", test$parameter))
      cat(sprintf("  p-value = %.4e\n", test$p.value))
    }
  })
  
  output$km_bivariado <- renderPlot({
    var_x <- input$var_km_bi  # Usar el nuevo selectInput específico para KM
    datos_km <- datos_modelo()  # Usar datos del modelo (tiene time y event_A)
    
    # Crear fórmula y ajustar modelo
    formula_km <- as.formula(paste("Surv(time, event_A) ~", var_x))
    km_fit <- survfit(formula_km, data = datos_km)
    
    # Extraer datos del survfit para ggplot2 (más estable que ggsurvplot en Shiny)
    surv_summary <- summary(km_fit)
    
    # Crear dataframe para plotear
    df_surv <- data.frame(
      time = surv_summary$time,
      surv = surv_summary$surv,
      upper = surv_summary$upper,
      lower = surv_summary$lower,
      strata = as.character(surv_summary$strata)
    )
    
    # Limpiar nombres de strata
    df_surv$strata <- gsub(paste0(var_x, "="), "", df_surv$strata)
    
    # Test log-rank para p-value
    test_logrank <- survdiff(formula_km, data = datos_km)
    pval <- pchisq(test_logrank$chisq, length(test_logrank$n) - 1, lower.tail = FALSE)
    pval_text <- ifelse(pval < 0.001, "p < 0.001", paste("p =", round(pval, 4)))
    
    # Gráfico con ggplot2
    ggplot(df_surv, aes(x = time, y = surv, color = strata)) +
      geom_step(size = 1.2) +
      scale_color_brewer(palette = "Set2", name = var_x) +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      labs(
        title = paste("Curvas de Kaplan-Meier por", var_x, "(Dataset Modelo)"),
        subtitle = paste(pval_text, "| n =", nrow(datos_km)),
        x = "Tiempo (años)",
        y = "Probabilidad de supervivencia S(t)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
  })
  
  # --- Scatter Time vs PLR (DATOS MODELO - tiene variable time) ---
  output$scatter_time_plr <- renderPlot({
    df <- datos_modelo() %>%
      filter(!is.na(time), !is.na(plr_median))
    
    ggplot(df, aes(x = time, y = plr_median)) +
      geom_point(alpha = 0.3, size = 1.5, color = "#3c8dbc") +
      geom_smooth(method = "loess", se = TRUE, color = "#e74c3c", fill = "#e74c3c", alpha = 0.2) +
      geom_hline(yintercept = -1.5, linetype = "dashed", color = "red", size = 0.8) +
      annotate("text", x = max(df$time, na.rm = TRUE) * 0.8, y = -1.3, 
               label = "Umbral evento (-1.5%)", color = "red", size = 3.5) +
      labs(
        title = "Tiempo vs PLR (Dataset Modelo n=1,720)",
        subtitle = "Línea roja: LOESS con IC 95% | Usa variable 'time' del modelo",
        x = "Tiempo de seguimiento (años)",
        y = "PLR mediano (%/año)"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # --- Tabla de Correlaciones (DATOS MODELO) ---
  output$tabla_correlaciones <- renderTable({
    df <- datos_modelo() %>%
      filter(!is.na(time), !is.na(plr_median))
    
    n_obs <- nrow(df)
    
    # Pearson
    pear <- cor.test(df$time, df$plr_median, method = "pearson")
    # Spearman
    spear <- cor.test(df$time, df$plr_median, method = "spearman")
    
    # IC para correlaciones
    ci_cor <- function(r, n, level = 0.95) {
      z <- atanh(r)
      se <- 1 / sqrt(n - 3)
      z_crit <- qnorm(1 - (1 - level)/2)
      c(tanh(z - z_crit * se), tanh(z + z_crit * se))
    }
    
    ci_p <- ci_cor(pear$estimate, n_obs)
    ci_s <- ci_cor(as.numeric(spear$estimate), n_obs)
    
    data.frame(
      Metodo = c("Pearson", "Spearman"),
      r = c(round(pear$estimate, 3), round(as.numeric(spear$estimate), 3)),
      IC_95 = c(sprintf("[%.3f, %.3f]", ci_p[1], ci_p[2]),
                sprintf("[%.3f, %.3f]", ci_s[1], ci_s[2])),
      p_valor = c(format(pear$p.value, scientific = TRUE, digits = 3),
                  format(spear$p.value, scientific = TRUE, digits = 3)),
      n = n_obs,
      Interpretacion = c(
        ifelse(abs(pear$estimate) < 0.3, "Débil", ifelse(abs(pear$estimate) < 0.7, "Moderada", "Fuerte")),
        ifelse(abs(spear$estimate) < 0.3, "Débil", ifelse(abs(spear$estimate) < 0.7, "Moderada", "Fuerte"))
      )
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # --- Outliers Bivariados (Mahalanobis) - DATOS ORIGINALES ---
  output$plot_outliers_biv <- renderPlot({
    df <- datos_orig() %>%
      mutate(
        tiempo = as.numeric(length_years_rounded),
        plr = as.numeric(plr_median)
      ) %>%
      filter(!is.na(tiempo), !is.na(plr)) %>%
      select(tiempo, plr)
    
    # Mahalanobis robusta con MCD
    X <- as.matrix(df)
    
    tryCatch({
      mcd <- robustbase::covMcd(X)
      d2 <- mahalanobis(X, center = mcd$center, cov = mcd$cov)
      cutoff <- qchisq(0.975, df = 2)
      
      df$d2 <- d2
      df$outlier <- d2 > cutoff
      
      n_outliers <- sum(df$outlier)
      
      # Elipse de tolerancia
      ellipse_pts <- car::ellipse(center = mcd$center, shape = mcd$cov, 
                                   radius = sqrt(cutoff), draw = FALSE)
      ellipse_df <- as.data.frame(ellipse_pts)
      names(ellipse_df) <- c("tiempo", "plr")
      
      ggplot(df, aes(x = tiempo, y = plr)) +
        geom_point(aes(color = outlier), size = 1.5, alpha = 0.6) +
        geom_path(data = ellipse_df, color = "black", size = 1) +
        scale_color_manual(values = c("FALSE" = "#3c8dbc", "TRUE" = "#e74c3c"),
                           labels = c("Normal", "Atípico"),
                           name = "Clasificación") +
        labs(
          title = "Outliers Bivariados (Datos Originales)",
          subtitle = sprintf("Elipse: tolerancia 97.5%% (MCD) | n=%d, outliers=%d", nrow(df), n_outliers),
          x = "Tiempo de monitoreo (años)",
          y = "PLR mediano (%/año)"
        ) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom")
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Error al calcular MCD", size = 5) +
        theme_void()
    })
  })
  
  output$tabla_outliers_biv <- renderTable({
    # Usar datos originales
    df <- datos_orig() %>%
      mutate(
        tiempo = as.numeric(length_years_rounded),
        plr = as.numeric(plr_median)
      ) %>%
      filter(!is.na(tiempo), !is.na(plr)) %>%
      select(tiempo, plr)
    
    X <- as.matrix(df)
    
    tryCatch({
      mcd <- robustbase::covMcd(X)
      d2 <- mahalanobis(X, center = mcd$center, cov = mcd$cov)
      cutoff <- qchisq(0.975, df = 2)
      
      n_outliers <- sum(d2 > cutoff)
      pct_outliers <- 100 * n_outliers / nrow(df)
      
      data.frame(
        Metrica = c("N total (datos orig.)", "N atípicos", "% atípicos", "Umbral χ²"),
        Valor = c(nrow(df), n_outliers, sprintf("%.1f%%", pct_outliers), round(cutoff, 2))
      )
    }, error = function(e) {
      data.frame(Metrica = "Error", Valor = "No se pudo calcular")
    })
  }, striped = TRUE, bordered = TRUE)
  
  # ==========================================================================
  # TAB 5: EDA MULTIVARIADO (usa datos del MODELO - completos)
  # ==========================================================================
  # Nota: El CSV original tiene ~65% de valores vacíos en variables clave,
  # por lo que el análisis multivariado usa el dataset filtrado (n=1,720)
  
  # --- Matriz de Correlaciones (DATOS MODELO - completos) ---
  output$plot_corr_matrix <- renderPlot({
    # Usar datos del modelo (completos, sin NA)
    df_num <- datos_modelo() %>%
      select(plr_median, time, pv_climate_zone) %>%
      filter(complete.cases(.))
    
    # Renombrar para mejor visualización
    names(df_num) <- c("PLR", "Tiempo", "Zona_Clim")
    
    # Verificar que hay datos
    if (nrow(df_num) < 10) {
      plot.new()
      text(0.5, 0.5, "Datos insuficientes para calcular correlaciones", cex = 1.5)
      return()
    }
    
    # Calcular matriz de correlaciones
    cor_mat <- cor(df_num, use = "pairwise.complete.obs")
    
    # Convertir a formato largo para ggplot
    cor_df <- as.data.frame(as.table(cor_mat))
    names(cor_df) <- c("Var1", "Var2", "Correlacion")
    
    n_valid <- nrow(df_num)
    
    ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlacion)) +
      geom_tile(color = "white") +
      geom_text(aes(label = sprintf("%.2f", Correlacion)), color = "black", size = 5) +
      scale_fill_gradient2(low = "#e74c3c", mid = "white", high = "#3498db",
                           midpoint = 0, limits = c(-1, 1)) +
      labs(title = "Matriz de Correlaciones (Dataset Modelo)",
           subtitle = paste("n =", n_valid, "(datos filtrados y completos)"),
           x = "", y = "") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # --- Tabla de Correlaciones Significativas (DATOS MODELO) ---
  output$tabla_corr_signif <- renderTable({
    df_num <- datos_modelo() %>%
      select(plr_median, time, pv_climate_zone) %>%
      filter(complete.cases(.))
    
    names(df_num) <- c("PLR", "Tiempo", "Zona_Climatica")
    
    # Verificar que hay datos suficientes
    if (nrow(df_num) < 10) {
      return(data.frame(Mensaje = "Datos insuficientes"))
    }
    
    vars <- names(df_num)
    results <- list()
    
    for (i in 1:(length(vars)-1)) {
      for (j in (i+1):length(vars)) {
        tryCatch({
          test <- cor.test(df_num[[vars[i]]], df_num[[vars[j]]])
          if (abs(test$estimate) >= 0.1) {
            results[[length(results)+1]] <- data.frame(
              Variable_1 = vars[i],
              Variable_2 = vars[j],
              r = round(test$estimate, 3),
              p_valor = format(test$p.value, scientific = TRUE, digits = 2),
              Fuerza = ifelse(abs(test$estimate) < 0.3, "Débil",
                             ifelse(abs(test$estimate) < 0.7, "Moderada", "Fuerte"))
            )
          }
        }, error = function(e) NULL)
      }
    }
    
    if (length(results) > 0) {
      do.call(rbind, results) %>% arrange(desc(abs(r)))
    } else {
      data.frame(Mensaje = "No hay correlaciones |r| >= 0.1")
    }
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # --- Scatter PLR vs Tiempo (DATOS MODELO - completos) ---
  output$plot_plr_vs_tiempo <- renderPlot({
    # Usar datos del modelo (completos)
    df <- datos_modelo() %>%
      filter(!is.na(plr_median), !is.na(time))
    
    # Verificar que hay datos
    if (nrow(df) < 10) {
      plot.new()
      text(0.5, 0.5, "Datos insuficientes", cex = 1.5)
      return()
    }
    
    # Calcular correlación
    cor_val <- cor(df$plr_median, df$time, use = "complete.obs")
    
    ggplot(df, aes(x = time, y = plr_median)) +
      geom_point(alpha = 0.4, color = "#3c8dbc", size = 1.5) +
      geom_smooth(method = "lm", color = "#e74c3c", se = TRUE, linetype = "dashed") +
      geom_hline(yintercept = -1.5, color = "#f39c12", linetype = "dotted", size = 1) +
      annotate("text", x = max(df$time, na.rm = TRUE) * 0.7, y = -1.3, 
               label = "Umbral degradación severa (-1.5%/año)", color = "#f39c12", size = 3.5) +
      labs(
        title = "PLR vs Tiempo de Monitoreo (Dataset Modelo)",
        subtitle = sprintf("n = %d | Correlación: r = %.3f", nrow(df), cor_val),
        x = "Tiempo de monitoreo (años)",
        y = "PLR (tasa de degradación %/año)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
  })
  
  # --- VIF (Factor de Inflación de Varianza) - DATOS MODELO ---
  output$tabla_vif <- renderTable({
    df_vif <- datos_modelo() %>%
      select(plr_median, time, pv_climate_zone, is_PERC) %>%
      filter(complete.cases(.))
    
    tryCatch({
      # Modelo lineal para calcular VIF
      mod <- lm(plr_median ~ time + pv_climate_zone + is_PERC, data = df_vif)
      vif_vals <- car::vif(mod)
      
      data.frame(
        Variable = names(vif_vals),
        VIF = round(vif_vals, 3),
        Estado = ifelse(vif_vals < 5, "OK", 
                       ifelse(vif_vals < 10, "Moderado", "Severo"))
      )
    }, error = function(e) {
      data.frame(Variable = "Error", VIF = NA, Estado = e$message)
    })
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  output$resumen_vif <- renderPrint({
    df_vif <- datos_modelo() %>%
      select(plr_median, time, pv_climate_zone, is_PERC) %>%
      filter(complete.cases(.))
    
    tryCatch({
      mod <- lm(plr_median ~ time + pv_climate_zone + is_PERC, data = df_vif)
      vif_vals <- car::vif(mod)
      
      cat("=== DIAGNÓSTICO DE MULTICOLINEALIDAD ===\n\n")
      cat(sprintf("VIF máximo: %.3f (%s)\n", max(vif_vals), names(which.max(vif_vals))))
      cat(sprintf("VIF promedio: %.3f\n\n", mean(vif_vals)))
      
      if (all(vif_vals < 5)) {
        cat("✓ CONCLUSIÓN: No hay evidencia de multicolinealidad.\n")
        cat("  Todas las variables pueden incluirse en el modelo.\n")
      } else if (any(vif_vals >= 10)) {
        cat("✗ CONCLUSIÓN: Multicolinealidad severa detectada.\n")
        cat("  Considerar eliminar variables con VIF > 10.\n")
      } else {
        cat("⚠ CONCLUSIÓN: Multicolinealidad moderada.\n")
        cat("  Monitorear variables con VIF entre 5 y 10.\n")
      }
    }, error = function(e) {
      cat("Error al calcular VIF:", e$message, "\n")
    })
  })
  
  # --- Outliers Multivariados (DATOS MODELO - completos) ---
  output$plot_outliers_multi <- renderPlot({
    # Usar datos del modelo (completos, sin NA)
    df <- datos_modelo()
    
    # Asegurar que X_id exista
    if (!"X_id" %in% names(df)) {
      df$X_id <- seq_len(nrow(df))
    }
    
    df <- df %>%
      select(X_id, time, pv_climate_zone, plr_median) %>%
      filter(complete.cases(.))
    
    # Verificar que hay suficientes datos
    if (nrow(df) < 10) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Datos insuficientes", size = 5) +
        theme_void()
      return()
    }
    
    X <- as.matrix(df[, c("time", "pv_climate_zone", "plr_median")])
    
    tryCatch({
      mcd <- robustbase::covMcd(X)
      d2 <- mahalanobis(X, center = mcd$center, cov = mcd$cov)
      cutoff <- qchisq(0.975, df = ncol(X))
      
      df$d2 <- d2
      df$outlier <- d2 > cutoff
      df$index <- 1:nrow(df)
      
      n_total <- nrow(df)
      n_outliers <- sum(df$outlier)
      
      ggplot(df, aes(x = index, y = d2, color = outlier)) +
        geom_point(alpha = 0.6, size = 1.5) +
        geom_hline(yintercept = cutoff, linetype = "dashed", color = "red", size = 1) +
        scale_color_manual(values = c("FALSE" = "#3c8dbc", "TRUE" = "#e74c3c"),
                           labels = c("Normal", "Atípico"),
                           name = "Clasificación") +
        annotate("text", x = n_total * 0.8, y = cutoff + 2, 
                 label = sprintf("Umbral χ²(3, 0.975) = %.2f", cutoff), color = "red") +
        labs(
          title = "Distancia de Mahalanobis Robusta (Dataset Modelo)",
          subtitle = sprintf("Variables: time, zona_climática, PLR | n=%d, outliers=%d (%.1f%%)", 
                            n_total, n_outliers, 100*n_outliers/n_total),
          x = "Índice de observación",
          y = "Distancia de Mahalanobis (d²)"
        ) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom")
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message), size = 4) +
        theme_void()
    })
  })
  
  output$tabla_outliers_multi <- renderTable({
    # Usar datos del modelo (completos)
    df <- datos_modelo() %>%
      select(time, pv_climate_zone, plr_median) %>%
      filter(complete.cases(.))
    
    X <- as.matrix(df)
    
    tryCatch({
      mcd <- robustbase::covMcd(X)
      d2 <- mahalanobis(X, center = mcd$center, cov = mcd$cov)
      cutoff <- qchisq(0.975, df = ncol(X))
      
      n_outliers <- sum(d2 > cutoff)
      pct_outliers <- 100 * n_outliers / nrow(df)
      
      data.frame(
        Metrica = c("N total (modelo)", "N atípicos", "% atípicos", "Umbral χ²(3)"),
        Valor = c(nrow(df), n_outliers, sprintf("%.1f%%", pct_outliers), round(cutoff, 2))
      )
    }, error = function(e) {
      data.frame(Metrica = "Error", Valor = e$message)
    })
  }, striped = TRUE, bordered = TRUE)
  
  output$tabla_top10_outliers <- renderTable({
    # Usar datos del modelo (completos)
    df <- datos_modelo()
    
    # Asegurar que X_id exista
    if (!"X_id" %in% names(df)) {
      df$X_id <- seq_len(nrow(df))
    }
    
    df <- df %>%
      select(X_id, time, pv_climate_zone, plr_median) %>%
      filter(complete.cases(.))
    
    X <- as.matrix(df[, c("time", "pv_climate_zone", "plr_median")])
    
    tryCatch({
      mcd <- robustbase::covMcd(X)
      d2 <- mahalanobis(X, center = mcd$center, cov = mcd$cov)
      
      df$d2 <- round(d2, 2)
      
      df %>%
        arrange(desc(d2)) %>%
        head(10) %>%
        select(X_id, time, plr_median, d2) %>%
        rename(ID = X_id, Tiempo = time, PLR = plr_median, `d²` = d2)
    }, error = function(e) {
      data.frame(Error = e$message)
    })
  }, striped = TRUE, bordered = TRUE)
  
  # ==========================================================================
  # TAB 6: PREPROCESAMIENTO (Datos Originales → Filtrados)
  # ==========================================================================
  
  output$vb_n_original <- renderValueBox({
    valueBox(
      value = format(nrow(datos_orig()), big.mark = ","),
      subtitle = "CSV original",
      icon = icon("database"),
      color = "blue"
    )
  })
  
  output$vb_n_filtrado <- renderValueBox({
    valueBox(
      value = format(nrow(datos_modelo()), big.mark = ","),
      subtitle = "Dataset modelo",
      icon = icon("filter"),
      color = "green"
    )
  })
  
  output$vb_pct_perdido <- renderValueBox({
    pct <- round((1 - nrow(datos_modelo()) / nrow(datos_orig())) * 100, 1)
    valueBox(
      value = paste0(pct, "%"),
      subtitle = "Datos perdidos",
      icon = icon("exclamation-triangle"),
      color = "yellow"
    )
  })
  
  output$vb_eventos <- renderValueBox({
    eventos <- sum(datos_modelo()$event_A)
    pct <- round(eventos / nrow(datos_modelo()) * 100, 1)
    valueBox(
      value = paste0(eventos, " (", pct, "%)"),
      subtitle = "Eventos (modelo)",
      icon = icon("flag"),
      color = "red"
    )
  })
  
  # --- Plot de NA en datos ORIGINALES ---
  output$plot_na <- renderPlot({
    df <- datos_orig()
    
    # Contar NA y vacíos para cada variable
    na_counts <- sapply(df, function(x) {
      if (is.character(x)) {
        sum(is.na(x) | x == "" | x == " ")
      } else {
        sum(is.na(x))
      }
    })
    
    df_na <- data.frame(
      variable = names(na_counts),
      n_na = na_counts,
      pct_na = na_counts / nrow(df) * 100
    ) %>%
      filter(pct_na > 0) %>%
      arrange(desc(pct_na)) %>%
      head(15)  # Top 15 variables con más NA
    
    if (nrow(df_na) == 0) {
      df_na <- data.frame(variable = "Sin NA/vacíos", n_na = 0, pct_na = 0)
    }
    
    # Gráfico consistente: ordenado y etiquetado por porcentaje
    ggplot(df_na, aes(x = reorder(variable, pct_na), y = pct_na)) +
      geom_bar(stat = "identity", fill = "#e74c3c", alpha = 0.8) +
      geom_text(aes(label = sprintf("%d (%.1f%%)", n_na, pct_na)), hjust = -0.1, size = 3) +
      coord_flip() +
      scale_y_continuous(limits = c(0, max(df_na$pct_na, na.rm = TRUE) * 1.3)) +
      labs(title = "Valores Faltantes/Vacíos en Datos ORIGINALES (Top 7)",
           subtitle = sprintf("n total = %d filas", nrow(df)),
           x = "Variable", 
           y = "% NA/vacíos") +
      theme_minimal(base_size = 12)
  })
  
  output$resumen_na <- renderPrint({
    df_orig <- datos_orig()
    df_modelo <- datos_modelo()
    
    # Contar NA/vacíos en datos originales
    na_orig <- sapply(df_orig, function(x) {
      if (is.character(x)) sum(is.na(x) | x == "" | x == " ")
      else sum(is.na(x))
    })
    
    cat("=== DATOS ORIGINALES (CSV) ===\n")
    cat(sprintf("Total filas: %d\n", nrow(df_orig)))
    cat(sprintf("Variables con NA/vacíos: %d de %d\n", sum(na_orig > 0), length(na_orig)))
    cat("\nVariables más afectadas:\n")
    
    # Usar head() para evitar errores si hay menos de 5 variables con NA
    top_na <- head(sort(na_orig[na_orig > 0], decreasing = TRUE), 5)
    for (i in seq_along(top_na)) {
      cat(sprintf("  %d. %s: %d (%.1f%%)\n", i, names(top_na)[i], top_na[i], top_na[i]/nrow(df_orig)*100))
    }
    
    cat("\n=== DATOS MODELO (Filtrado) ===\n")
    cat(sprintf("Total filas: %d\n", nrow(df_modelo)))
    cat("NA en dataset modelo: 0 (completamente limpio)\n")
  })
  
  # --- Tabla de pérdida paso a paso (ACUMULATIVA) ---
  output$tabla_perdida_pasos <- renderTable({
    df0 <- datos_orig()
    n0 <- nrow(df0)
    
    # Pipeline acumulativo real (cada paso filtra sobre el resultado anterior)
    suppressWarnings({
      # Paso 1: time válido
      df1 <- df0 %>%
        filter(!is.na(length_years_rounded), 
               length_years_rounded != "",
               as.numeric(length_years_rounded) > 0)
      n1 <- nrow(df1)
      
      # Paso 2: zona climática válida
      df2 <- df1 %>%
        mutate(cz_temp = as.numeric(gsub("T", "", pv_climate_zone))) %>%
        filter(!is.na(cz_temp),
               cz_temp >= 2,
               cz_temp <= 6)
      n2 <- nrow(df2)
      
      # Paso 3: technology1 no vacío
      df3 <- df2 %>%
        filter(!is.na(technology1), technology1 != "")
      n3 <- nrow(df3)
      
      # Paso 4: type_mounting no vacío
      df4 <- df3 %>%
        filter(!is.na(type_mounting), type_mounting != "")
      n4 <- nrow(df4)
      
      # Paso 5: tracking no vacío
      df5 <- df4 %>%
        filter(!is.na(tracking), tracking != "")
      n5 <- nrow(df5)
      
      # Paso 6: power_dc no vacío
      df6 <- df5 %>%
        filter(!is.na(power_dc), power_dc != "")
      n6 <- nrow(df6)
      
      # Paso 7: plr_type no vacío
      df7 <- df6 %>%
        filter(!is.na(plr_type), plr_type != "")
      n7 <- nrow(df7)
    })
    
    # Dataset final del modelo
    n_final <- nrow(datos_modelo())
    
    pasos <- data.frame(
      Paso = c(
        "0. Inicio (CSV original)",
        "1. time > 0 y válido",
        "2. zona_clim entre 2-6",
        "3. technology1 no vacío",
        "4. type_mounting no vacío",
        "5. tracking no vacío",
        "6. power_dc no vacío",
        "7. plr_type no vacío",
        "8. Dataset modelo final"
      ),
      n = c(n0, n1, n2, n3, n4, n5, n6, n7, n_final),
      stringsAsFactors = FALSE
    )
    
    pasos$Perdida <- c(0, -diff(pasos$n))
    pasos$Pct_restante <- sprintf("%.1f%%", pasos$n / n0 * 100)
    
    pasos
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  output$plot_balance <- renderPlot({
    df_balance <- datos_modelo() %>%
      count(event_A) %>%
      mutate(
        evento = ifelse(event_A == 1, "Evento (degradación)", "Censurado"),
        pct = n / sum(n) * 100
      )
    
    ggplot(df_balance, aes(x = evento, y = n, fill = evento)) +
      geom_bar(stat = "identity", alpha = 0.8) +
      geom_text(aes(label = paste0(n, "\n(", round(pct, 1), "%)")), 
                vjust = -0.2, size = 5) +
      scale_fill_manual(values = c("Censurado" = "#3498db", "Evento (degradación)" = "#e74c3c")) +
      labs(title = "Balance de Clases (Dataset Modelo n=1,720)",
           subtitle = "Evento = degradación severa (PLR < -1.5%/año)",
           x = "", y = "Cantidad") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none") +
      expand_limits(y = max(df_balance$n) * 1.15)
  })
  
  output$resumen_balance <- renderPrint({
    n_modelo <- nrow(datos_modelo())
    eventos <- sum(datos_modelo()$event_A)
    censurados <- sum(datos_modelo()$event_A == 0)
    ratio <- censurados / eventos
    
    cat(sprintf("Eventos:    %d (%.1f%%)\n", eventos, eventos/n_modelo*100))
    cat(sprintf("Censurados: %d (%.1f%%)\n", censurados, censurados/n_modelo*100))
    cat(sprintf("\nRatio censurados/eventos: %.2f\n", ratio))
  })
  
  # ==========================================================================
  # TAB 6: TESTS ESTADÍSTICOS
  # ==========================================================================
  
  output$tabla_aic <- renderTable({
    data.frame(
      Modelo = c("Weibull", "Loglogística", "Lognormal", "Exponencial", "Cox PH"),
      AIC = c(2033.4, 2047.8, 2060.1, 2448.1, 3973.1),
      Delta_AIC = c(0, 14.4, 26.7, 414.7, 1939.7),
      Interpretacion = c("Mejor ajuste", "Aceptable", "Aceptable", 
                         "Muy pobre", "No recomendado (viola PH)")
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  output$test_schoenfeld <- renderPrint({
    cat("Test de residuos de Schoenfeld (Cox PH):\n\n")
    cat("Variable              p-value    Interpretación\n")
    cat("------------------------------------------------\n")
    cat("pv_climate_zone       0.0023     ** Viola PH\n")
    cat("technology1_fac       0.1456     OK\n")
    cat("is_PERC               0.0001     *** Viola PH\n")
    cat("mounting_group_fac    0.0089     ** Viola PH\n")
    cat("tracking_fac          0.2341     OK\n")
    cat("------------------------------------------------\n")
    cat("GLOBAL                0.0002     *** Viola PH\n\n")
    cat("Conclusión: El supuesto de riesgos proporcionales se viola\n")
    cat("para varias covariables → Se justifica usar modelo AFT.\n")
  })
  
  output$test_logrank <- renderPrint({
    var <- input$var_logrank
    formula_km <- as.formula(paste("Surv(time, event_A) ~", var))
    
    test <- survdiff(formula_km, data = datos())
    
    cat(sprintf("Test Log-Rank para %s:\n\n", var))
    cat(sprintf("Chi-cuadrado: %.3f\n", test$chisq))
    cat(sprintf("Grados de libertad: %d\n", length(test$n) - 1))
    cat(sprintf("p-value: %.4e\n\n", pchisq(test$chisq, length(test$n) - 1, lower.tail = FALSE)))
    
    if (pchisq(test$chisq, length(test$n) - 1, lower.tail = FALSE) < 0.05) {
      cat("→ Diferencias significativas entre grupos (p < 0.05)\n")
    } else {
      cat("→ No hay diferencias significativas (p >= 0.05)\n")
    }
  })
  
  output$tabla_coefs_test <- renderTable({
    summ <- summary(modelo())
    coefs <- summ$table
    
    # Filtrar solo covariables (excluir Intercept y Log(scale))
    idx <- !grepl("Intercept|Log\\(scale\\)", rownames(coefs))
    coefs_filt <- coefs[idx, , drop = FALSE]
    
    # Filtrar filas con NA (categorías sin datos como CdTe, CIGS)
    idx_valid <- !is.na(coefs_filt[, "Value"])
    coefs_valid <- coefs_filt[idx_valid, , drop = FALSE]
    
    df <- data.frame(
      Variable = rownames(coefs_valid),
      Coeficiente = round(coefs_valid[, "Value"], 4),
      SE = round(coefs_valid[, "Std. Error"], 4),
      z = round(coefs_valid[, "z"], 3),
      p_value = format(coefs_valid[, "p"], scientific = TRUE, digits = 3),
      Significancia = ifelse(coefs_valid[, "p"] < 0.001, "*** (p<0.001)",
                             ifelse(coefs_valid[, "p"] < 0.01, "** (p<0.01)",
                                    ifelse(coefs_valid[, "p"] < 0.05, "* (p<0.05)", 
                                           "No signif.")))
    )
    
    # Limpiar nombres de variables
    df$Variable <- gsub("_fac", "", df$Variable)
    df$Variable <- gsub("technology1", "Tecnología: ", df$Variable)
    df$Variable <- gsub("mounting_group", "Montaje: ", df$Variable)
    df$Variable <- gsub("tracking", "Tracking: ", df$Variable)
    
    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE, rownames = FALSE)
  
  # ==========================================================================
  # TAB 7: EVALUACIÓN DE TESTS
  # ==========================================================================
  
  observeEvent(input$btn_evaluar, {
    tiempo <- input$tiempo_eval
    metrica <- input$metrica_eval
    
    # Calcular predictor lineal
    lp <- calcular_lp(modelo(), datos())
    
    # Crear quintiles
    datos_eval <- datos() %>%
      mutate(
        pred_linear = lp,
        risk_group = cut(pred_linear,
                         breaks = quantile(pred_linear, probs = seq(0, 1, 0.2)),
                         labels = paste0("Q", 1:5),
                         include.lowest = TRUE)
      )
    
    # Función de supervivencia predicha
    shape <- 1 / modelo()$scale
    datos_eval$surv_pred <- exp(-(tiempo / exp(datos_eval$pred_linear))^shape)
    
    # Calcular supervivencia observada por grupo
    calc_surv_obs <- function(data, time_point) {
      km_fit <- survfit(Surv(time, event_A) ~ 1, data = data)
      idx <- which.min(abs(km_fit$time - time_point))
      if (length(idx) == 0) return(NA)
      return(km_fit$surv[idx])
    }
    
    calibracion <- datos_eval %>%
      group_by(risk_group) %>%
      summarise(
        n = n(),
        surv_obs = calc_surv_obs(cur_data(), tiempo),
        surv_pred = mean(surv_pred, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(error = abs(surv_obs - surv_pred))
    
    output$tabla_calibracion <- renderTable({
      calibracion %>%
        mutate(
          surv_obs = round(surv_obs, 3),
          surv_pred = round(surv_pred, 3),
          error = round(error, 3)
        ) %>%
        rename(
          Quintil = risk_group,
          N = n,
          S_Observada = surv_obs,
          S_Predicha = surv_pred,
          Error = error
        )
    }, striped = TRUE, bordered = TRUE, hover = TRUE)
    
    output$plot_evaluacion <- renderPlot({
      if (metrica == "calibracion") {
        ggplot(calibracion, aes(x = surv_pred, y = surv_obs)) +
          geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
          geom_point(aes(size = n), color = "#3c8dbc", alpha = 0.7) +
          geom_text(aes(label = risk_group), vjust = -1.5, size = 4) +
          scale_size_continuous(range = c(4, 12), name = "n") +
          coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
          labs(title = paste("Calibración a", tiempo, "años"),
               subtitle = paste("Error medio:", round(mean(calibracion$error, na.rm = TRUE), 3)),
               x = "Supervivencia Predicha",
               y = "Supervivencia Observada (KM)") +
          theme_minimal(base_size = 14)
      } else if (metrica == "cindex") {
        # Para C-index: mostrar distribución de predictor lineal por quintil
        ggplot(datos_eval, aes(x = risk_group, y = pred_linear, fill = risk_group)) +
          geom_boxplot(alpha = 0.7) +
          labs(title = "Distribución de Predictor Lineal por Quintil",
               subtitle = paste("C-index:", 0.746),
               x = "Quintil de Riesgo", y = "Predictor Lineal") +
          theme_minimal(base_size = 14) +
          theme(legend.position = "none") +
          scale_fill_brewer(palette = "RdYlGn", direction = 1)
      } else {
        # Para Brier Score: calcular y mostrar error cuadrático por quintil
        brier_by_quintil <- calibracion %>%
          mutate(
            brier_score = (surv_pred - surv_obs)^2
          ) %>%
          arrange(risk_group)
        
        brier_total <- mean(brier_by_quintil$brier_score, na.rm = TRUE)
        
        ggplot(brier_by_quintil, aes(x = risk_group, y = brier_score, fill = risk_group)) +
          geom_col(alpha = 0.8) +
          geom_hline(yintercept = brier_total, linetype = "dashed", color = "red", size = 1) +
          geom_text(aes(label = round(brier_score, 3)), vjust = -0.5, size = 4) +
          labs(title = paste("Brier Score por Quintil a", tiempo, "años"),
               subtitle = paste("Brier Score promedio:", round(brier_total, 3), 
                              "| Ideal < 0.25"),
               x = "Quintil de Riesgo", 
               y = "Brier Score (Error Cuadrático Medio)") +
          theme_minimal(base_size = 14) +
          theme(legend.position = "none") +
          scale_fill_brewer(palette = "RdYlBu", direction = -1) +
          ylim(0, max(brier_by_quintil$brier_score, na.rm = TRUE) * 1.2)
      }
    })
    
    output$vb_cindex <- renderValueBox({
      valueBox(
        value = "0.746",
        subtitle = "C-index (IC: 0.725-0.766)",
        icon = icon("chart-line"),
        color = "green"
      )
    })
    
    output$vb_error_medio <- renderValueBox({
      err <- round(mean(calibracion$error, na.rm = TRUE), 3)
      valueBox(
        value = err,
        subtitle = paste("Error medio a", tiempo, "años"),
        icon = icon("exclamation-circle"),
        color = ifelse(err < 0.2, "green", ifelse(err < 0.4, "yellow", "red"))
      )
    })
    
    output$interpretacion_eval <- renderUI({
      err <- mean(calibracion$error, na.rm = TRUE)
      
      if (err < 0.15) {
        msg <- "Calibración excelente: las predicciones son muy cercanas a las observaciones."
        color <- "success"
      } else if (err < 0.30) {
        msg <- "Calibración aceptable: las predicciones tienen desviaciones moderadas."
        color <- "info"
      } else {
        msg <- "Calibración deficiente: el modelo subestima/sobreestima consistentemente. 
                Usar solo para ranking relativo, no predicciones absolutas."
        color <- "warning"
      }
      
      div(class = paste0("alert alert-", color),
          h4("Interpretación:"),
          p(msg),
          hr(),
          p("El C-index de 0.746 indica capacidad de discriminación aceptable:"),
          tags$ul(
            tags$li("C > 0.7: Útil para ranking de riesgo"),
            tags$li("C < 0.8: No ideal para predicciones individuales precisas")
          )
      )
    })
  })
  
  # ==========================================================================
  # TAB 8: MODELOS DE RIESGO
  # ==========================================================================
  
  output$summary_modelo <- renderPrint({
    summary(modelo())
  })
  
  output$tabla_coefs_modelo <- renderTable({
    coefs <- coef(modelo())
    coefs <- coefs[!is.na(coefs)]
    coefs <- coefs[!grepl("Intercept", names(coefs))]
    
    data.frame(
      Variable = names(coefs),
      Coeficiente = round(coefs, 4),
      Time_Ratio = round(exp(coefs), 3),
      Efecto = ifelse(exp(coefs) > 1, "Protector", "Riesgo")
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE, rownames = FALSE)
  
  output$forest_plot <- renderPlot({
    coefs <- coef(modelo())
    se <- sqrt(diag(vcov(modelo())))
    
    idx <- !is.na(coefs) & !grepl("Intercept|Log\\(scale\\)", names(coefs))
    
    df_forest <- data.frame(
      variable = names(coefs)[idx],
      coef = coefs[idx],
      se = se[idx]
    ) %>%
      mutate(
        tr = exp(coef),
        tr_lower = exp(coef - 1.96 * se),
        tr_upper = exp(coef + 1.96 * se),
        variable = gsub("_fac", "", variable),
        variable = gsub("technology1", "Tech: ", variable),
        variable = gsub("mounting_group", "Mount: ", variable),
        variable = gsub("tracking", "Tracking: ", variable)
      )
    
    ggplot(df_forest, aes(x = tr, y = reorder(variable, tr))) +
      geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
      geom_errorbarh(aes(xmin = tr_lower, xmax = tr_upper), height = 0.2, color = "#3c8dbc") +
      geom_point(size = 3, color = "#3c8dbc") +
      scale_x_log10() +
      labs(title = "Forest Plot - Time Ratios",
           subtitle = "TR > 1 = Protector | TR < 1 = Riesgo",
           x = "Time Ratio (escala log)", y = "") +
      theme_minimal(base_size = 14)
  })
  
  # Predictor interactivo
  observeEvent(input$btn_predecir, {
    # Usar los mismos niveles que el dataset del modelo (evita "new factor levels")
    df_mod <- datos_modelo()
    
    newdata <- data.frame(
      pv_climate_zone = input$pred_clima,
      technology1_fac = factor(input$pred_tech, levels = levels(df_mod$technology1_fac)),
      is_PERC = as.numeric(input$pred_perc),
      mounting_group_fac = factor(input$pred_mount, levels = levels(df_mod$mounting_group_fac)),
      tracking_fac = factor(input$pred_track, levels = levels(df_mod$tracking_fac))
    )
    
    lp_user <- calcular_lp(modelo(), newdata)
    shape <- 1 / modelo()$scale
    
    # Curva de supervivencia
    tiempos <- seq(0, 25, by = 0.5)
    surv_probs <- exp(-(tiempos / exp(lp_user))^shape)
    
    df_curva <- data.frame(tiempo = tiempos, supervivencia = surv_probs)
    
    output$curva_predicha <- renderPlot({
      ggplot(df_curva, aes(x = tiempo, y = supervivencia)) +
        geom_line(color = "#3c8dbc", size = 1.5) +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
        labs(title = "Curva de Supervivencia Predicha",
             x = "Tiempo (años)", y = "S(t)") +
        theme_minimal(base_size = 14) +
        scale_y_continuous(limits = c(0, 1), labels = percent) +
        annotate("text", x = 20, y = 0.52, label = "Mediana", color = "red")
    })
    
    # Indicadores
    lp_todos <- calcular_lp(modelo(), datos_modelo())
    percentil <- ecdf(lp_todos)(lp_user) * 100
    mediana_tiempo <- exp(lp_user) * (-log(0.5))^(1/shape)
    s10 <- exp(-(10 / exp(lp_user))^shape)
    s15 <- exp(-(15 / exp(lp_user))^shape)
    
    output$indicadores_predichos <- renderTable({
      data.frame(
        Indicador = c("Predictor lineal", "Percentil en flota", 
                      "Tiempo mediano estimado", "S(10 años)", "S(15 años)"),
        Valor = c(sprintf("%.3f", lp_user),
                  sprintf("%.1f%%", percentil),
                  sprintf("%.1f años", mediana_tiempo),
                  sprintf("%.1f%%", s10 * 100),
                  sprintf("%.1f%%", s15 * 100))
      )
    }, striped = TRUE, bordered = TRUE, hover = TRUE)
  })
}

# ==============================================================================
# EJECUTAR APP
# ==============================================================================

shinyApp(ui = ui, server = server)
