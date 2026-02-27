# modules/lista_nominal_ui.R
# Versión: 3.5 - CORRECCIÓN RAÍZ: graficas_dinamicas cubre historico Y semanal
#
# PROBLEMA v3.4:
#   El uiOutput("graficas_dinamicas") solo estaba bajo condition="historico".
#   Para semanal, el conditionalPanel mostraba "main-plot_container" y
#   "main-tasa_inclusion_plot" — outputs que NO existen en el servidor.
#   Shiny es lazy: si un output no está en el DOM, nunca se solicita al
#   servidor, y los reactives que lo alimentan nunca se ejecutan.
#   Resultado: spinners infinitos en vista semanal.
#
# CORRECCIÓN v3.5:
#   - Eliminados los conditionalPanel separados para historico/semanal
#   - uiOutput("graficas_dinamicas") cubre AMBOS tipos de corte
#   - graficas_ui_render.R (que ya estaba correcto) decide qué mostrar
#     internamente según input$tipo_corte
#   - Eliminados "main-plot_container" y "main-tasa_inclusion_plot" (no existen)

lista_nominal_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        id = ns("sidebar_panel"),
        width = 4,
        
        radioButtons(
          ns("tipo_corte"), 
          "Tipo de datos:",
          choices = c("Histórico (Evolución anual y mensual)" = "historico", 
                      "Semanal (Detallado)" = "semanal"),
          selected = "historico"
        ),
        uiOutput(ns("info_tipo_corte")),
        tags$hr(),
        
        # ========== SELECTOR NACIONAL/EXTRANJERO ==========
        radioButtons(
          ns("ambito_datos"), 
          "Ámbito de datos:",
          choices = c("Nacional" = "nacional", 
                      "Extranjero" = "extranjero"),
          selected = "nacional",
          inline = TRUE
        ),
        tags$hr(),
        
        tags$small(
          style = "color: #000; display: block; font-weight: medium; margin-bottom: 8px; text-align: center; background-color: #CCE4B1; border-color #71A251; padding: 8px; border-radius: 4px;",
          "Configura los filtros y presiona el botón 'Consultar' para actualizar"
        ),
        selectInput(ns("year"), "Año:", choices = NULL, selected = NULL),
        selectInput(ns("entidad"), "Entidad:", choices = c("Nacional"), selected = "Nacional"),
        conditionalPanel(
          condition = "input.entidad != 'Nacional'",
          ns = ns,
          selectInput(ns("distrito"), "Distrito Electoral:", choices = c("Todos"), selected = "Todos"),
          selectInput(ns("municipio"), "Municipio:", choices = c("Todos"), selected = "Todos"),
          selectizeInput(
            ns("seccion"), 
            "Sección Electoral:", 
            choices = c("Todas"), 
            selected = "Todas", 
            multiple = TRUE,
            options = list(
              placeholder = "Selecciona una o más secciones",
              plugins = list("remove_button"),
              maxItems = NULL
            )
          )
        ),
        tags$hr(),
        
        # Selector de desglose SOLO para datos semanales
        conditionalPanel(
          condition = "input.tipo_corte == 'semanal'",
          ns = ns,
          uiOutput(ns("selector_desglose"))
        ),
        
        # ========== BOTÓN CONSULTAR ==========
        actionButton(
          ns("btn_consultar"), 
          "Consultar", 
          icon = icon("search"),
          class = "btn-success",
          style = "width: 100%; margin-bottom: 10px; font-weight: bold; font-size: 16px;"
        ),
        tags$hr(),
        
        actionButton(ns("reset_config"), "Restablecer consulta", class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
        downloadButton(ns("download_csv"), "Descargar CSV", class = "btn-primary", style = "width:100%")
      ),
      
      mainPanel(
        width = 8,
        
        # ✅ v3.5: graficas_dinamicas cubre AMBOS tipos de corte
        # graficas_ui_render.R decide internamente qué mostrar según tipo_corte
        uiOutput(ns("graficas_dinamicas")),
        
        # ========== ✅ v3.4: DATATABLE CON ENCABEZADO SEPARADO ==========
        fluidRow(
          column(12, 
                 div(
                   class = "datatable-section",
                   
                   h3("Tabla de Datos", 
                      align = "center", 
                      style = "margin-top: 40px;",
                      class = "datatable-title"),
                   
                   div(
                     class = "datatable-header",
                     uiOutput(ns("main-table_header"))
                   ),
                   
                   shinycssloaders::withSpinner(
                     DTOutput(ns("main-table_data")),
                     type = 6,
                     color = "#44559B",
                     size = 0.8
                   )
                 )
          )
        ),
        # ========== FIN DATATABLE ==========
        
        # ========== BOTÓN DESCARGAR CSV PARA MÓVIL ==========
        fluidRow(
          column(12,
                 div(
                   class = "mobile-download-container mobile-only",
                   style = "margin-top: 20px; margin-bottom: 80px; padding: 0 10px;",
                   downloadButton(
                     ns("download_csv_mobile"), 
                     "Descargar CSV", 
                     class = "btn-primary mobile-download-btn",
                     style = "width: 100%; font-size: 16px; padding: 12px; font-weight: bold;"
                   )
                 )
          )
        )
      )
    ),
    
    div(class = "toggle-container",
        actionButton(
          inputId = ns("toggle-sidebar-lista"), 
          label = ">>", 
          class = "toggle-sidebar-btn", 
          `data-sidebar-id` = ns("sidebar-right-lista")
        )
    ),
    
    div(
      id = ns("sidebar-right-lista"), 
      class = "sidebar-right",
      uiOutput(ns("text_analysis-titulo_lista")),
      uiOutput(ns("text_analysis-alcance_lista")),
      div(class = "sidebar-section", uiOutput(ns("text_analysis-resumen_general_lista"))),
      div(class = "sidebar-section", uiOutput(ns("text_analysis-demografia_lista"))),
      div(class = "sidebar-section", uiOutput(ns("text_analysis-comparacion_lista")))
    )
  )
}
