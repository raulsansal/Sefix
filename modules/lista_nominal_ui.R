# modules/lista_nominal_ui.R
# Versión: 3.4 - Encabezado Ámbito/Alcance separado del DataTable
# Cambios vs v3.3:
#   - Nuevo uiOutput para Ámbito y Alcance ANTES del DataTable
#   - Título cambiado a "Tabla de Datos" (español)
#   - Encabezado dinámico se renderiza desde el server

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
        # ========== FIN SELECTOR NACIONAL/EXTRANJERO ==========
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
        
        # ========== FIN BOTÓN CONSULTAR ==========
        
        actionButton(ns("reset_config"), "Restablecer consulta", class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
        downloadButton(ns("download_csv"), "Descargar CSV", class = "btn-primary", style = "width:100%")
      ),
      
      mainPanel(
        width = 8,
        
        # ========== GRÁFICAS PARA HISTÓRICOS ==========
        conditionalPanel(
          condition = "input.tipo_corte == 'historico'",
          ns = ns,
          uiOutput(ns("graficas_dinamicas"))
        ),
        
        # ========== GRÁFICAS PARA SEMANALES (CON SPINNERS) ==========
        conditionalPanel(
          condition = "input.tipo_corte == 'semanal'",
          ns = ns,
          
          # Gráfico de barras actual
          fluidRow(
            column(12, 
                   div(class = "plot-container",
                       style = "height: 450px;",
                       shinycssloaders::withSpinner(
                         uiOutput(ns("main-plot_container")),
                         type = 6,
                         color = "#C0311A",
                         size = 1
                       )
                   )
            )
          ),
          
          # Gráfico de participación actual
          fluidRow(
            column(12,
                   div(class = "participacion-container",
                       style = "height: 500px; display: flex; flex-direction: column; align-items: center; justify-content: center;",
                       shinycssloaders::withSpinner(
                         plotlyOutput(ns("main-tasa_inclusion_plot"), width = "100%", height = "432px"),
                         type = 6,
                         color = "#4CAF50",
                         size = 1
                       )
                   )
            )
          )
        ),
        
        # ========== ✅ v3.4: DATATABLE CON ENCABEZADO SEPARADO ==========
        fluidRow(
          column(12, 
                 div(
                   class = "datatable-section",
                   
                   # ✅ v3.4: Título "Tabla de Datos" (en español)
                   h3("Tabla de Datos", 
                      align = "center", 
                      style = "margin-top: 40px;",
                      class = "datatable-title"),
                   
                   # ✅ v3.4: Encabezado con Ámbito y Alcance (renderizado desde server)
                   # Este div aparece ANTES del DataTable, no como caption interno
                   div(
                     class = "datatable-header",
                     uiOutput(ns("main-table_header"))
                   ),
                   
                   # DataTable con spinner
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
        # ========== FIN BOTÓN DESCARGAR CSV MÓVIL ==========
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
