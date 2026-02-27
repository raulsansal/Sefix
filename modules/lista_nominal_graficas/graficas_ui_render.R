# modules/lista_nominal_graficas/graficas_ui_render.R
# Renderizado dinámico de UI para gráficas históricas Y semanales
# Versión: 2.5
#
# CAMBIOS vs v2.4:
#   - Agrega bloque de UI para vista semanal (tipo_corte == "semanal")
#   - El bloque semanal se renderiza automáticamente al iniciar
#     (estado "restablecido" + tipo_corte == "semanal")
#   - Incluye:
#       - Título principal dinámico (semanal_titulo_principal)
#       - Sección 1: Rango de Edad (A + B) + subtítulo + DataTable + botón descarga
#       - Sección 2: Distribución por Sexo (A + B) + subtítulo + DataTable + botón descarga
#       - Sección 3: Entidad de Origen (A + B, con selector Top N) + subtítulo + DataTable + botón descarga
#   - Mantiene comportamiento histórico intacto (gráficas 1-5)

graficas_ui_render <- function(input, output, session, estado_app,
                               mostrar_graficas_anuales,
                               mostrar_graficas_consultadas,
                               ambito_reactivo) {
  
  message("📊 Inicializando graficas_ui_render v2.5")
  ns <- session$ns
  
  output$graficas_dinamicas <- renderUI({
    
    estado_actual <- estado_app()
    
    # ── Determinar tipo de corte ─────────────────────────────────────────────
    tipo_corte_actual <- input$tipo_corte %||% "historico"
    
    message("🔄 [UI RENDER] Estado: ", estado_actual, " | tipo_corte: ", tipo_corte_actual)
    
    # ════════════════════════════════════════════════════════════════════════
    # VISTA SEMANAL
    # ════════════════════════════════════════════════════════════════════════
    
    if (tipo_corte_actual == "semanal") {
      
      if (estado_actual == "inicial") {
        return(div(
          style = "text-align:center;padding:40px;color:#999;",
          icon("chart-line", style = "font-size:48px;margin-bottom:20px;"),
          h4("Configure su consulta y presione 'Consultar' para visualizar las gráficas semanales",
             style = "color:#666;font-weight:normal;")
        ))
      }
      
      message("✅ [UI RENDER] Renderizando vista SEMANAL (estado: ", estado_actual, ")")
      
      return(tagList(
        
        # ── Título principal dinámico ──────────────────────────────────────
        uiOutput(ns("semanal_titulo_principal")),
        
        # ════════════════════════════════════════════════════════════════════
        # SECCIÓN 1: RANGO DE EDAD
        # ════════════════════════════════════════════════════════════════════
        hr(style = "border-top:2px solid #dee2e6;margin:20px 0 12px 0;"),
        h4("Rango de Edad",
           style = "font-weight:700;color:#2c3e50;font-family:Arial,sans-serif;margin-bottom:4px;"),
        uiOutput(ns("semanal_subtitulo_edad")),
        
        fluidRow(
          column(6,
                 div(class = "plot-container",
                     shinycssloaders::withSpinner(
                       plotlyOutput(ns("semanal_edad_piramide"), width = "100%", height = "100%"),
                       type = 6, color = "#44559B", size = 0.8
                     )
                 )
          ),
          column(6,
                 div(class = "plot-container",
                     shinycssloaders::withSpinner(
                       plotlyOutput(ns("semanal_edad_distribucion"), width = "100%", height = "100%"),
                       type = 6, color = "#44559B", size = 0.8
                     )
                 )
          )
        ),
        
        # DataTable Edad
        div(style = "margin-top:24px;",
            h5("Tabla de datos de Rango de Edad",
               style = "text-align:center;font-weight:700;color:#2c3e50;margin-bottom:2px;"),
            uiOutput(ns("semanal_subtitulo_edad")),
            div(style = "margin-top:8px;",
                DT::dataTableOutput(ns("semanal_dt_edad"))
            ),
            div(style = "text-align:right;margin-top:8px;",
                downloadButton(ns("semanal_dt_edad_descarga"),
                               label = "Descargar tabla de Edad",
                               class = "btn btn-sm btn-outline-secondary")
            )
        ),
        
        # ════════════════════════════════════════════════════════════════════
        # SECCIÓN 2: DISTRIBUCIÓN POR SEXO
        # ════════════════════════════════════════════════════════════════════
        hr(style = "border-top:2px solid #dee2e6;margin:28px 0 12px 0;"),
        h4("Distribución por Sexo",
           style = "font-weight:700;color:#2c3e50;font-family:Arial,sans-serif;margin-bottom:4px;"),
        uiOutput(ns("semanal_subtitulo_sexo")),
        
        fluidRow(
          column(6,
                 div(class = "plot-container",
                     shinycssloaders::withSpinner(
                       plotlyOutput(ns("semanal_sexo_barras"), width = "100%", height = "100%"),
                       type = 6, color = "#44559B", size = 0.8
                     )
                 )
          ),
          column(6,
                 div(class = "plot-container",
                     shinycssloaders::withSpinner(
                       plotlyOutput(ns("semanal_sexo_dona"), width = "100%", height = "100%"),
                       type = 6, color = "#44559B", size = 0.8
                     )
                 )
          )
        ),
        
        # DataTable Sexo
        div(style = "margin-top:24px;",
            h5("Tabla de datos de Distribución por Sexo",
               style = "text-align:center;font-weight:700;color:#2c3e50;margin-bottom:2px;"),
            uiOutput(ns("semanal_subtitulo_sexo")),
            div(style = "margin-top:8px;",
                DT::dataTableOutput(ns("semanal_dt_sexo"))
            ),
            div(style = "text-align:right;margin-top:8px;",
                downloadButton(ns("semanal_dt_sexo_descarga"),
                               label = "Descargar tabla de Sexo",
                               class = "btn btn-sm btn-outline-secondary")
            )
        ),
        
        # ════════════════════════════════════════════════════════════════════
        # SECCIÓN 3: ENTIDAD DE ORIGEN
        # ════════════════════════════════════════════════════════════════════
        hr(style = "border-top:2px solid #dee2e6;margin:28px 0 12px 0;"),
        h4("Entidad de Origen",
           style = "font-weight:700;color:#2c3e50;font-family:Arial,sans-serif;margin-bottom:4px;"),
        uiOutput(ns("semanal_subtitulo_origen")),
        
        # Selector Top N para gráfica B
        div(style = "display:flex;align-items:center;gap:12px;margin-bottom:10px;",
            span("Top estados de origen (gráfica derecha):",
                 style = "font-size:13px;color:#555;white-space:nowrap;"),
            selectInput(
              ns("semanal_top_n"),
              label = NULL,
              choices = c("5" = "5", "10" = "10", "15" = "15", "Todos" = "0"),
              selected = "5",
              width = "100px"
            )
        ),
        
        fluidRow(
          column(6,
                 div(class = "plot-container",
                     shinycssloaders::withSpinner(
                       plotlyOutput(ns("semanal_origen_calor"), width = "100%", height = "100%"),
                       type = 6, color = "#44559B", size = 0.8
                     )
                 )
          ),
          column(6,
                 div(class = "plot-container",
                     shinycssloaders::withSpinner(
                       plotlyOutput(ns("semanal_origen_barras"), width = "100%", height = "100%"),
                       type = 6, color = "#44559B", size = 0.8
                     )
                 )
          )
        ),
        
        # DataTable Origen
        div(style = "margin-top:24px;",
            h5("Tabla de datos de Entidad de Origen",
               style = "text-align:center;font-weight:700;color:#2c3e50;margin-bottom:2px;"),
            uiOutput(ns("semanal_subtitulo_origen")),
            div(style = "margin-top:8px;",
                DT::dataTableOutput(ns("semanal_dt_origen"))
            ),
            div(style = "text-align:right;margin-top:8px;",
                downloadButton(ns("semanal_dt_origen_descarga"),
                               label = "Descargar tabla de Origen",
                               class = "btn btn-sm btn-outline-secondary")
            )
        )
        
      )) # fin tagList semanal
    }
    
    # ════════════════════════════════════════════════════════════════════════
    # VISTA HISTÓRICA (lógica original intacta)
    # ════════════════════════════════════════════════════════════════════════
    
    if (estado_actual == "inicial") {
      message("⏸️ [UI RENDER] Estado inicial - NO renderizar gráficas")
      return(div(
        style = "text-align:center;padding:40px;color:#999;",
        icon("chart-line", style = "font-size:48px;margin-bottom:20px;"),
        h4("Configure su consulta y presione 'Consultar' para visualizar gráficas",
           style = "color:#666;font-weight:normal;")
      ))
    }
    
    if (estado_actual == "consultado") {
      if (is.null(input$btn_consultar) || input$btn_consultar == 0) {
        return(div(
          style = "text-align:center;padding:40px;color:#999;",
          icon("hourglass-half", style = "font-size:48px;margin-bottom:20px;"),
          h4("Procesando consulta...", style = "color:#666;font-weight:normal;")
        ))
      }
    }
    
    mostrar_anuales    <- isolate(mostrar_graficas_anuales())
    mostrar_consultadas <- isolate(mostrar_graficas_consultadas())
    
    # ── Gráficas 1, 2, 3 (año actual) ────────────────────────────────────────
    if (mostrar_anuales) {
      message("✅ [UI RENDER] Mostrando gráficas históricas 1, 2, 3")
      return(tagList(
        fluidRow(column(12,
                        div(class = "plot-container",
                            shinycssloaders::withSpinner(
                              plotlyOutput(ns("grafico_evolucion_2025"), width = "100%", height = "100%"),
                              type = 6, color = "#44559B", size = 0.8
                            )
                        ),
                        div(class = "metodologia-btn-container",
                            style = "display:flex;justify-content:flex-end;align-items:center;gap:8px;margin:4px 8px 12px 0;",
                            actionButton(ns("info_grafica1"), label = "Metodología",
                                         icon = icon("info-circle"), class = "btn-sm btn-outline-info metodologia-btn",
                                         style = "font-size:11px;padding:3px 10px;border-radius:12px;cursor:pointer;",
                                         title = "Ver metodología de proyección")
                        )
        )),
        fluidRow(column(12,
                        div(class = "plot-container",
                            shinycssloaders::withSpinner(
                              plotlyOutput(ns("grafico_evolucion_anual"), width = "100%", height = "100%"),
                              type = 6, color = "#44559B", size = 0.8
                            )
                        )
        )),
        fluidRow(column(12,
                        div(class = "plot-container",
                            shinycssloaders::withSpinner(
                              plotlyOutput(ns("grafico_evolucion_anual_sexo"), width = "100%", height = "100%"),
                              type = 6, color = "#44559B", size = 0.8
                            )
                        )
        ))
      ))
    }
    
    # ── Gráficas 4, 5 (año consultado != actual) ──────────────────────────────
    if (mostrar_consultadas) {
      message("✅ [UI RENDER] Mostrando gráficas históricas 4, 5")
      return(tagList(
        fluidRow(column(12,
                        div(class = "plot-container",
                            shinycssloaders::withSpinner(
                              plotlyOutput(ns("grafico_evolucion_year"), width = "100%", height = "100%"),
                              type = 6, color = "#44559B", size = 0.8
                            )
                        )
        )),
        fluidRow(column(12,
                        div(class = "plot-container",
                            shinycssloaders::withSpinner(
                              plotlyOutput(ns("grafico_evolucion_year_sexo"), width = "100%", height = "100%"),
                              type = 6, color = "#44559B", size = 0.8
                            )
                        )
        ))
      ))
    }
    
    message("⚠️ [UI RENDER] No se cumplen condiciones de renderizado")
    return(div(
      style = "text-align:center;padding:40px;color:#999;",
      icon("hourglass-half", style = "font-size:48px;margin-bottom:20px;"),
      h4("Procesando consulta...", style = "color:#666;font-weight:normal;")
    ))
    
  }) %>%
    bindEvent(
      estado_app(),
      input$btn_consultar,
      input$tipo_corte,
      ambito_reactivo(),
      ignoreNULL = FALSE,
      ignoreInit = FALSE
    )
  
  message("✅ graficas_ui_render v2.5 inicializado")
  message("   ✅ Vista Semanal: título + 3 secciones con 2 gráficas + dataTables + descargas")
  message("   ✅ Vista Histórica: gráficas 1-5 intactas")
}
