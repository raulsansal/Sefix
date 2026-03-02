# modules/lista_nominal_graficas/graficas_ui_render.R
# Renderizado dinámico de UI para gráficas históricas Y semanales
# Versión: 2.6
#
# CAMBIOS vs v2.5:
#   - Fase 2: Renderizado condicional por desglose en vista semanal
#     - desglose == "edad"   → solo sección Rango de Edad   + DataTable + análisis
#     - desglose == "sexo"   → solo sección Distribución por Sexo + DataTable + análisis
#     - desglose == "origen" → solo sección Entidad de Origen + DataTable + análisis
#   - input$desglose agregado al bindEvent para que el UI reaccione al cambio
#   - Vista histórica intacta (sin cambios)

graficas_ui_render <- function(input, output, session, estado_app,
                               mostrar_graficas_anuales,
                               mostrar_graficas_consultadas,
                               ambito_reactivo) {
  
  message("📊 Inicializando graficas_ui_render v2.6")
  ns <- session$ns
  
  # ── Bloque UI: Sección Rango de Edad ─────────────────────────────────────────
  ui_seccion_edad <- function(ns) {
    tagList(
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
      div(style = "margin-top:24px;",
          h5("Tabla de datos — Rango de Edad",
             style = "text-align:center;font-weight:700;color:#2c3e50;margin-bottom:2px;"),
          uiOutput(ns("semanal_subtitulo_edad")),
          div(style = "margin-top:8px;", DT::dataTableOutput(ns("semanal_dt_edad"))),
          div(style = "text-align:right;margin-top:8px;",
              downloadButton(ns("semanal_dt_edad_descarga"),
                             label = "Descargar tabla de Edad",
                             class = "btn btn-sm btn-outline-secondary")
          )
      )
    )
  }
  
  # ── Bloque UI: Sección Distribución por Sexo ─────────────────────────────────
  ui_seccion_sexo <- function(ns) {
    tagList(
      hr(style = "border-top:2px solid #dee2e6;margin:20px 0 12px 0;"),
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
      div(style = "margin-top:24px;",
          h5("Tabla de datos — Distribución por Sexo",
             style = "text-align:center;font-weight:700;color:#2c3e50;margin-bottom:2px;"),
          uiOutput(ns("semanal_subtitulo_sexo")),
          div(style = "margin-top:8px;", DT::dataTableOutput(ns("semanal_dt_sexo"))),
          div(style = "text-align:right;margin-top:8px;",
              downloadButton(ns("semanal_dt_sexo_descarga"),
                             label = "Descargar tabla de Sexo",
                             class = "btn btn-sm btn-outline-secondary")
          )
      )
    )
  }
  
  # ── Bloque UI: Sección Entidad de Origen ─────────────────────────────────────
  ui_seccion_origen <- function(ns) {
    tagList(
      hr(style = "border-top:2px solid #dee2e6;margin:20px 0 12px 0;"),
      h4("Entidad de Origen",
         style = "font-weight:700;color:#2c3e50;font-family:Arial,sans-serif;margin-bottom:4px;"),
      uiOutput(ns("semanal_subtitulo_origen")),
      div(
        style = "display:flex;align-items:center;gap:12px;margin-bottom:10px;",
        span("Top estados de origen (gráfica derecha):",
             style = "font-size:13px;color:#555;white-space:nowrap;"),
        selectInput(
          ns("semanal_top_n"),
          label   = NULL,
          choices = c("5" = "5", "10" = "10", "15" = "15", "Todos" = "0"),
          selected = "5",
          width   = "100px"
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
      div(style = "margin-top:24px;",
          h5("Tabla de datos — Entidad de Origen",
             style = "text-align:center;font-weight:700;color:#2c3e50;margin-bottom:2px;"),
          uiOutput(ns("semanal_subtitulo_origen")),
          div(style = "margin-top:8px;", DT::dataTableOutput(ns("semanal_dt_origen"))),
          div(style = "text-align:right;margin-top:8px;",
              downloadButton(ns("semanal_dt_origen_descarga"),
                             label = "Descargar tabla de Origen",
                             class = "btn btn-sm btn-outline-secondary")
          )
      )
    )
  }
  
  # ════════════════════════════════════════════════════════════════════════════
  # RENDER PRINCIPAL
  # ════════════════════════════════════════════════════════════════════════════
  
  output$graficas_dinamicas <- renderUI({
    
    estado_actual      <- estado_app()
    tipo_corte_actual  <- input$tipo_corte %||% "historico"
    desglose_actual    <- input$desglose   %||% "edad"
    
    message("🔄 [UI RENDER v2.6] Estado: ", estado_actual,
            " | tipo_corte: ", tipo_corte_actual,
            " | desglose: ", desglose_actual)
    
    # ══════════════════════════════════════════════════════════════════════════
    # VISTA SEMANAL
    # ══════════════════════════════════════════════════════════════════════════
    
    if (tipo_corte_actual == "semanal") {
      
      if (estado_actual == "inicial") {
        return(div(
          style = "text-align:center;padding:40px;color:#999;",
          icon("chart-line", style = "font-size:48px;margin-bottom:20px;"),
          h4("Configure su consulta y presione 'Consultar' para visualizar las gráficas semanales",
             style = "color:#666;font-weight:normal;")
        ))
      }
      
      message("✅ [UI RENDER] Semanal — desglose: ", desglose_actual)
      
      # Bloque de sección según desglose activo
      bloque_desglose <- switch(desglose_actual,
                                "edad"   = ui_seccion_edad(ns),
                                "sexo"   = ui_seccion_sexo(ns),
                                "origen" = ui_seccion_origen(ns),
                                ui_seccion_edad(ns)   # fallback
      )
      
      return(tagList(
        # Título principal dinámico (siempre visible)
        uiOutput(ns("semanal_titulo_principal")),
        # Solo el bloque del desglose seleccionado
        bloque_desglose
      ))
    }
    
    # ══════════════════════════════════════════════════════════════════════════
    # VISTA HISTÓRICA — intacta
    # ══════════════════════════════════════════════════════════════════════════
    
    if (estado_actual == "inicial") {
      message("⏸️ [UI RENDER] Estado inicial — sin gráficas")
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
    
    mostrar_anuales     <- isolate(mostrar_graficas_anuales())
    mostrar_consultadas <- isolate(mostrar_graficas_consultadas())
    
    # Gráficas 1, 2, 3 (año actual)
    if (mostrar_anuales) {
      message("✅ [UI RENDER] Histórico — gráficas 1, 2, 3")
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
    
    # Gráficas 4, 5 (año consultado != actual)
    if (mostrar_consultadas) {
      message("✅ [UI RENDER] Histórico — gráficas 4, 5")
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
    
    message("⚠️ [UI RENDER] Sin condiciones de renderizado")
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
      input$desglose,          # ← NUEVO: re-renderiza al cambiar desglose
      ambito_reactivo(),
      ignoreNULL = FALSE,
      ignoreInit = FALSE
    )
  
  message("✅ graficas_ui_render v2.6 inicializado")
  message("   ✅ Fase 2: renderizado condicional por desglose (edad / sexo / origen)")
  message("   ✅ Vista histórica intacta")
}
