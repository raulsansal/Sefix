# modules/lista_nominal_graficas/graficas_ui_render.R
# Renderizado dinámico de UI para gráficas históricas Y semanales
# Versión: 2.9 — Fase 3: ui_seccion_edad actualizada para E1–E4
#
# CAMBIOS vs v2.8:
#   - ui_seccion_edad(): agrega E3 (proyección por grupo etario) entre E2 y E4
#     E3 incluye widget semanal_e3_grupos_ui + plotly semanal_e3_proyeccion_grupos
#     E4 (era E3): semanal_e4_barras (renombrado desde semanal_e3_barras)
#   - ui_seccion_sexo(): sin cambios
#   - ui_seccion_origen(): sin cambios
#   - Vista histórica: sin cambios

graficas_ui_render <- function(input, output, session, estado_app,
                               mostrar_graficas_anuales,
                               mostrar_graficas_consultadas,
                               ambito_reactivo) {
  
  message("📊 Inicializando graficas_ui_render v2.9")
  ns <- session$ns
  
  # ════════════════════════════════════════════════════════════════════════════
  # BLOQUES UI — VISTA SEMANAL
  # ════════════════════════════════════════════════════════════════════════════
  
  # ── Sección EDAD: E1 (proyección rangos + widget), E2 (grupos barras),
  #                  E3 (proyección grupos + widget), E4 (barras rangos) ────────
  ui_seccion_edad <- function(ns) {
    tagList(
      
      uiOutput(ns("semanal_subtitulo_edad")),
      
      # E1: Proyección por rango de edad (12 rangos) + widget selector
      div(
        class = "well well-sm",
        style = "background:#fff;border:1px solid #e0e0e0;border-radius:6px;padding:14px;margin-bottom:18px;",
        uiOutput(ns("semanal_e1_rangos_ui")),
        withSpinner(
          plotlyOutput(ns("semanal_e1_proyeccion"), height = "420px"),
          type = 4, color = "#003E66", size = 0.8
        )
      ),
      
      # E2: LNE por grupos etarios (barras horizontales)
      div(
        class = "well well-sm",
        style = "background:#fff;border:1px solid #e0e0e0;border-radius:6px;padding:14px;margin-bottom:18px;",
        withSpinner(
          plotlyOutput(ns("semanal_e2_grupos"), height = "320px"),
          type = 4, color = "#003E66", size = 0.8
        )
      ),
      
      # E3: Proyección por grupo etario (Jóvenes / Adultos / Mayores) + widget
      div(
        class = "well well-sm",
        style = "background:#fff;border:1px solid #e0e0e0;border-radius:6px;padding:14px;margin-bottom:18px;",
        uiOutput(ns("semanal_e3_grupos_ui")),
        withSpinner(
          plotlyOutput(ns("semanal_e3_proyeccion_grupos"), height = "420px"),
          type = 4, color = "#003E66", size = 0.8
        )
      ),
      
      # E4: Padrón y LNE por rango individual (barras agrupadas)
      div(
        class = "well well-sm",
        style = "background:#fff;border:1px solid #e0e0e0;border-radius:6px;padding:14px;margin-bottom:18px;",
        withSpinner(
          plotlyOutput(ns("semanal_e4_barras"), height = "380px"),
          type = 4, color = "#003E66", size = 0.8
        )
      ),
      
      # DataTable + descarga
      div(
        style = "margin-top:10px;",
        div(
          style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:6px;",
          tags$h5(
            style = "margin:0;font-size:14px;color:#2c3e50;font-weight:600;",
            icon("table"), " Detalle por Rango de Edad"
          ),
          downloadButton(
            ns("semanal_dt_edad_descarga"),
            label = "Descargar CSV",
            icon  = icon("download"),
            class = "btn btn-xs btn-outline-secondary",
            style = "font-size:11px;"
          )
        ),
        DT::dataTableOutput(ns("semanal_dt_edad"))
      )
    )
  }
  
  # ── Sección SEXO: S1–S7 ──────────────────────────────────────────────────────
  ui_seccion_sexo <- function(ns) {
    tagList(
      
      uiOutput(ns("semanal_subtitulo_sexo")),
      
      # S1: Pirámide por 12 rangos individuales (H vs M)
      div(
        class = "well well-sm",
        style = "background:#fff;border:1px solid #e0e0e0;border-radius:6px;padding:14px;margin-bottom:18px;",
        withSpinner(
          plotlyOutput(ns("semanal_s1_piramide"), height = "480px"),
          type = 4, color = "#44559B", size = 0.8
        )
      ),
      
      # S2 / S3 / S4: LNE por grupo etario × sexo (3 columnas)
      div(
        style = "margin-bottom:18px;",
        tags$h6(
          style = "font-size:13px;font-weight:600;color:#555;margin-bottom:8px;text-align:center;",
          "Lista Nominal Electoral por Grupo Etario y Sexo"
        ),
        fluidRow(
          column(4,
                 div(
                   class = "well well-sm",
                   style = "background:#fff;border:1px solid #e0e0e0;border-radius:6px;padding:10px;",
                   withSpinner(
                     plotlyOutput(ns("semanal_s2_mujeres"), height = "280px"),
                     type = 4, color = "#C0311A", size = 0.6
                   )
                 )
          ),
          column(4,
                 div(
                   class = "well well-sm",
                   style = "background:#fff;border:1px solid #e0e0e0;border-radius:6px;padding:10px;",
                   withSpinner(
                     plotlyOutput(ns("semanal_s3_hombres"), height = "280px"),
                     type = 4, color = "#44559B", size = 0.6
                   )
                 )
          ),
          column(4,
                 div(
                   class = "well well-sm",
                   style = "background:#fff;border:1px solid #e0e0e0;border-radius:6px;padding:10px;",
                   withSpinner(
                     plotlyOutput(ns("semanal_s4_nobinario"), height = "280px"),
                     type = 4, color = "#9B59B6", size = 0.6
                   )
                 )
          )
        )
      ),
      
      # S5: Barras agrupadas Padrón/LNE por sexo
      div(
        class = "well well-sm",
        style = "background:#fff;border:1px solid #e0e0e0;border-radius:6px;padding:14px;margin-bottom:18px;",
        withSpinner(
          plotlyOutput(ns("semanal_s5_barras"), height = "380px"),
          type = 4, color = "#44559B", size = 0.8
        )
      ),
      
      # S6 + S7: Dona y proyección (columnas 5/7)
      fluidRow(
        column(5,
               div(
                 class = "well well-sm",
                 style = "background:#fff;border:1px solid #e0e0e0;border-radius:6px;padding:14px;margin-bottom:18px;",
                 withSpinner(
                   plotlyOutput(ns("semanal_s6_dona"), height = "380px"),
                   type = 4, color = "#44559B", size = 0.8
                 )
               )
        ),
        column(7,
               div(
                 class = "well well-sm",
                 style = "background:#fff;border:1px solid #e0e0e0;border-radius:6px;padding:14px;margin-bottom:18px;",
                 withSpinner(
                   plotlyOutput(ns("semanal_s7_proyeccion"), height = "380px"),
                   type = 4, color = "#44559B", size = 0.8
                 )
               )
        )
      ),
      
      # DataTable + descarga
      div(
        style = "margin-top:10px;",
        div(
          style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:6px;",
          tags$h5(
            style = "margin:0;font-size:14px;color:#2c3e50;font-weight:600;",
            icon("table"), " Detalle por Sexo"
          ),
          downloadButton(
            ns("semanal_dt_sexo_descarga"),
            label = "Descargar CSV",
            icon  = icon("download"),
            class = "btn btn-xs btn-outline-secondary",
            style = "font-size:11px;"
          )
        ),
        DT::dataTableOutput(ns("semanal_dt_sexo"))
      )
    )
  }
  
  # ── Sección ORIGEN: O1 (mapa de calor) + O2 (proyección) ────────────────────
  ui_seccion_origen <- function(ns) {
    tagList(
      
      uiOutput(ns("semanal_subtitulo_origen")),
      
      # O1: Mapa de calor — widget top N inline
      div(
        class = "well well-sm",
        style = "background:#fff;border:1px solid #e0e0e0;border-radius:6px;padding:14px;margin-bottom:18px;",
        uiOutput(ns("semanal_o1_topn_ui")),
        withSpinner(
          plotlyOutput(ns("semanal_o1_calor"), height = "500px"),
          type = 4, color = "#44559B", size = 0.8
        )
      ),
      
      # O2: Proyección por entidad de origen — widget top N + checks 87/88 inline
      div(
        class = "well well-sm",
        style = "background:#fff;border:1px solid #e0e0e0;border-radius:6px;padding:14px;margin-bottom:18px;",
        uiOutput(ns("semanal_o2_controles_ui")),
        withSpinner(
          plotlyOutput(ns("semanal_o2_proyeccion"), height = "460px"),
          type = 4, color = "#44559B", size = 0.8
        )
      ),
      
      # DataTable + descarga
      div(
        style = "margin-top:10px;",
        div(
          style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:6px;",
          tags$h5(
            style = "margin:0;font-size:14px;color:#2c3e50;font-weight:600;",
            icon("table"), " Detalle por Entidad de Origen"
          ),
          downloadButton(
            ns("semanal_dt_origen_descarga"),
            label = "Descargar CSV",
            icon  = icon("download"),
            class = "btn btn-xs btn-outline-secondary",
            style = "font-size:11px;"
          )
        ),
        DT::dataTableOutput(ns("semanal_dt_origen"))
      )
    )
  }
  
  # ════════════════════════════════════════════════════════════════════════════
  # RENDER PRINCIPAL
  # ════════════════════════════════════════════════════════════════════════════
  
  output$graficas_dinamicas <- renderUI({
    
    estado_actual     <- estado_app()
    tipo_corte_actual <- input$tipo_corte %||% "historico"
    desglose_actual   <- input$desglose   %||% "edad"
    
    message("🔄 [UI RENDER v2.7] Estado: ", estado_actual,
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
      
      bloque_desglose <- switch(desglose_actual,
                                "edad"   = ui_seccion_edad(ns),
                                "sexo"   = ui_seccion_sexo(ns),
                                "origen" = ui_seccion_origen(ns),
                                ui_seccion_edad(ns)   # fallback
      )
      
      return(tagList(
        uiOutput(ns("semanal_titulo_principal")),
        bloque_desglose
      ))
    }
    
    # ══════════════════════════════════════════════════════════════════════════
    # VISTA HISTÓRICA — sin cambios vs v2.6
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
                        div(
                          class = "metodologia-btn-container",
                          style = "display:flex;justify-content:flex-end;align-items:center;gap:8px;margin:4px 8px 12px 0;",
                          actionButton(ns("info_grafica1"), label = "Metodología",
                                       icon  = icon("info-circle"),
                                       class = "btn-sm btn-outline-info metodologia-btn",
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
      input$desglose,
      ambito_reactivo(),
      ignoreNULL = FALSE,
      ignoreInit = FALSE
    )
  
  message("✅ graficas_ui_render v2.9 inicializado")
  message("   ✅ Fase 3: E1–E4 (edad), S1–S7 (sexo), O1–O2 (origen) en UI")
  message("   ✅ Vista histórica sin cambios")
}