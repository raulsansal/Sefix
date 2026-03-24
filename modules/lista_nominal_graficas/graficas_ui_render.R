# modules/lista_nominal_graficas/graficas_ui_render.R
# Renderizado dinámico de UI para gráficas históricas Y semanales
# Versión: 2.15 — ui_seccion_origen: O1 ajustado, O2 nuevo, O3 (era O2)
#
# CAMBIOS vs v2.14:
#   ui_seccion_origen(): reestructurada completamente:
#     - O1: widget top N (default Todos) + plotly altura dinámica + leyenda LN87/LN88
#     - O2: nueva gráfica calor Padrón vs LNE (absoluto lado a lado | diferencial)
#     - O3: proyección semanal por entidad de origen (era O2 v1.0)
#     - DataTable: homologado con patrón Edad/Sexo (h3 centrado, btn-primary)

graficas_ui_render <- function(input, output, session, estado_app,
                               mostrar_graficas_anuales,
                               mostrar_graficas_consultadas,
                               ambito_reactivo) {
  
  message("📊 Inicializando graficas_ui_render v2.14")
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
        div(
          style = "position:relative;z-index:10;",
          uiOutput(ns("semanal_e1_rangos_ui"))
        ),
        withSpinner(
          plotlyOutput(ns("semanal_e1_proyeccion"), height = "420px"),
          type = 4, color = "#003E66", size = 0.8
        ),
        div(
          style = "text-align:center;font-size:10px;color:#666666;font-family:Arial,sans-serif;padding:4px 0 2px 0;",
          "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado"
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
        div(
          style = "position:relative;z-index:10;",
          uiOutput(ns("semanal_e3_grupos_ui"))
        ),
        withSpinner(
          plotlyOutput(ns("semanal_e3_proyeccion_grupos"), height = "420px"),
          type = 4, color = "#003E66", size = 0.8
        ),
        div(
          style = "text-align:center;font-size:10px;color:#666666;font-family:Arial,sans-serif;padding:4px 0 2px 0;",
          "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado"
        )
      ),
      
      # E4: Padrón y LNE por rango individual (barras agrupadas)
      div(
        class = "well well-sm",
        style = "background:#fff;border:1px solid #e0e0e0;border-radius:6px;padding:14px;margin-bottom:18px;",
        withSpinner(
          plotlyOutput(ns("semanal_e4_barras"), height = "380px"),
          type = 4, color = "#003E66", size = 0.8
        ),
        div(
          style = "text-align:center;font-size:10px;color:#666666;font-family:Arial,sans-serif;padding:4px 0 2px 0;",
          "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado"
        )
      ),
      
      # DataTable: misma estructura que la tabla histórica (título centrado + header + descarga + DT)
      div(
        class = "datatable-section",
        style = "margin-top:30px;",
        # Título centrado — igual que en histórico
        h3("Tabla de Datos",
           align = "center",
           style = "margin-top:0;margin-bottom:15px;",
           class = "datatable-title"),
        # Header: ámbito + alcance (renderizado en graficas_semanal.R)
        div(class = "datatable-header",
            uiOutput(ns("semanal_dt_edad_header"))),
        # Fila: etiqueta vacía a la izquierda + botón descarga azul a la derecha
        div(
          style = "display:flex;justify-content:flex-end;align-items:center;margin:10px 0 6px 0;",
          downloadButton(
            ns("semanal_dt_edad_descarga"),
            label = "Descargar CSV",
            icon  = icon("download"),
            class = "btn btn-primary btn-sm",
            style = "font-size:12px;padding:5px 12px;"
          )
        ),
        shinycssloaders::withSpinner(
          DT::dataTableOutput(ns("semanal_dt_edad")),
          type  = 6,
          color = "#44559B",
          size  = 0.8
        )
      )
    )
  }
  
  # ── Sección SEXO: S1–S7 ──────────────────────────────────────────────────────
  ui_seccion_sexo <- function(ns) {
    tagList(
      
      # S1: Pirámide por 12 rangos individuales (H vs M)
      div(
        class = "well well-sm",
        style = "background:#fff;border:1px solid #e0e0e0;border-radius:6px;padding:14px;margin-bottom:18px;",
        withSpinner(
          plotlyOutput(ns("semanal_s1_piramide"), height = "480px"),
          type = 4, color = "#44559B", size = 0.8
        ),
        div(
          style = "display:flex;justify-content:flex-end;align-items:center;margin:8px 0 4px 0;",
          downloadButton(
            ns("semanal_s1_descarga"),
            label = "Descargar CSV Pir\u00e1mide",
            icon  = icon("download"),
            class = "btn btn-primary btn-sm",
            style = "font-size:12px;padding:5px 12px;"
          )
        ),
        div(
          style = "text-align:center;font-size:10px;color:#666666;font-family:Arial,sans-serif;padding:4px 0 2px 0;",
          "Fuente: INE. Estad\u00edstica de Padr\u00f3n Electoral y Lista Nominal del Electorado"
        )
      ),

      # S2: barras horizontales agrupadas LNE × Grupo Etario × Sexo — ancho completo
      div(
        class = "well well-sm",
        style = "background:#fff;border:1px solid #e0e0e0;border-radius:6px;padding:14px;margin-bottom:18px;",
        div(
          style = "position:relative;z-index:10;",
          uiOutput(ns("semanal_s2_widget_ui"))
        ),
        withSpinner(
          plotlyOutput(ns("semanal_s2_barras"), height = "420px"),
          type = 4, color = "#44559B", size = 0.8
        ),
        div(
          style = "text-align:center;font-size:10px;color:#666666;font-family:Arial,sans-serif;padding:4px 0 2px 0;",
          "Fuente: INE. Estad\u00edstica de Padr\u00f3n Electoral y Lista Nominal del Electorado"
        )
      ),
      
      # S3: Evolución y Proyección Semanal por Sexo — ancho completo + widget
      div(
        class = "well well-sm",
        style = "background:#fff;border:1px solid #e0e0e0;border-radius:6px;padding:14px;margin-bottom:18px;",
        div(
          style = "position:relative;z-index:10;",
          uiOutput(ns("semanal_s3_widget_ui"))
        ),
        withSpinner(
          plotlyOutput(ns("semanal_s3_proyeccion"), height = "420px"),
          type = 4, color = "#44559B", size = 0.8
        ),
        div(
          style = "text-align:center;font-size:10px;color:#666666;font-family:Arial,sans-serif;padding:4px 0 2px 0;",
          "Fuente: INE. Estad\u00edstica de Padr\u00f3n Electoral y Lista Nominal del Electorado"
        )
      ),

      # S4: Padrón y LNE por Sexo — ancho completo
      div(
        class = "well well-sm",
        style = "background:#fff;border:1px solid #e0e0e0;border-radius:6px;padding:14px;margin-bottom:18px;",
        withSpinner(
          plotlyOutput(ns("semanal_s4_barras"), height = "380px"),
          type = 4, color = "#44559B", size = 0.8
        ),
        div(
          style = "text-align:center;font-size:10px;color:#666666;font-family:Arial,sans-serif;padding:4px 0 2px 0;",
          "Fuente: INE. Estad\u00edstica de Padr\u00f3n Electoral y Lista Nominal del Electorado"
        )
      ),
      
      # DataTable: misma estructura que la tabla histórica (título centrado + header + descarga + DT)
      div(
        class = "datatable-section",
        style = "margin-top:30px;",
        h3("Tabla de Datos",
           align = "center",
           style = "margin-top:0;margin-bottom:15px;",
           class = "datatable-title"),
        div(class = "datatable-header",
            uiOutput(ns("semanal_dt_sexo_header"))),
        div(
          style = "display:flex;justify-content:flex-end;align-items:center;margin:10px 0 6px 0;",
          downloadButton(
            ns("semanal_dt_sexo_descarga"),
            label = "Descargar CSV",
            icon  = icon("download"),
            class = "btn btn-primary btn-sm",
            style = "font-size:12px;padding:5px 12px;"
          )
        ),
        shinycssloaders::withSpinner(
          DT::dataTableOutput(ns("semanal_dt_sexo")),
          type  = 6,
          color = "#44559B",
          size  = 0.8
        )
      )
    )
  }
  
  # ── Sección ORIGEN: O1, O2 (nuevo), O3 (antes O2) ───────────────────────────
  ui_seccion_origen <- function(ns) {
    tagList(

      # O1: Mapa de calor LNE — widget top N + gráfica dinámica + leyenda LN87/88
      div(
        class = "well well-sm",
        style = "background:#fff;border:1px solid #e0e0e0;border-radius:6px;padding:14px;margin-bottom:18px;",
        div(
          style = "position:relative;z-index:10;",
          uiOutput(ns("semanal_o1_widget_ui"))
        ),
        uiOutput(ns("semanal_o1_grafica_ui")),
        div(
          style = "text-align:center;font-size:10px;color:#666666;font-family:Arial,sans-serif;padding:6px 0 2px 0;line-height:1.9;",
          tags$div(tags$b("LN87:"), " Lista Nominal Electoral de ciudadanos mexicanos nacidos en el extranjero, residentes en la entidad"),
          tags$div(tags$b("LN88:"), " Lista Nominal de ciudadanos naturalizados mexicanos, residentes en la entidad"),
          tags$div(style = "margin-top:4px;",
            "Fuente: INE. Estad\u00edstica de Padr\u00f3n Electoral y Lista Nominal del Electorado"
          )
        )
      ),

      # O2: Comparación Padrón vs LNE — widget top N + radio + gráfica dinámica
      div(
        class = "well well-sm",
        style = "background:#fff;border:1px solid #e0e0e0;border-radius:6px;padding:14px;margin-bottom:18px;",
        div(
          style = "position:relative;z-index:10;",
          uiOutput(ns("semanal_o2_widget_ui"))
        ),
        uiOutput(ns("semanal_o2_grafica_ui")),
        div(
          style = "text-align:center;font-size:10px;color:#666666;font-family:Arial,sans-serif;padding:6px 0 2px 0;line-height:1.9;",
          tags$div(tags$b("PAD87:"), " Padr\u00f3n Electoral de ciudadanos mexicanos nacidos en el extranjero, residentes en la entidad"),
          tags$div(tags$b("PAD88:"), " Padr\u00f3n Electoral de ciudadanos naturalizados mexicanos, residentes en la entidad"),
          tags$div(style = "margin-top:4px;",
            "Fuente: INE. Estad\u00edstica de Padr\u00f3n Electoral y Lista Nominal del Electorado"
          )
        )
      ),

      # O3: Evolución semanal LNE/Padrón por origen × receptor
      div(
        class = "well well-sm",
        style = "background:#fff;border:1px solid #e0e0e0;border-radius:6px;padding:14px;margin-bottom:18px;",
        div(
          style = "position:relative;z-index:10;",
          uiOutput(ns("semanal_o3_controles_ui"))
        ),
        uiOutput(ns("semanal_o3_grafica_ui")),
        uiOutput(ns("semanal_o3_leyenda_ui"))
      ),

      # DataTable: estructura homologada con Edad / Sexo
      div(
        class = "datatable-section",
        style = "margin-top:30px;",
        h3("Tabla de Datos",
           align = "center",
           style = "margin-top:0;margin-bottom:15px;",
           class = "datatable-title"),
        div(class = "datatable-header",
            uiOutput(ns("semanal_dt_origen_header"))),
        div(
          style = "display:flex;justify-content:flex-end;align-items:center;margin:10px 0 6px 0;",
          downloadButton(
            ns("semanal_dt_origen_descarga"),
            label = "Descargar CSV",
            icon  = icon("download"),
            class = "btn btn-primary btn-sm",
            style = "font-size:12px;padding:5px 12px;"
          )
        ),
        shinycssloaders::withSpinner(
          DT::dataTableOutput(ns("semanal_dt_origen")),
          type  = 6,
          color = "#44559B",
          size  = 0.8
        )
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
      
      message("✅ [UI RENDER] Semanal — todas las secciones (conditionalPanel)")

      # Las tres secciones siempre están en el DOM; conditionalPanel las muestra/oculta
      # via CSS en el cliente — sin rerender del servidor al cambiar desglose.
      return(tagList(
        uiOutput(ns("semanal_titulo_principal")),
        conditionalPanel(
          condition = sprintf("input['%s'] === 'edad'",   ns("desglose")),
          ui_seccion_edad(ns)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] === 'sexo'",   ns("desglose")),
          ui_seccion_sexo(ns)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] === 'origen'", ns("desglose")),
          ui_seccion_origen(ns)
        )
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
      ambito_reactivo(),
      ignoreNULL = FALSE,
      ignoreInit = FALSE
    )
  
  message("✅ graficas_ui_render v2.15 inicializado")
  message("   ✅ ui_seccion_edad: tabla con header ámbito+alcance, descarga azul")
  message("   ✅ ui_seccion_sexo: S1-S4 + DataTable homologado con Edad")
  message("   ✅ ui_seccion_origen: O1 (dinámico+LN87/88), O2 (absoluto|dif.), O3")
  message("   ✅ Vista histórica sin cambios")
}