# modules/lista_nominal_graficas/graficas_ui_render.R
# Renderizado dinámico de UI para gráficas históricas
# Versión: 2.4 - Alturas responsivas para móvil

graficas_ui_render <- function(input, output, session, estado_app, mostrar_graficas_anuales, mostrar_graficas_consultadas, ambito_reactivo) {
  
  message("📊 Inicializando graficas_ui_render v2.4")
  
  # ========== RENDERIZADO DINÁMICO DE GRÁFICAS ==========
  
  output$graficas_dinamicas <- renderUI({
    
    estado_actual <- estado_app()
    
    message("🔄 [UI RENDER] Estado actual: ", estado_actual)
    
    # ========== NO RENDERIZAR EN ESTADO INICIAL ==========
    if (estado_actual == "inicial") {
      message("⏸️ [UI RENDER] Estado inicial - NO renderizar gráficas")
      return(div(
        style = "text-align: center; padding: 40px; color: #999;",
        icon("chart-line", style = "font-size: 48px; margin-bottom: 20px;"),
        h4("Configure su consulta y presione 'Consultar' para visualizar gráficas", 
           style = "color: #666; font-weight: normal;")
      ))
    }
    
    # ========== VALIDAR SIN BLOQUEAR ==========
    if (estado_actual == "consultado") {
      if (is.null(input$btn_consultar) || input$btn_consultar == 0) {
        message("⚠️ [UI RENDER] Esperando...")
        return(div(
          style = "text-align: center; padding: 40px; color: #999;",
          icon("hourglass-half", style = "font-size: 48px; margin-bottom: 20px;"),
          h4("Procesando consulta...", style = "color: #666; font-weight: normal;")
        ))
      }
    }
    
    btn_count <- isolate(input$btn_consultar)
    ambito_actual <- ambito_reactivo()
    mostrar_anuales <- isolate(mostrar_graficas_anuales())
    mostrar_consultadas <- isolate(mostrar_graficas_consultadas())
    
    message("   Estado: ", estado_actual, ", Ámbito: ", ambito_actual)
    
    # ========== GRÁFICAS 1, 2, 3 (AÑO ACTUAL) ==========
    if (mostrar_anuales) {
      message("✅ [UI RENDER] Mostrando gráficas 1, 2, 3 (año actual)")
      return(tagList(
        # Gráfica 1: Evolución mensual año actual + Proyección
        fluidRow(
          column(12, 
                 # ✅ v2.4: Altura responsiva - sin altura fija en style
                 div(class = "plot-container",
                     shinycssloaders::withSpinner(
                       # ✅ v2.4: height = "100%" para que CSS controle
                       plotlyOutput(session$ns("grafico_evolucion_2025"), width = "100%", height = "100%"),
                       type = 6,
                       color = "#44559B",
                       size = 0.8
                     )
                 ),
                 # ✅ v2.4: Contenedor del botón metodología con clase específica
                 div(
                   class = "metodologia-btn-container",
                   style = "display: flex; justify-content: flex-end; align-items: center; gap: 8px; margin: 4px 8px 12px 0;",
                   tags$span(
                     class = "metodologia-label desktop-only",
                     style = "color: #666; font-size: 11px;",
                     "Info:"
                   ),
                   actionButton(
                     session$ns("info_grafica1"),
                     label = "Metodología",
                     icon = icon("info-circle"),
                     class = "btn-sm btn-outline-info metodologia-btn",
                     style = "font-size: 11px; padding: 3px 10px; border-radius: 12px; cursor: pointer;",
                     title = "Ver metodología de proyección"
                   )
                 )
          )
        ),
        
        # Gráfica 2: Evolución anual
        fluidRow(
          column(12, 
                 div(class = "plot-container",
                     shinycssloaders::withSpinner(
                       plotlyOutput(session$ns("grafico_evolucion_anual"), width = "100%", height = "100%"),
                       type = 6,
                       color = "#44559B",
                       size = 0.8
                     )
                 )
          )
        ),
        
        # Gráfica 3: Evolución anual + Desglose por sexo
        fluidRow(
          column(12, 
                 div(class = "plot-container",
                     shinycssloaders::withSpinner(
                       plotlyOutput(session$ns("grafico_evolucion_anual_sexo"), width = "100%", height = "100%"),
                       type = 6,
                       color = "#44559B",
                       size = 0.8
                     )
                 )
          )
        )
      ))
    }
    
    # ========== GRÁFICAS 4, 5 (AÑO CONSULTADO != ACTUAL) ==========
    if (mostrar_consultadas) {
      message("✅ [UI RENDER] Mostrando gráficas 4, 5 (año consultado)")
      return(tagList(
        # Gráfica 4: Evolución mensual del año seleccionado
        fluidRow(
          column(12, 
                 div(class = "plot-container",
                     shinycssloaders::withSpinner(
                       plotlyOutput(session$ns("grafico_evolucion_year"), width = "100%", height = "100%"),
                       type = 6,
                       color = "#44559B",
                       size = 0.8
                     )
                 )
          )
        ),
        
        # Gráfica 5: Evolución mensual + sexo
        fluidRow(
          column(12, 
                 div(class = "plot-container",
                     shinycssloaders::withSpinner(
                       plotlyOutput(session$ns("grafico_evolucion_year_sexo"), width = "100%", height = "100%"),
                       type = 6,
                       color = "#44559B",
                       size = 0.8
                     )
                 )
          )
        )
      ))
    }
    
    # ========== NINGUNA CONDICIÓN CUMPLIDA ==========
    message("⚠️ [UI RENDER] No se cumplen condiciones")
    return(div(
      style = "text-align: center; padding: 40px; color: #999;",
      icon("hourglass-half", style = "font-size: 48px; margin-bottom: 20px;"),
      h4("Procesando consulta...", style = "color: #666; font-weight: normal;")
    ))
    
  }) %>%
    bindEvent(
      estado_app(),
      input$btn_consultar,
      ambito_reactivo(),
      ignoreNULL = FALSE,
      ignoreInit = FALSE
    )
  
  message("✅ graficas_ui_render v2.4 inicializado")
  message("   ✅ Alturas responsivas (CSS controla)")
  message("   ✅ Botón metodología con clase específica")
}
