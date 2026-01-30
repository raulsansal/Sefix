# modules/lista_nominal_graficas/graficas_ui_render.R
# Renderizado dinámico de UI para gráficas históricas
# Versión: 2.3 - CORRECCIÓN: Eliminar req() que bloqueaba renderizado

graficas_ui_render <- function(input, output, session, estado_app, mostrar_graficas_anuales, mostrar_graficas_consultadas, ambito_reactivo) {
  
  message("📊 Inicializando graficas_ui_render v2.3")
  
  # ========== RENDERIZADO DINÁMICO DE GRÁFICAS ==========
  
  output$graficas_dinamicas <- renderUI({
    
    # ========== OBTENER ESTADO Y VALIDAR ==========
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
    
    # ========== ✅ CORRECCIÓN v2.3: VALIDAR SIN BLOQUEAR CON req() ==========
    if (estado_actual == "consultado") {
      # ✅ Validar sin bloquear con req() - esto permite que el reactive se complete
      if (is.null(input$btn_consultar) || input$btn_consultar == 0) {
        message("⚠️ [UI RENDER] Estado consultado pero botón no presionado - Esperando...")
        return(div(
          style = "text-align: center; padding: 40px; color: #999;",
          icon("hourglass-half", style = "font-size: 48px; margin-bottom: 20px;"),
          h4("Procesando consulta...", style = "color: #666; font-weight: normal;")
        ))
      }
      message("🔍 [UI RENDER] Renderizando en estado CONSULTADO - Botón: ", input$btn_consultar)
    } else {
      message("🔍 [UI RENDER] Renderizando en estado RESTABLECIDO")
    }
    
    # ========== ✅ AISLAR INPUTS Y REACTIVES PARA EVITAR REACTIVIDAD ==========
    btn_count <- isolate(input$btn_consultar)
    
    # ✅ v2.2: Usar ambito_reactivo en lugar de isolate(input$ambito_datos)
    ambito_actual <- ambito_reactivo()
    
    # ✅ AISLAR las llamadas a los reactives que controlan visibilidad
    mostrar_anuales <- isolate(mostrar_graficas_anuales())
    mostrar_consultadas <- isolate(mostrar_graficas_consultadas())
    
    message("   Estado: ", estado_actual, ", Botón: ", btn_count, ", Ámbito: ", ambito_actual)
    message("   Mostrar anuales: ", mostrar_anuales, " | Mostrar consultadas: ", mostrar_consultadas)
    
    # ========== GRÁFICAS 1, 2, 3 (AÑO ACTUAL) ==========
    if (mostrar_anuales) {
      message("✅ [UI RENDER] Mostrando gráficas 1, 2, 3 (año actual)")
      return(tagList(
        # Gráfica 1: Evolución mensual año actual + Proyección
        fluidRow(
          column(12, 
                 div(class = "plot-container",
                     style = "height: 450px; margin-bottom: 10px;",
                     shinycssloaders::withSpinner(
                       plotlyOutput(session$ns("grafico_evolucion_2025"), width = "100%", height = "450px"),
                       type = 6,
                       color = "#44559B",
                       size = 1
                     )
                 ),
                 div(
                   style = "display: flex; justify-content: center; align-items: center; gap: 10px; margin-bottom: 20px;",
                   tags$span(
                     style = "color: #666; font-size: 12px;",
                     "Información adicional:"
                   ),
                   actionButton(
                     session$ns("info_grafica1"),
                     label = "Metodología de Proyección",
                     icon = icon("info-circle"),
                     class = "btn-sm btn-outline-info",
                     style = "font-size: 12px; padding: 4px 12px; border-radius: 15px; cursor: pointer;",
                     title = "Ver metodología de proyección"
                   )
                 )
          )
        ),
        
        # Gráfica 2: Evolución anual
        fluidRow(
          column(12, 
                 div(class = "plot-container",
                     style = "height: 450px; margin-bottom: 30px;",
                     shinycssloaders::withSpinner(
                       plotlyOutput(session$ns("grafico_evolucion_anual"), width = "100%", height = "450px"),
                       type = 6,
                       color = "#44559B",
                       size = 1
                     )
                 )
          )
        ),
        
        # Gráfica 3: Evolución anual + Desglose por sexo
        fluidRow(
          column(12, 
                 div(class = "plot-container",
                     style = "height: 450px; margin-bottom: 30px;",
                     shinycssloaders::withSpinner(
                       plotlyOutput(session$ns("grafico_evolucion_anual_sexo"), width = "100%", height = "450px"),
                       type = 6,
                       color = "#44559B",
                       size = 1
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
                     style = "height: 450px; margin-bottom: 30px;",
                     shinycssloaders::withSpinner(
                       plotlyOutput(session$ns("grafico_evolucion_year"), width = "100%", height = "450px"),
                       type = 6,
                       color = "#44559B",
                       size = 1
                     )
                 )
          )
        ),
        
        # Gráfica 5: Evolución mensual + sexo
        fluidRow(
          column(12, 
                 div(class = "plot-container",
                     style = "height: 450px; margin-bottom: 30px;",
                     shinycssloaders::withSpinner(
                       plotlyOutput(session$ns("grafico_evolucion_year_sexo"), width = "100%", height = "450px"),
                       type = 6,
                       color = "#44559B",
                       size = 1
                     )
                 )
          )
        )
      ))
    }
    
    # ========== NINGUNA CONDICIÓN CUMPLIDA ==========
    message("⚠️ [UI RENDER] No se cumplen condiciones - Retornando mensaje de espera")
    return(div(
      style = "text-align: center; padding: 40px; color: #999;",
      icon("hourglass-half", style = "font-size: 48px; margin-bottom: 20px;"),
      h4("Procesando consulta...", style = "color: #666; font-weight: normal;")
    ))
    
  }) %>%
    # ========== ✅ CORRECCIÓN v2.2: AGREGAR ambito_reactivo PARA CAMBIO DE VISTA ==========
  # Se ejecuta cuando:
  # 1. Cambia estado_app (inicial → restablecido → consultado)
  # 2. Se presiona btn_consultar
  # 3. Se cambia ambito_reactivo (nacional ↔ extranjero) DESPUÉS de consultar
  bindEvent(
    estado_app(),
    input$btn_consultar,
    ambito_reactivo(),  # ✅ v2.2: AGREGADO para cambio de vista automático
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )
  
  message("✅ graficas_ui_render v2.3 inicializado")
  message("   ✅ CORRECCIÓN v2.3: req() eliminado para no bloquear renderizado")
}
