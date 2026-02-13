# modules/lista_nominal_graficas/graficas_historico_1_2.R
# Gráficas históricas 1 y 2: Proyección mensual y Evolución anual
# Versión: 2.14 - Posición Fuente restaurada para desktop (y=-0.35)
# Cambios v2.14:
#   - Gráfica 1 (4 trazas): Fuente en y = -0.35 (desktop) - JS ajusta a -0.60 en mobile
#   - Gráfica 2 (2 trazas): Fuente en y = -0.35 (desktop) - JS ajusta a -0.40 en mobile
#   - La lógica de ajuste móvil se maneja en custom.js

graficas_historico_1_2 <- function(input, output, session, datos_year_actual, datos_anuales_completos, 
                                   anio_actual, texto_alcance, estado_app, mostrar_graficas_anuales, ambito_reactivo) {
  
  message("📊 Inicializando graficas_historico_1_2 v2.14")
  
  # ========== GRÁFICA 1: EVOLUCIÓN MENSUAL AÑO ACTUAL + PROYECCIÓN ==========
  
  output$grafico_evolucion_2025 <- renderPlotly({
    req(input$tipo_corte == "historico")
    
    # ✅ v2.7: Usar ambito_reactivo en lugar de input$ambito_datos
    ambito_actual <- ambito_reactivo()
    
    # ========== NO RENDERIZAR EN ESTADO INICIAL ==========
    if (estado_app() == "inicial") {
      return(plot_ly() %>%
               layout(
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE),
                 annotations = list(
                   list(
                     text = "Presione 'Consultar' para visualizar datos",
                     xref = "paper", yref = "paper",
                     x = 0.5, y = 0.5,
                     showarrow = FALSE,
                     font = list(size = 16, color = "#999")
                   )
                 )
               ))
    }
    
    # ========== CONTROL DE RENDERIZADO ==========
    if (!mostrar_graficas_anuales()) {
      return(NULL)
    }
    
    datos_completos <- datos_year_actual()
    
    message("📊 [GRÁFICA 1] Renderizando - Estado: ", estado_app(), " | Ámbito: ", ambito_actual)
    
    # ========== SIN DATOS DISPONIBLES ==========
    if (is.null(datos_completos) || !is.data.frame(datos_completos) || nrow(datos_completos) == 0) {
      return(plot_ly() %>%
               layout(
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE),
                 annotations = list(
                   list(
                     text = "No hay datos disponibles para esa consulta",
                     xref = "paper", yref = "paper",
                     x = 0.5, y = 0.5,
                     showarrow = FALSE,
                     font = list(size = 14, color = "#666")
                   )
                 )
               ))
    }
    
    # Obtener año de los datos (del último registro disponible)
    year_datos <- format(max(datos_completos$fecha), "%Y")
    
    # Calcular meses restantes hasta diciembre
    ultimo_mes <- as.numeric(format(max(datos_completos$fecha), "%m"))
    meses_restantes <- 12 - ultimo_mes
    
    # ========== GRÁFICA NACIONAL ==========
    if (ambito_actual == "nacional") {
      
      # Proyectar usando columnas nacionales
      proyeccion <- NULL
      if (meses_restantes > 0) {
        datos_para_proyeccion <- datos_completos
        datos_para_proyeccion$lista_nominal <- datos_para_proyeccion$lista_nacional
        datos_para_proyeccion$padron_electoral <- datos_para_proyeccion$padron_nacional
        proyeccion <- proyectar_con_tasa_crecimiento(datos_para_proyeccion, meses_restantes)
      }
      
      # Crear gráfico
      p <- plot_ly()
      
      # 1. Padrón Nacional
      p <- p %>% add_trace(
        data = datos_completos,
        x = ~fecha,
        y = ~padron_nacional,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Padrón Nacional',
        line = list(color = '#003E66', width = 3),
        marker = list(size = 8, color = '#003E66'),
        hovertemplate = paste0(
          '<b>%{x|%B %Y}</b><br>',
          'Padrón Nacional: %{y:,.0f}<extra></extra>'
        )
      )
      
      # 2. Lista Nacional
      p <- p %>% add_trace(
        data = datos_completos,
        x = ~fecha,
        y = ~lista_nacional,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Lista Nacional',
        line = list(color = '#AE0E35', width = 3),
        marker = list(size = 8, color = '#AE0E35'),
        hovertemplate = paste0(
          '<b>%{x|%B %Y}</b><br>',
          'Lista Nacional: %{y:,.0f}<extra></extra>'
        )
      )
      
      # Proyecciones
      if (!is.null(proyeccion)) {
        # Proyección Padrón
        p <- p %>% add_trace(
          data = proyeccion,
          x = ~fecha,
          y = ~padron_proyectado,
          type = 'scatter',
          mode = 'lines',
          name = 'Proyección Padrón',
          line = list(color = '#6B8FB3', width = 2, dash = 'dash'),
          hovertemplate = paste0(
            '<b>%{x|%B %Y}</b><br>',
            'Proyección Padrón: %{y:,.0f}<extra></extra>'
          )
        )
        
        # Proyección Lista
        p <- p %>% add_trace(
          data = proyeccion,
          x = ~fecha,
          y = ~lista_proyectada,
          type = 'scatter',
          mode = 'lines',
          name = 'Proyección Lista',
          line = list(color = '#D66B7D', width = 2, dash = 'dash'),
          hovertemplate = paste0(
            '<b>%{x|%B %Y}</b><br>',
            'Proyección Lista: %{y:,.0f}<extra></extra>'
          )
        )
      }
      
      # ========== CONFIGURACIÓN DEL EJE X ==========
      fechas_reales <- datos_completos$fecha
      
      if (!is.null(proyeccion) && nrow(proyeccion) > 0) {
        fechas_completas_eje <- c(fechas_reales, proyeccion$fecha)
      } else {
        fechas_completas_eje <- fechas_reales
      }
      
      etiquetas_meses <- format(fechas_completas_eje, "%b")
      
      # ========== PREPARAR ANNOTATIONS ==========
      # ✅ v2.14: Fuente en y = -0.35 (desktop) - JS ajusta a -0.60 en mobile para 4 trazas
      annotations_list <- list(
        list(
          text = isolate(texto_alcance()),
          x = 0.5, y = 1.12,
          xref = "paper", yref = "paper",
          xanchor = "center", yanchor = "top",
          showarrow = FALSE,
          font = list(size = 13, color = "#555555", family = "Arial, sans-serif"),
          align = "center"
        ),
        list(
          text = "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado",
          x = 0.5, y = -0.35,
          xref = "paper", yref = "paper",
          xanchor = "center", yanchor = "top",
          showarrow = FALSE,
          font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
          align = "center"
        )
      )
      
      # Layout con eje X corregido
      p <- p %>% layout(
        title = list(
          text = paste0("Proyección ", year_datos, " - Padrón y LNE Nacional"),
          font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
          x = 0.5,
          xanchor = "center"
        ),
        xaxis = list(
          title = "",
          type = 'date',
          tickmode = "array",
          tickvals = fechas_completas_eje,
          ticktext = etiquetas_meses,
          tickangle = 0,
          range = c(min(fechas_reales) - 5, 
                    as.Date(paste0(year_datos, "-12-31")))
        ),
        yaxis = list(
          title = "Número de Electores", 
          separatethousands = TRUE
        ),
        legend = list(
          orientation = "h", 
          xanchor = "center", 
          x = 0.5, 
          y = -0.20
        ),
        margin = list(t = 120, b = 140, l = 90, r = 50),
        hovermode = 'x unified',
        annotations = annotations_list
      )
      
      message("✅ Gráfico 1: Proyección ", year_datos, " Nacional renderizado")
      return(p)
      
    } else {
      # ========== GRÁFICA EXTRANJERO ==========
      
      # Reemplazar NA con 0 para graficar
      datos_extranjero <- datos_completos
      if ("padron_extranjero" %in% colnames(datos_extranjero)) {
        datos_extranjero$padron_extranjero[is.na(datos_extranjero$padron_extranjero)] <- 0
      } else {
        datos_extranjero$padron_extranjero <- 0
      }
      
      if ("lista_extranjero" %in% colnames(datos_extranjero)) {
        datos_extranjero$lista_extranjero[is.na(datos_extranjero$lista_extranjero)] <- 0
      } else {
        datos_extranjero$lista_extranjero <- 0
      }
      
      # Proyectar usando columnas extranjero
      proyeccion <- NULL
      if (meses_restantes > 0) {
        datos_para_proyeccion <- datos_extranjero
        datos_para_proyeccion$lista_nominal <- datos_para_proyeccion$lista_extranjero
        datos_para_proyeccion$padron_electoral <- datos_para_proyeccion$padron_extranjero
        proyeccion <- proyectar_con_tasa_crecimiento(datos_para_proyeccion, meses_restantes)
      }
      
      # Crear gráfico
      p <- plot_ly()
      
      # 1. Padrón Extranjero
      p <- p %>% add_trace(
        data = datos_extranjero,
        x = ~fecha,
        y = ~padron_extranjero,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Padrón Extranjero',
        line = list(color = '#EAC43E', width = 3),
        marker = list(size = 8, color = '#EAC43E'),
        hovertemplate = paste0(
          '<b>%{x|%B %Y}</b><br>',
          'Padrón Extranjero: %{y:,.0f}<extra></extra>'
        )
      )
      
      # 2. Lista Extranjero
      p <- p %>% add_trace(
        data = datos_extranjero,
        x = ~fecha,
        y = ~lista_extranjero,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Lista Extranjero',
        line = list(color = '#B3D491', width = 3),
        marker = list(size = 8, color = '#B3D491'),
        hovertemplate = paste0(
          '<b>%{x|%B %Y}</b><br>',
          'Lista Extranjero: %{y:,.0f}<extra></extra>'
        )
      )
      
      # Proyecciones
      if (!is.null(proyeccion)) {
        # Proyección Padrón
        p <- p %>% add_trace(
          data = proyeccion,
          x = ~fecha,
          y = ~padron_proyectado,
          type = 'scatter',
          mode = 'lines',
          name = 'Proyección Padrón',
          line = list(color = '#F5CA45', width = 2, dash = 'dash'),
          hovertemplate = paste0(
            '<b>%{x|%B %Y}</b><br>',
            'Proyección Padrón: %{y:,.0f}<extra></extra>'
          )
        )
        
        # Proyección Lista
        p <- p %>% add_trace(
          data = proyeccion,
          x = ~fecha,
          y = ~lista_proyectada,
          type = 'scatter',
          mode = 'lines',
          name = 'Proyección Lista',
          line = list(color = '#CCE4B1', width = 2, dash = 'dash'),
          hovertemplate = paste0(
            '<b>%{x|%B %Y}</b><br>',
            'Proyección Lista: %{y:,.0f}<extra></extra>'
          )
        )
      }
      
      # ========== CONFIGURACIÓN DEL EJE X ==========
      fechas_reales <- datos_extranjero$fecha
      
      if (!is.null(proyeccion) && nrow(proyeccion) > 0) {
        fechas_completas_eje <- c(fechas_reales, proyeccion$fecha)
      } else {
        fechas_completas_eje <- fechas_reales
      }
      
      etiquetas_meses <- format(fechas_completas_eje, "%b")
      
      # ========== PREPARAR ANNOTATIONS ==========
      # ✅ v2.14: Fuente en y = -0.35 (desktop) - JS ajusta a -0.60 en mobile para 4 trazas
      annotations_list <- list(
        list(
          text = isolate(texto_alcance()),
          x = 0.5, y = 1.12,
          xref = "paper", yref = "paper",
          xanchor = "center", yanchor = "top",
          showarrow = FALSE,
          font = list(size = 13, color = "#555555", family = "Arial, sans-serif"),
          align = "center"
        ),
        list(
          text = "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado",
          x = 0.5, y = -0.35,
          xref = "paper", yref = "paper",
          xanchor = "center", yanchor = "top",
          showarrow = FALSE,
          font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
          align = "center"
        )
      )
      
      # Layout con eje X corregido
      p <- p %>% layout(
        title = list(
          text = paste0("Proyección ", year_datos, " - Padrón y LNE de Residentes en el Extranjero"),
          font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
          x = 0.5,
          xanchor = "center"
        ),
        xaxis = list(
          title = "",
          type = 'date',
          tickmode = "array",
          tickvals = fechas_completas_eje,
          ticktext = etiquetas_meses,
          tickangle = 0,
          range = c(min(fechas_reales) - 5, 
                    as.Date(paste0(year_datos, "-12-31")))
        ),
        yaxis = list(
          title = "Número de Electores", 
          separatethousands = TRUE
        ),
        legend = list(
          orientation = "h", 
          xanchor = "center", 
          x = 0.5, 
          y = -0.20
        ),
        margin = list(t = 120, b = 140, l = 90, r = 50),
        hovermode = 'x unified',
        annotations = annotations_list
      )
      
      message("✅ Gráfico 1: Proyección ", year_datos, " Extranjero renderizado")
      return(p)
    }
    
  }) %>%
    bindEvent(
      estado_app(),
      input$btn_consultar,
      ambito_reactivo(),
      ignoreNULL = FALSE,
      ignoreInit = FALSE
    )
  
  # ========== GRÁFICA 2: EVOLUCIÓN ANUAL ==========
  
  output$grafico_evolucion_anual <- renderPlotly({
    req(input$tipo_corte == "historico")
    
    ambito_actual <- ambito_reactivo()
    
    # ========== NO RENDERIZAR EN ESTADO INICIAL ==========
    if (estado_app() == "inicial") {
      return(plot_ly() %>%
               layout(
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE),
                 annotations = list(
                   list(
                     text = "Presione 'Consultar' para visualizar datos",
                     xref = "paper", yref = "paper",
                     x = 0.5, y = 0.5,
                     showarrow = FALSE,
                     font = list(size = 16, color = "#999")
                   )
                 )
               ))
    }
    
    # ========== CONTROL DE RENDERIZADO ==========
    if (!mostrar_graficas_anuales()) {
      return(NULL)
    }
    
    datos_anuales <- datos_anuales_completos()
    
    message("📊 [GRÁFICA 2] Renderizando - Estado: ", estado_app(), " | Ámbito: ", ambito_actual)
    
    # ========== SIN DATOS DISPONIBLES ==========
    if (is.null(datos_anuales) || !is.data.frame(datos_anuales) || nrow(datos_anuales) == 0) {
      return(plot_ly() %>%
               layout(
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE),
                 annotations = list(
                   list(
                     text = "No hay datos disponibles para esa consulta",
                     xref = "paper", yref = "paper",
                     x = 0.5, y = 0.5,
                     showarrow = FALSE,
                     font = list(size = 14, color = "#666")
                   )
                 )
               ))
    }
    
    # ========== GRÁFICA NACIONAL ==========
    if (ambito_actual == "nacional") {
      
      p <- plot_ly()
      
      # 1. Padrón Nacional
      p <- p %>% add_trace(
        data = datos_anuales,
        x = ~año,
        y = ~padron_nacional,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Padrón Nacional',
        line = list(color = '#003E66', width = 3),
        marker = list(size = 10, color = '#003E66'),
        hovertemplate = paste0(
          '<b>%{x}</b><br>',
          'Padrón Nacional: %{y:,.0f}<extra></extra>'
        )
      )
      
      # 2. Lista Nacional
      p <- p %>% add_trace(
        data = datos_anuales,
        x = ~año,
        y = ~lista_nacional,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Lista Nacional',
        line = list(color = '#AE0E35', width = 3),
        marker = list(size = 10, color = '#AE0E35'),
        hovertemplate = paste0(
          '<b>%{x}</b><br>',
          'Lista Nacional: %{y:,.0f}<extra></extra>'
        )
      )
      
      # ========== PREPARAR ANNOTATIONS ==========
      # ✅ v2.14: Fuente en y = -0.35 (desktop) - JS ajusta a -0.40 en mobile para 2 trazas
      annotations_list <- list(
        list(
          text = isolate(texto_alcance()),
          x = 0.5, y = 1.12,
          xref = "paper", yref = "paper",
          xanchor = "center", yanchor = "top",
          showarrow = FALSE,
          font = list(size = 13, color = "#555555", family = "Arial, sans-serif"),
          align = "center"
        ),
        list(
          text = "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado",
          x = 0.5, y = -0.35,
          xref = "paper", yref = "paper",
          xanchor = "center", yanchor = "top",
          showarrow = FALSE,
          font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
          align = "center"
        )
      )
      
      # Layout
      p <- p %>% layout(
        title = list(
          text = paste0("Evolución Anual (2017-", anio_actual(), ") - Padrón y LNE Nacional"),
          font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
          x = 0.5,
          xanchor = "center"
        ),
        xaxis = list(title = "", type = 'category'),
        yaxis = list(title = "Número de Electores", separatethousands = TRUE),
        legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.20),
        margin = list(t = 120, b = 140, l = 90, r = 50),
        hovermode = 'x unified',
        annotations = annotations_list
      )
      
      message("✅ Gráfico 2: Evolución anual Nacional renderizado")
      return(p)
      
    } else {
      # ========== GRÁFICA EXTRANJERO ==========
      
      # Reemplazar NA con 0 para graficar
      datos_extranjero <- datos_anuales
      if ("padron_extranjero" %in% colnames(datos_extranjero)) {
        datos_extranjero$padron_extranjero[is.na(datos_extranjero$padron_extranjero)] <- 0
      } else {
        datos_extranjero$padron_extranjero <- 0
      }
      
      if ("lista_extranjero" %in% colnames(datos_extranjero)) {
        datos_extranjero$lista_extranjero[is.na(datos_extranjero$lista_extranjero)] <- 0
      } else {
        datos_extranjero$lista_extranjero <- 0
      }
      
      p <- plot_ly()
      
      # 1. Padrón Extranjero
      p <- p %>% add_trace(
        data = datos_extranjero,
        x = ~año,
        y = ~padron_extranjero,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Padrón Extranjero',
        line = list(color = '#EAC43E', width = 3),
        marker = list(size = 10, color = '#EAC43E'),
        hovertemplate = paste0(
          '<b>%{x}</b><br>',
          'Padrón Extranjero: %{y:,.0f}<extra></extra>'
        )
      )
      
      # 2. Lista Extranjero
      p <- p %>% add_trace(
        data = datos_extranjero,
        x = ~año,
        y = ~lista_extranjero,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Lista Extranjero',
        line = list(color = '#B3D491', width = 3),
        marker = list(size = 10, color = '#B3D491'),
        hovertemplate = paste0(
          '<b>%{x}</b><br>',
          'Lista Extranjero: %{y:,.0f}<extra></extra>'
        )
      )
      
      # ========== PREPARAR ANNOTATIONS ==========
      # ✅ v2.14: Fuente en y = -0.35 (desktop) - JS ajusta a -0.40 en mobile para 2 trazas
      annotations_list <- list(
        list(
          text = isolate(texto_alcance()),
          x = 0.5, y = 1.12,
          xref = "paper", yref = "paper",
          xanchor = "center", yanchor = "top",
          showarrow = FALSE,
          font = list(size = 13, color = "#555555", family = "Arial, sans-serif"),
          align = "center"
        ),
        list(
          text = "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado",
          x = 0.5, y = -0.35,
          xref = "paper", yref = "paper",
          xanchor = "center", yanchor = "top",
          showarrow = FALSE,
          font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
          align = "center"
        )
      )
      
      # Layout
      p <- p %>% layout(
        title = list(
          text = paste0("Evolución Anual (2017-", anio_actual(), ") - Residentes en el Extranjero"),
          font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
          x = 0.5,
          xanchor = "center"
        ),
        xaxis = list(title = "", type = 'category'),
        yaxis = list(title = "Número de Electores", separatethousands = TRUE),
        legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.20),
        margin = list(t = 120, b = 140, l = 90, r = 50),
        hovermode = 'x unified',
        annotations = annotations_list
      )
      
      message("✅ Gráfico 2: Evolución anual Extranjero renderizado")
      return(p)
    }
    
  }) %>%
    bindEvent(
      estado_app(),
      input$btn_consultar,
      ambito_reactivo(),
      ignoreNULL = FALSE,
      ignoreInit = FALSE
    )
  
  # ========== MODAL METODOLOGÍA ==========
  
  observeEvent(input$info_grafica1, {
    showModal(modalDialog(
      title = tags$div(
        style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
        tags$span(
          style = "color: #003E66; font-weight: bold; font-size: 18px;",
          icon("chart-line"), " Metodología de Proyección"
        ),
        tags$button(
          type = "button",
          class = "close modal-close-x",
          `data-dismiss` = "modal",
          `aria-label` = "Cerrar",
          style = "font-size: 24px; font-weight: bold; color: #666; opacity: 0.8; border: none; background: transparent; cursor: pointer; padding: 0; margin: -10px -5px 0 0;",
          HTML("&times;")
        )
      ),
      tags$div(
        class = "modal-metodologia-content",
        style = "font-size: 14px; line-height: 1.6;",
        
        tags$h5(
          style = "color: #44559B; font-weight: bold; margin-top: 10px;",
          "¿Cómo se calcula la proyección?"
        ),
        tags$p(
          "La proyección utiliza un ", 
          tags$strong("modelo de tasa de crecimiento mensual promedio"), 
          " basado en los datos históricos del año en curso."
        ),
        
        tags$h5(
          style = "color: #44559B; font-weight: bold; margin-top: 12px;",
          "Pasos del cálculo:"
        ),
        tags$ol(
          style = "padding-left: 18px; margin-bottom: 10px;",
          tags$li(tags$strong("Datos base:"), " Cortes mensuales del año actual."),
          tags$li(tags$strong("Tasa de crecimiento:"), " Promedio mensual entre primer y último mes."),
          tags$li(tags$strong("Proyección:"), " Se aplica la tasa hasta diciembre."),
          tags$li(tags$strong("Visualización:"), " Líneas punteadas = valores proyectados.")
        ),
        
        tags$h5(
          style = "color: #44559B; font-weight: bold; margin-top: 12px;",
          "Fórmula:"
        ),
        tags$div(
          style = "background-color: #f8f9fa; padding: 10px; border-left: 3px solid #003E66; margin: 8px 0; font-family: monospace; font-size: 11px;",
          tags$code("Tasa = (Valor_final / Valor_inicial)^(1/(n-1)) - 1"),
          tags$br(),
          tags$code("Proyección(i) = Último_valor × (1 + tasa)^i")
        ),
        
        tags$h5(
          style = "color: #AE0E35; font-weight: bold; margin-top: 12px;",
          icon("exclamation-triangle"), " Consideraciones:"
        ),
        tags$ul(
          style = "padding-left: 18px; margin-bottom: 10px;",
          tags$li("Asume crecimiento ", tags$strong("constante"), "."),
          tags$li("Es una ", tags$strong("estimación estadística"), "."),
          tags$li("Proyecta hasta ", tags$strong("diciembre"), " del año."),
          tags$li("Los datos oficiales del INE prevalecen.")
        ),
        
        tags$hr(style = "margin: 12px 0;"),
        tags$p(
          style = "font-size: 11px; color: #666; text-align: center; margin-bottom: 0;",
          icon("info-circle"), " Esta es una herramienta de referencia. ",
          "Los datos oficiales son los publicados por el INE."
        )
      ),
      
      easyClose = TRUE,
      fade = TRUE,
      size = "m",
      
      footer = tags$div(
        style = "text-align: center;",
        modalButton(
          label = "Cerrar",
          icon = NULL
        ) %>% tagAppendAttributes(
          style = "background-color: #006988; border-color: #006988; color: white; font-weight: bold; padding: 8px 30px;"
        )
      )
    ))
  })
  
  message("✅ graficas_historico_1_2 v2.14 inicializado")
  message("   ✅ v2.14: Fuente en y=-0.35 para desktop (todas las gráficas)")
  message("   ✅ v2.14: JS ajusta a y=-0.60 en mobile para gráficas con 4 trazas")
  message("   ✅ v2.14: JS ajusta a y=-0.40 en mobile para gráficas con 2 trazas")
}
