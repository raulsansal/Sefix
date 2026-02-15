# modules/lista_nominal_graficas/graficas_consultado_4_5.R
# Gráficas 4 y 5: Evolución mensual del año consultado (con y sin desglose por sexo)
# Versión: 2.3 - Texto alcance reposicionado (y=1.15) + screen_width robusto
# Cambios v2.3:
#   - Texto de alcance subido 20px (y=1.15 en lugar de y=1.12)
#   - screen_width se obtiene de forma más robusta

graficas_consultado_4_5 <- function(input, output, session, datos_year_consulta, 
                                    anio_consultado, texto_alcance, estado_app, mostrar_graficas_consultadas, ambito_reactivo) {
  
  message("📊 Inicializando graficas_consultado_4_5 v2.3")
  
  # ========== GRÁFICA 4: EVOLUCIÓN MENSUAL DEL AÑO SELECCIONADO ==========
  
  output$grafico_evolucion_year <- renderPlotly({
    req(input$tipo_corte == "historico")
    
    ambito_actual <- ambito_reactivo()
    
    datos_completos <- datos_year_consulta()
    
    # ========== NO MOSTRAR SI NO SE DEBE ==========
    if (!mostrar_graficas_consultadas()) {
      return(NULL)
    }
    
    if (is.null(datos_completos) || !is.data.frame(datos_completos) || nrow(datos_completos) == 0) {
      return(plot_ly() %>%
               layout(
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE),
                 annotations = list(
                   list(
                     text = "No hay datos disponibles",
                     xref = "paper", yref = "paper",
                     x = 0.5, y = 0.5,
                     showarrow = FALSE,
                     font = list(size = 14, color = "#666")
                   )
                 )
               ))
    }
    
    year_datos <- format(datos_completos$fecha[1], "%Y")
    
    message("📊 [GRÁFICA 4] Renderizando año ", year_datos, " - Ámbito: ", ambito_actual)
    
    if (ambito_actual == "nacional") {
      
      p <- plot_ly()
      
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
      
      fechas_datos <- datos_completos$fecha
      etiquetas_meses <- format(fechas_datos, "%b")
      
      # ========== PREPARAR ANNOTATIONS ==========
      # ✅ v2.3: Texto alcance en y = 1.15 (subido 20px)
      annotations_list <- list(
        list(
          text = isolate(texto_alcance()),
          x = 0.5, y = 1.15,
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
      
      p <- p %>% layout(
        title = list(
          text = paste0("Evolución Mensual ", year_datos, " - Padrón y LNE Nacional"),
          font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
          x = 0.5,
          xanchor = "center"
        ),
        xaxis = list(
          title = "",
          type = 'date',
          tickmode = "array",
          tickvals = fechas_datos,
          ticktext = etiquetas_meses,
          tickangle = 0,
          range = c(min(fechas_datos) - 5, max(fechas_datos) + 5)
        ),
        yaxis = list(title = "Número de Electores", separatethousands = TRUE),
        legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.20),
        margin = list(t = 120, b = 140, l = 90, r = 50),
        hovermode = 'x unified',
        annotations = annotations_list
      )
      
      message("✅ Gráfico 4: Evolución mensual ", year_datos, " Nacional renderizado")
      return(p)
      
    } else {
      # ========== GRÁFICA EXTRANJERO ==========
      
      if (!("padron_extranjero" %in% colnames(datos_completos)) ||
          !("lista_extranjero" %in% colnames(datos_completos))) {
        
        return(plot_ly() %>%
                 layout(
                   xaxis = list(visible = FALSE),
                   yaxis = list(visible = FALSE),
                   annotations = list(
                     list(
                       text = paste0("Desglose por ámbito extranjero no disponible para ", year_datos, ".<br>",
                                     "Verifique que los datos hayan sido cargados correctamente."),
                       xref = "paper", yref = "paper",
                       x = 0.5, y = 0.5,
                       showarrow = FALSE,
                       font = list(size = 14, color = "#666")
                     )
                   )
                 ))
      }
      
      datos_extranjero <- datos_completos
      datos_extranjero$padron_extranjero[is.na(datos_extranjero$padron_extranjero)] <- 0
      datos_extranjero$lista_extranjero[is.na(datos_extranjero$lista_extranjero)] <- 0
      
      message("📊 [GRÁFICA 4] Graficando ", nrow(datos_extranjero), " meses de extranjero (año ", year_datos, ")")
      
      p <- plot_ly()
      
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
      
      fechas_extranjero <- datos_extranjero$fecha
      etiquetas_meses <- format(fechas_extranjero, "%b")
      
      # ========== PREPARAR ANNOTATIONS ==========
      # ✅ v2.3: Texto alcance en y = 1.15 (subido 20px)
      annotations_list <- list(
        list(
          text = isolate(texto_alcance()),
          x = 0.5, y = 1.15,
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
      
      p <- p %>% layout(
        title = list(
          text = paste0("Evolución Mensual ", year_datos, " - Residentes en el Extranjero"),
          font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
          x = 0.5,
          xanchor = "center"
        ),
        xaxis = list(
          title = "",
          type = 'date',
          tickmode = "array",
          tickvals = fechas_extranjero,
          ticktext = etiquetas_meses,
          tickangle = 0,
          range = c(min(fechas_extranjero) - 5, max(fechas_extranjero) + 5)
        ),
        yaxis = list(title = "Número de Electores", separatethousands = TRUE),
        legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.20),
        margin = list(t = 120, b = 140, l = 90, r = 50),
        hovermode = 'x unified',
        annotations = annotations_list
      )
      
      message("✅ Gráfico 4: Evolución mensual ", year_datos, " Extranjero renderizado")
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
  
  # ========== GRÁFICA 5: EVOLUCIÓN MENSUAL DEL AÑO SELECCIONADO + SEXO ==========
  
  output$grafico_evolucion_year_sexo <- renderPlotly({
    req(input$tipo_corte == "historico")
    
    ambito_actual <- ambito_reactivo()
    
    # ========== NO MOSTRAR SI NO SE DEBE ==========
    if (!mostrar_graficas_consultadas()) {
      return(NULL)
    }
    
    datos_completos <- datos_year_consulta()
    
    if (is.null(datos_completos) || !is.data.frame(datos_completos) || nrow(datos_completos) == 0) {
      return(plot_ly() %>%
               layout(
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE),
                 annotations = list(
                   list(
                     text = "No hay datos disponibles",
                     xref = "paper", yref = "paper",
                     x = 0.5, y = 0.5,
                     showarrow = FALSE,
                     font = list(size = 14, color = "#666")
                   )
                 )
               ))
    }
    
    year_datos <- format(datos_completos$fecha[1], "%Y")
    
    message("📊 [GRÁFICA 5] Renderizando año ", year_datos, " - Ámbito: ", ambito_actual)
    
    # ========== GRÁFICA NACIONAL ==========
    if (ambito_actual == "nacional") {
      
      if (!all(c("padron_hombres", "padron_mujeres", "lista_hombres", "lista_mujeres") %in% colnames(datos_completos))) {
        return(plot_ly() %>%
                 layout(
                   xaxis = list(visible = FALSE),
                   yaxis = list(visible = FALSE),
                   annotations = list(
                     list(
                       text = "Desglose por sexo no disponible",
                       xref = "paper", yref = "paper",
                       x = 0.5, y = 0.5,
                       showarrow = FALSE,
                       font = list(size = 14, color = "#666")
                     )
                   )
                 ))
      }
      
      p <- plot_ly()
      
      ultimo_padron_h <- tail(datos_completos$padron_hombres[!is.na(datos_completos$padron_hombres)], 1)
      ultimo_padron_m <- tail(datos_completos$padron_mujeres[!is.na(datos_completos$padron_mujeres)], 1)
      ultimo_lista_h <- tail(datos_completos$lista_hombres[!is.na(datos_completos$lista_hombres)], 1)
      ultimo_lista_m <- tail(datos_completos$lista_mujeres[!is.na(datos_completos$lista_mujeres)], 1)
      
      trazas_info <- data.frame(
        nombre = c("padron_h", "padron_m", "lista_h", "lista_m"),
        valor = c(ultimo_padron_h, ultimo_padron_m, ultimo_lista_h, ultimo_lista_m),
        stringsAsFactors = FALSE
      )
      
      trazas_info <- trazas_info[order(trazas_info$valor, decreasing = TRUE), ]
      
      for (i in 1:nrow(trazas_info)) {
        traza_nombre <- trazas_info$nombre[i]
        
        if (traza_nombre == "padron_h") {
          p <- p %>% add_trace(
            data = datos_completos,
            x = ~fecha,
            y = ~padron_hombres,
            type = 'scatter',
            mode = 'lines+markers',
            name = 'Padrón Hombres',
            line = list(color = '#4A90E2', width = 2.5),
            marker = list(size = 8, color = '#4A90E2'),
            hovertemplate = paste0('<b>%{x|%B %Y}</b><br>Padrón H: %{y:,.0f}<extra></extra>')
          )
        } else if (traza_nombre == "padron_m") {
          p <- p %>% add_trace(
            data = datos_completos,
            x = ~fecha,
            y = ~padron_mujeres,
            type = 'scatter',
            mode = 'lines+markers',
            name = 'Padrón Mujeres',
            line = list(color = '#E24A90', width = 2.5),
            marker = list(size = 8, color = '#E24A90'),
            hovertemplate = paste0('<b>%{x|%B %Y}</b><br>Padrón M: %{y:,.0f}<extra></extra>')
          )
        } else if (traza_nombre == "lista_h") {
          p <- p %>% add_trace(
            data = datos_completos,
            x = ~fecha,
            y = ~lista_hombres,
            type = 'scatter',
            mode = 'lines+markers',
            name = 'Lista Hombres',
            line = list(color = '#2E5C8A', width = 2.5, dash = 'dot'),
            marker = list(size = 8, color = '#2E5C8A', symbol = 'square'),
            hovertemplate = paste0('<b>%{x|%B %Y}</b><br>Lista H: %{y:,.0f}<extra></extra>')
          )
        } else if (traza_nombre == "lista_m") {
          p <- p %>% add_trace(
            data = datos_completos,
            x = ~fecha,
            y = ~lista_mujeres,
            type = 'scatter',
            mode = 'lines+markers',
            name = 'Lista Mujeres',
            line = list(color = '#A83565', width = 2.5, dash = 'dot'),
            marker = list(size = 8, color = '#A83565', symbol = 'square'),
            hovertemplate = paste0('<b>%{x|%B %Y}</b><br>Lista M: %{y:,.0f}<extra></extra>')
          )
        }
      }
      
      fechas_datos <- datos_completos$fecha
      etiquetas_meses <- format(fechas_datos, "%b")
      
      # ========== PREPARAR ANNOTATIONS ==========
      # ✅ v2.3: Texto alcance en y = 1.15 (subido 20px)
      annotations_list <- list(
        list(
          text = isolate(texto_alcance()),
          x = 0.5, y = 1.15,
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
      
      # Card NB SÍ aplica en gráfica 5 (CON desglose por sexo)
      # ✅ v2.3: Obtener screen_width de forma robusta
      screen_w <- isolate(input$screen_width)
      message("📱 [GRÁFICA 5] screen_width recibido: ", if(is.null(screen_w)) "NULL" else screen_w)
      card_nb <- crear_card_no_binario(datos_completos, ambito = "nacional", tipo_periodo = "mensual", año_consultado = year_datos, screen_width = screen_w)
      if (!is.null(card_nb)) {
        annotations_list[[length(annotations_list) + 1]] <- card_nb
      }
      
      p <- p %>% layout(
        title = list(
          text = paste0("Evolución Mensual ", year_datos, " por Sexo - Padrón y LNE Nacional"),
          font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
          x = 0.5,
          xanchor = "center"
        ),
        xaxis = list(
          title = "",
          type = 'date',
          tickmode = "array",
          tickvals = fechas_datos,
          ticktext = etiquetas_meses,
          tickangle = 0,
          range = c(min(fechas_datos) - 5, max(fechas_datos) + 5)
        ),
        yaxis = list(title = "Número de Electores", separatethousands = TRUE),
        legend = list(
          orientation = "h", 
          xanchor = "center", 
          x = 0.5, 
          y = -0.20,
          traceorder = "normal"
        ),
        margin = list(t = 120, b = 140, l = 90, r = 50),
        hovermode = 'x unified',
        annotations = annotations_list
      )
      
      message("✅ Gráfico 5: Evolución mensual ", year_datos, " por sexo Nacional renderizado")
      return(p)
      
    } else {
      # ========== GRÁFICA EXTRANJERO ==========
      
      if (ambito_actual != "extranjero") {
        return(plot_ly() %>%
                 layout(
                   xaxis = list(visible = FALSE),
                   yaxis = list(visible = FALSE),
                   annotations = list(
                     list(
                       text = "Esta gráfica requiere seleccionar 'Ámbito: Extranjero'",
                       xref = "paper", yref = "paper",
                       x = 0.5, y = 0.5,
                       showarrow = FALSE,
                       font = list(size = 14, color = "#666")
                     )
                   )
                 ))
      }
      
      cols_sexo_extranjero <- c("padron_extranjero_hombres", "padron_extranjero_mujeres", 
                                "lista_extranjero_hombres", "lista_extranjero_mujeres")
      
      tiene_columnas_sexo <- all(cols_sexo_extranjero %in% colnames(datos_completos))
      
      if (!tiene_columnas_sexo) {
        message("⚠️ [GRAF5] Columnas de sexo NO existen para año ", year_datos)
        return(plot_ly() %>%
                 layout(
                   xaxis = list(visible = FALSE),
                   yaxis = list(visible = FALSE),
                   annotations = list(
                     list(
                       text = paste0("Desglose por sexo no disponible para extranjero en ", year_datos),
                       xref = "paper", yref = "paper",
                       x = 0.5, y = 0.5,
                       showarrow = FALSE,
                       font = list(size = 14, color = "#666")
                     )
                   )
                 ))
      }
      
      datos_extranjero <- datos_completos
      datos_extranjero$padron_extranjero_hombres[is.na(datos_extranjero$padron_extranjero_hombres)] <- 0
      datos_extranjero$padron_extranjero_mujeres[is.na(datos_extranjero$padron_extranjero_mujeres)] <- 0
      datos_extranjero$lista_extranjero_hombres[is.na(datos_extranjero$lista_extranjero_hombres)] <- 0
      datos_extranjero$lista_extranjero_mujeres[is.na(datos_extranjero$lista_extranjero_mujeres)] <- 0
      
      message("📊 [GRÁFICA 5] Graficando ", nrow(datos_extranjero), " meses de extranjero por sexo (año ", year_datos, ")")
      
      p <- plot_ly()
      
      ultimo_padron_h <- tail(datos_extranjero$padron_extranjero_hombres, 1)
      ultimo_padron_m <- tail(datos_extranjero$padron_extranjero_mujeres, 1)
      ultimo_lista_h <- tail(datos_extranjero$lista_extranjero_hombres, 1)
      ultimo_lista_m <- tail(datos_extranjero$lista_extranjero_mujeres, 1)
      
      trazas_info <- data.frame(
        nombre = c("padron_h", "padron_m", "lista_h", "lista_m"),
        valor = c(ultimo_padron_h, ultimo_padron_m, ultimo_lista_h, ultimo_lista_m),
        stringsAsFactors = FALSE
      )
      
      trazas_info <- trazas_info[order(trazas_info$valor, decreasing = TRUE), ]
      
      for (i in 1:nrow(trazas_info)) {
        traza_nombre <- trazas_info$nombre[i]
        
        if (traza_nombre == "padron_h") {
          p <- p %>% add_trace(
            data = datos_extranjero,
            x = ~fecha,
            y = ~padron_extranjero_hombres,
            type = 'scatter',
            mode = 'lines+markers',
            name = 'Padrón Hombres',
            line = list(color = '#D4A500', width = 2.5),
            marker = list(size = 8, color = '#D4A500'),
            hovertemplate = paste0('<b>%{x|%B %Y}</b><br>Padrón H: %{y:,.0f}<extra></extra>')
          )
        } else if (traza_nombre == "padron_m") {
          p <- p %>% add_trace(
            data = datos_extranjero,
            x = ~fecha,
            y = ~padron_extranjero_mujeres,
            type = 'scatter',
            mode = 'lines+markers',
            name = 'Padrón Mujeres',
            line = list(color = '#F5CA45', width = 2.5),
            marker = list(size = 8, color = '#F5CA45'),
            hovertemplate = paste0('<b>%{x|%B %Y}</b><br>Padrón M: %{y:,.0f}<extra></extra>')
          )
        } else if (traza_nombre == "lista_h") {
          p <- p %>% add_trace(
            data = datos_extranjero,
            x = ~fecha,
            y = ~lista_extranjero_hombres,
            type = 'scatter',
            mode = 'lines+markers',
            name = 'Lista Hombres',
            line = list(color = '#8FB369', width = 2.5, dash = 'dot'),
            marker = list(size = 8, color = '#8FB369', symbol = 'square'),
            hovertemplate = paste0('<b>%{x|%B %Y}</b><br>Lista H: %{y:,.0f}<extra></extra>')
          )
        } else if (traza_nombre == "lista_m") {
          p <- p %>% add_trace(
            data = datos_extranjero,
            x = ~fecha,
            y = ~lista_extranjero_mujeres,
            type = 'scatter',
            mode = 'lines+markers',
            name = 'Lista Mujeres',
            line = list(color = '#CCE4B1', width = 2.5, dash = 'dot'),
            marker = list(size = 8, color = '#CCE4B1', symbol = 'square'),
            hovertemplate = paste0('<b>%{x|%B %Y}</b><br>Lista M: %{y:,.0f}<extra></extra>')
          )
        }
      }
      
      fechas_extranjero <- datos_extranjero$fecha
      etiquetas_meses <- format(fechas_extranjero, "%b")
      
      # ========== PREPARAR ANNOTATIONS ==========
      # ✅ v2.3: Texto alcance en y = 1.15 (subido 20px)
      annotations_list <- list(
        list(
          text = isolate(texto_alcance()),
          x = 0.5, y = 1.15,
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
      
      # Card NB SÍ aplica en gráfica 5 (CON desglose por sexo)
      # ✅ v2.3: Obtener screen_width de forma robusta
      screen_w <- isolate(input$screen_width)
      message("📱 [GRÁFICA 5 EXT] screen_width recibido: ", if(is.null(screen_w)) "NULL" else screen_w)
      card_nb <- crear_card_no_binario(datos_extranjero, ambito = "extranjero", tipo_periodo = "mensual", año_consultado = year_datos, screen_width = screen_w)
      if (!is.null(card_nb)) {
        annotations_list[[length(annotations_list) + 1]] <- card_nb
      }
      
      p <- p %>% layout(
        title = list(
          text = paste0("Evolución Mensual ", year_datos, " por Sexo - Residentes en el Extranjero"),
          font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
          x = 0.5,
          xanchor = "center"
        ),
        xaxis = list(
          title = "",
          type = 'date',
          tickmode = "array",
          tickvals = fechas_extranjero,
          ticktext = etiquetas_meses,
          tickangle = 0,
          range = c(min(fechas_extranjero) - 5, max(fechas_extranjero) + 5)
        ),
        yaxis = list(title = "Número de Electores", separatethousands = TRUE),
        legend = list(
          orientation = "h", 
          xanchor = "center", 
          x = 0.5, 
          y = -0.20,
          traceorder = "normal"
        ),
        margin = list(t = 120, b = 140, l = 90, r = 50),
        hovermode = 'x unified',
        annotations = annotations_list
      )
      
      message("✅ Gráfico 5: Evolución mensual ", year_datos, " por sexo Extranjero renderizado")
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
  
  message("✅ graficas_consultado_4_5 v2.3 inicializado")
  message("   ✅ v2.3: Texto alcance en y=1.15 (subido 20px)")
}
