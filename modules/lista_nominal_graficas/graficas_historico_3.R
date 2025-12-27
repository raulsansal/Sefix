# modules/lista_nominal_graficas/graficas_historico_3.R
# Gráfica 3: Evolución anual por sexo
# Versión: 2.6 - CORRECCIÓN: Usar ambito_reactivo para cambio de vista automático

graficas_historico_3 <- function(input, output, session, datos_anuales_completos, anio_actual, 
                                 texto_alcance, estado_app, mostrar_graficas_anuales, ambito_reactivo) {
  
  message("📊 Inicializando graficas_historico_3 v2.6")
  
  # ========== GRÁFICA 3: EVOLUCIÓN ANUAL + DESGLOSE POR SEXO ==========
  
  output$grafico_evolucion_anual_sexo <- renderPlotly({
    req(input$tipo_corte == "historico")
    
    # ✅ v2.6: Usar ambito_reactivo en lugar de input$ambito_datos
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
    
    message("📊 [GRÁFICA 3] Renderizando - Estado: ", estado_app(), " | Ámbito: ", ambito_actual)
    
    # ========== VALIDACIÓN ROBUSTA ==========
    if (is.null(datos_anuales) || !is.data.frame(datos_anuales) || nrow(datos_anuales) == 0) {
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
    
    # ========== GRÁFICA NACIONAL ==========
    if (ambito_actual == "nacional") {
      
      # Verificar que existan columnas de sexo
      if (!all(c("padron_hombres", "padron_mujeres", "lista_hombres", "lista_mujeres") %in% colnames(datos_anuales))) {
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
      
      # Crear gráfico con ORDEN DINÁMICO
      p <- plot_ly()
      
      # ========== ORDENAR TRAZAS DINÁMICAMENTE ==========
      # Obtener último valor de cada serie para determinar orden visual
      ultimo_padron_h <- tail(datos_anuales$padron_hombres[!is.na(datos_anuales$padron_hombres)], 1)
      ultimo_padron_m <- tail(datos_anuales$padron_mujeres[!is.na(datos_anuales$padron_mujeres)], 1)
      ultimo_lista_h <- tail(datos_anuales$lista_hombres[!is.na(datos_anuales$lista_hombres)], 1)
      ultimo_lista_m <- tail(datos_anuales$lista_mujeres[!is.na(datos_anuales$lista_mujeres)], 1)
      
      # Crear lista con metadatos de cada traza
      trazas_info <- data.frame(
        nombre = c("padron_h", "padron_m", "lista_h", "lista_m"),
        valor = c(ultimo_padron_h, ultimo_padron_m, ultimo_lista_h, ultimo_lista_m),
        stringsAsFactors = FALSE
      )
      
      # Ordenar de mayor a menor (orden visual de arriba a abajo)
      trazas_info <- trazas_info[order(trazas_info$valor, decreasing = TRUE), ]
      
      # Agregar trazas en el orden visual correcto
      for (i in 1:nrow(trazas_info)) {
        traza_nombre <- trazas_info$nombre[i]
        
        if (traza_nombre == "padron_h") {
          p <- p %>% add_trace(
            data = datos_anuales,
            x = ~año,
            y = ~padron_hombres,
            type = 'scatter',
            mode = 'lines+markers',
            name = 'Padrón Hombres',
            line = list(color = '#4A90E2', width = 2.5),
            marker = list(size = 8, color = '#4A90E2'),
            hovertemplate = paste0('<b>%{x}</b><br>Padrón H: %{y:,.0f}<extra></extra>')
          )
        } else if (traza_nombre == "padron_m") {
          p <- p %>% add_trace(
            data = datos_anuales,
            x = ~año,
            y = ~padron_mujeres,
            type = 'scatter',
            mode = 'lines+markers',
            name = 'Padrón Mujeres',
            line = list(color = '#E24A90', width = 2.5),
            marker = list(size = 8, color = '#E24A90'),
            hovertemplate = paste0('<b>%{x}</b><br>Padrón M: %{y:,.0f}<extra></extra>')
          )
        } else if (traza_nombre == "lista_h") {
          p <- p %>% add_trace(
            data = datos_anuales,
            x = ~año,
            y = ~lista_hombres,
            type = 'scatter',
            mode = 'lines+markers',
            name = 'Lista Hombres',
            line = list(color = '#2E5C8A', width = 2.5, dash = 'dot'),
            marker = list(size = 8, color = '#2E5C8A', symbol = 'square'),
            hovertemplate = paste0('<b>%{x}</b><br>Lista H: %{y:,.0f}<extra></extra>')
          )
        } else if (traza_nombre == "lista_m") {
          p <- p %>% add_trace(
            data = datos_anuales,
            x = ~año,
            y = ~lista_mujeres,
            type = 'scatter',
            mode = 'lines+markers',
            name = 'Lista Mujeres',
            line = list(color = '#A83565', width = 2.5, dash = 'dot'),
            marker = list(size = 8, color = '#A83565', symbol = 'square'),
            hovertemplate = paste0('<b>%{x}</b><br>Lista M: %{y:,.0f}<extra></extra>')
          )
        }
      }
      
      # ========== PREPARAR ANNOTATIONS (CON CARD NB) ==========
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
      
      # ✅ MANTENER: Card NB SÍ aplica en gráfica 3 (CON desglose por sexo)
      card_nb <- crear_card_no_binario(datos_anuales, ambito = "nacional", tipo_periodo = "anual")
      if (!is.null(card_nb)) {
        annotations_list[[length(annotations_list) + 1]] <- card_nb
      }
      
      # Layout
      p <- p %>% layout(
        title = list(
          text = paste0("Evolución Anual por Sexo (2017-", anio_actual(), ") - Nacional"),
          font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
          x = 0.5,
          xanchor = "center"
        ),
        xaxis = list(title = "", type = 'category'),
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
      
      message("✅ Gráfico 3: Evolución anual por sexo Nacional renderizado (CON card NB)")
      return(p)
      
    } else {
      # ========== GRÁFICA EXTRANJERO (LÓGICA HÍBRIDA) ==========
      
      # Filtrar años con datos de extranjero
      datos_extranjero <- datos_anuales[!is.na(datos_anuales$padron_extranjero) & 
                                          !is.na(datos_anuales$lista_extranjero), ]
      
      if (nrow(datos_extranjero) == 0) {
        return(plot_ly() %>%
                 layout(
                   xaxis = list(visible = FALSE),
                   yaxis = list(visible = FALSE),
                   annotations = list(
                     list(
                       text = "Datos de extranjero disponibles desde 2020",
                       xref = "paper", yref = "paper",
                       x = 0.5, y = 0.5,
                       showarrow = FALSE,
                       font = list(size = 14, color = "#666")
                     )
                   )
                 ))
      }
      
      # ========== DETECTAR AÑOS CON/SIN DATOS DE SEXO ==========
      # Verificar que las columnas existan
      tiene_cols_sexo <- all(c("padron_extranjero_hombres", "padron_extranjero_mujeres", 
                               "lista_extranjero_hombres", "lista_extranjero_mujeres") %in% 
                               colnames(datos_extranjero))
      
      if (!tiene_cols_sexo) {
        # Si no hay columnas de sexo, todos los años no tienen sexo
        datos_extranjero$tiene_sexo <- FALSE
      } else {
        # Crear vector lógico con la misma longitud que datos_extranjero
        tiene_sexo_vector <- !is.na(datos_extranjero$padron_extranjero_hombres) & 
          !is.na(datos_extranjero$padron_extranjero_mujeres) &
          !is.na(datos_extranjero$lista_extranjero_hombres) &
          !is.na(datos_extranjero$lista_extranjero_mujeres)
        
        # Asignar el vector (ahora tiene la misma longitud)
        datos_extranjero$tiene_sexo <- tiene_sexo_vector
      }
      
      años_sin_sexo <- datos_extranjero$año[!datos_extranjero$tiene_sexo]
      años_con_sexo <- datos_extranjero$año[datos_extranjero$tiene_sexo]
      
      message("📊 Años SIN sexo: ", paste(años_sin_sexo, collapse = ", "))
      message("📊 Años CON sexo: ", paste(años_con_sexo, collapse = ", "))
      
      # Crear gráfico
      p <- plot_ly()
      
      # ========== GRAFICAR AÑOS SIN SEXO (2 LÍNEAS) ==========
      if (length(años_sin_sexo) > 0) {
        datos_sin_sexo <- datos_extranjero[datos_extranjero$año %in% años_sin_sexo, ]
        
        # Padrón Total
        p <- p %>% add_trace(
          data = datos_sin_sexo,
          x = ~año,
          y = ~padron_extranjero,
          type = 'scatter',
          mode = 'lines+markers',
          name = 'Padrón Extranjero',
          line = list(color = '#EAC43E', width = 3),
          marker = list(size = 10, color = '#EAC43E'),
          hovertemplate = paste0(
            '<b>%{x}</b><br>',
            'Padrón: %{y:,.0f}<extra></extra>'
          )
        )
        
        # Lista Total
        p <- p %>% add_trace(
          data = datos_sin_sexo,
          x = ~año,
          y = ~lista_extranjero,
          type = 'scatter',
          mode = 'lines+markers',
          name = 'Lista Extranjero',
          line = list(color = '#B3D491', width = 3),
          marker = list(size = 10, color = '#B3D491'),
          hovertemplate = paste0(
            '<b>%{x}</b><br>',
            'Lista: %{y:,.0f}<extra></extra>'
          )
        )
      }
      
      # ========== GRAFICAR AÑOS CON SEXO (4 LÍNEAS) - ORDEN DINÁMICO ==========
      if (length(años_con_sexo) > 0) {
        datos_con_sexo <- datos_extranjero[datos_extranjero$año %in% años_con_sexo, ]
        
        # ========== ORDENAR TRAZAS DINÁMICAMENTE ==========
        # Obtener último valor de cada serie para determinar orden visual
        vals_padron_h <- datos_con_sexo$padron_extranjero_hombres[!is.na(datos_con_sexo$padron_extranjero_hombres)]
        ultimo_padron_h <- if(length(vals_padron_h) > 0) tail(vals_padron_h, 1) else 0
        
        vals_padron_m <- datos_con_sexo$padron_extranjero_mujeres[!is.na(datos_con_sexo$padron_extranjero_mujeres)]
        ultimo_padron_m <- if(length(vals_padron_m) > 0) tail(vals_padron_m, 1) else 0
        
        vals_lista_h <- datos_con_sexo$lista_extranjero_hombres[!is.na(datos_con_sexo$lista_extranjero_hombres)]
        ultimo_lista_h <- if(length(vals_lista_h) > 0) tail(vals_lista_h, 1) else 0
        
        vals_lista_m <- datos_con_sexo$lista_extranjero_mujeres[!is.na(datos_con_sexo$lista_extranjero_mujeres)]
        ultimo_lista_m <- if(length(vals_lista_m) > 0) tail(vals_lista_m, 1) else 0
        
        # Crear lista con metadatos de cada traza
        trazas_info <- data.frame(
          nombre = c("padron_h", "padron_m", "lista_h", "lista_m"),
          valor = c(ultimo_padron_h, ultimo_padron_m, ultimo_lista_h, ultimo_lista_m),
          stringsAsFactors = FALSE
        )
        
        # Ordenar de mayor a menor (orden visual de arriba a abajo)
        trazas_info <- trazas_info[order(trazas_info$valor, decreasing = TRUE), ]
        
        # Agregar trazas en el orden visual correcto
        for (i in 1:nrow(trazas_info)) {
          traza_nombre <- trazas_info$nombre[i]
          
          if (traza_nombre == "padron_h") {
            p <- p %>% add_trace(
              data = datos_con_sexo,
              x = ~año,
              y = ~padron_extranjero_hombres,
              type = 'scatter',
              mode = 'lines+markers',
              name = 'Padrón Hombres',
              line = list(color = '#D4A500', width = 2.5),
              marker = list(size = 8, color = '#D4A500'),
              hovertemplate = paste0('<b>%{x}</b><br>Padrón H: %{y:,.0f}<extra></extra>')
            )
          } else if (traza_nombre == "padron_m") {
            p <- p %>% add_trace(
              data = datos_con_sexo,
              x = ~año,
              y = ~padron_extranjero_mujeres,
              type = 'scatter',
              mode = 'lines+markers',
              name = 'Padrón Mujeres',
              line = list(color = '#F5CA45', width = 2.5),
              marker = list(size = 8, color = '#F5CA45'),
              hovertemplate = paste0('<b>%{x}</b><br>Padrón M: %{y:,.0f}<extra></extra>')
            )
          } else if (traza_nombre == "lista_h") {
            p <- p %>% add_trace(
              data = datos_con_sexo,
              x = ~año,
              y = ~lista_extranjero_hombres,
              type = 'scatter',
              mode = 'lines+markers',
              name = 'Lista Hombres',
              line = list(color = '#8FB369', width = 2.5, dash = 'dot'),
              marker = list(size = 8, color = '#8FB369', symbol = 'square'),
              hovertemplate = paste0('<b>%{x}</b><br>Lista H: %{y:,.0f}<extra></extra>')
            )
          } else if (traza_nombre == "lista_m") {
            p <- p %>% add_trace(
              data = datos_con_sexo,
              x = ~año,
              y = ~lista_extranjero_mujeres,
              type = 'scatter',
              mode = 'lines+markers',
              name = 'Lista Mujeres',
              line = list(color = '#CCE4B1', width = 2.5, dash = 'dot'),
              marker = list(size = 8, color = '#CCE4B1', symbol = 'square'),
              hovertemplate = paste0('<b>%{x}</b><br>Lista M: %{y:,.0f}<extra></extra>')
            )
          }
        }
      }
      
      # ========== PREPARAR TEXTO DE ANOTACIÓN ==========
      texto_nota <- ""
      if (length(años_sin_sexo) > 0) {
        if (length(años_sin_sexo) == 1) {
          texto_nota <- paste0("Nota: Año ", años_sin_sexo, " sin desglose por sexo (se muestran totales).")
        } else {
          texto_nota <- paste0("Nota: Años ", paste(años_sin_sexo, collapse = ", "), " sin desglose por sexo (se muestran totales).")
        }
      }
      
      # ========== LAYOUT CON ANOTACIÓN ==========
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
          x = 0.5, y = -0.45,
          xref = "paper", yref = "paper",
          xanchor = "center", yanchor = "top",
          showarrow = FALSE,
          font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
          align = "center"
        )
      )
      
      # Agregar nota si hay años sin sexo
      if (texto_nota != "") {
        annotations_list[[length(annotations_list) + 1]] <- list(
          text = texto_nota,
          x = 0.5, y = 1.05,
          xref = "paper", yref = "paper",
          xanchor = "center", yanchor = "top",
          showarrow = FALSE,
          font = list(size = 11, color = "#EAC43E", family = "Arial, sans-serif", style = "italic"),
          align = "center"
        )
      }
      
      # ✅ MANTENER: Card NB SÍ aplica en gráfica 3 (CON desglose por sexo cuando existe)
      if (length(años_con_sexo) > 0) {
        card_nb <- crear_card_no_binario(datos_extranjero, ambito = "extranjero", tipo_periodo = "anual")
        if (!is.null(card_nb)) {
          annotations_list[[length(annotations_list) + 1]] <- card_nb
        }
      }
      
      p <- p %>% layout(
        title = list(
          text = paste0("Evolución Anual por Sexo (2020-", anio_actual(), ") - Extranjero"),
          font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
          x = 0.5,
          xanchor = "center"
        ),
        xaxis = list(title = "", type = 'category'),
        yaxis = list(title = "Número de Electores", separatethousands = TRUE),
        legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25),
        margin = list(t = 130, b = 140, l = 90, r = 50),
        hovermode = 'x unified',
        annotations = annotations_list
      )
      
      message("✅ Gráfico 3: Evolución anual por sexo Extranjero (híbrido) renderizado (CON card NB cuando aplica)")
      return(p)
    }
  }) %>%
    # ✅ CORRECCIÓN v2.6: Agregar ambito_reactivo para cambio de vista automático
    bindEvent(
      estado_app(),
      input$btn_consultar,
      ambito_reactivo(),  # ✅ v2.6: AGREGADO para cambio de vista
      ignoreNULL = FALSE,
      ignoreInit = FALSE
    )
  
  message("✅ graficas_historico_3 v2.6 inicializado")
  message("   ✅ CORRECCIÓN: ambito_reactivo usado para cambio de vista automático")
}

