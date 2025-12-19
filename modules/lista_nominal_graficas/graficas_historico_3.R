# modules/lista_nominal_graficas/graficas_historico_3.R
# Gráfica 3: Evolución anual por sexo
# Versión: 2.2 - LIMPIEZA: Eliminados mensajes de "datos no disponibles", solo graficar

graficas_historico_3 <- function(input, output, session, datos_anuales_completos, 
                                 anio_actual, texto_alcance, estado_app, mostrar_graficas_anuales) {
  
  message("📊 Inicializando graficas_historico_3 v2.2")
  
  # ========== GRÁFICA 3: EVOLUCIÓN ANUAL + DESGLOSE POR SEXO ==========
  
  output$grafico_evolucion_anual_sexo <- renderPlotly({
    req(input$tipo_corte == "historico")
    req(input$ambito_datos)
    
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
    
    message("📊 [GRÁFICA 3] Renderizando - Estado: ", estado_app(), " | Ámbito: ", input$ambito_datos)
    
    # ========== ✅ ÚNICO MENSAJE: Sin datos disponibles ==========
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
    if (input$ambito_datos == "nacional") {
      
      # Verificar que existan columnas de sexo
      if (!all(c("padron_hombres", "padron_mujeres", "lista_hombres", "lista_mujeres") %in% colnames(datos_anuales))) {
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
      
      # Layout
      p <- p %>% layout(
        title = list(
          text = paste0("Evolución Anual por Sexo (2017-", anio_actual(), ") - Padrón y LNE Nacional"),
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
        annotations = list(
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
      )
      
      message("✅ Gráfico 3: Evolución anual por sexo Nacional renderizado")
      return(p)
      
    } else {
      # ========== GRÁFICA EXTRANJERO ==========
      
      # ✅ v2.2: ELIMINADO filtro de años >= 2020
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
      
      # Verificar si existen columnas de sexo
      cols_sexo_extranjero <- c("padron_extranjero_hombres", "padron_extranjero_mujeres", 
                                "lista_extranjero_hombres", "lista_extranjero_mujeres")
      
      tiene_columnas_sexo <- all(cols_sexo_extranjero %in% colnames(datos_extranjero))
      
      message("📊 [GRÁFICA 3 EXTRANJERO] ¿Tiene columnas de sexo?: ", tiene_columnas_sexo)
      
      # Crear gráfico
      p <- plot_ly()
      
      if (!tiene_columnas_sexo) {
        # ========== CASO 1: SIN COLUMNAS DE SEXO - MOSTRAR 2 LÍNEAS (TOTALES) ==========
        message("📊 [GRÁFICA 3] Mostrando totales (sin desglose por sexo)")
        
        # Padrón Total
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
            'Padrón: %{y:,.0f}<extra></extra>'
          )
        )
        
        # Lista Total
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
            'Lista: %{y:,.0f}<extra></extra>'
          )
        )
        
      } else {
        # ========== CASO 2: CON COLUMNAS DE SEXO - MOSTRAR 4 LÍNEAS CON ORDEN DINÁMICO ==========
        message("📊 [GRÁFICA 3] Mostrando desglose por sexo (4 líneas)")
        
        # Reemplazar NA con 0
        datos_extranjero$padron_extranjero_hombres[is.na(datos_extranjero$padron_extranjero_hombres)] <- 0
        datos_extranjero$padron_extranjero_mujeres[is.na(datos_extranjero$padron_extranjero_mujeres)] <- 0
        datos_extranjero$lista_extranjero_hombres[is.na(datos_extranjero$lista_extranjero_hombres)] <- 0
        datos_extranjero$lista_extranjero_mujeres[is.na(datos_extranjero$lista_extranjero_mujeres)] <- 0
        
        # Obtener último valor de cada serie para determinar orden visual
        vals_padron_h <- datos_extranjero$padron_extranjero_hombres[!is.na(datos_extranjero$padron_extranjero_hombres)]
        ultimo_padron_h <- if(length(vals_padron_h) > 0) tail(vals_padron_h, 1) else 0
        
        vals_padron_m <- datos_extranjero$padron_extranjero_mujeres[!is.na(datos_extranjero$padron_extranjero_mujeres)]
        ultimo_padron_m <- if(length(vals_padron_m) > 0) tail(vals_padron_m, 1) else 0
        
        vals_lista_h <- datos_extranjero$lista_extranjero_hombres[!is.na(datos_extranjero$lista_extranjero_hombres)]
        ultimo_lista_h <- if(length(vals_lista_h) > 0) tail(vals_lista_h, 1) else 0
        
        vals_lista_m <- datos_extranjero$lista_extranjero_mujeres[!is.na(datos_extranjero$lista_extranjero_mujeres)]
        ultimo_lista_m <- if(length(vals_lista_m) > 0) tail(vals_lista_m, 1) else 0
        
        # Crear lista con metadatos de cada traza
        trazas_info <- data.frame(
          nombre = c("padron_h", "padron_m", "lista_h", "lista_m"),
          valor = c(ultimo_padron_h, ultimo_padron_m, ultimo_lista_h, ultimo_lista_m),
          stringsAsFactors = FALSE
        )
        
        # Ordenar de mayor a menor (orden visual de arriba a abajo)
        trazas_info <- trazas_info[order(trazas_info$valor, decreasing = TRUE), ]
        
        message("📊 [GRÁFICA 3] Orden de trazas: ", paste(trazas_info$nombre, collapse = " > "))
        
        # Agregar trazas en el orden visual correcto
        for (i in 1:nrow(trazas_info)) {
          traza_nombre <- trazas_info$nombre[i]
          
          if (traza_nombre == "padron_h") {
            p <- p %>% add_trace(
              data = datos_extranjero,
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
              data = datos_extranjero,
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
              data = datos_extranjero,
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
              data = datos_extranjero,
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
      
      # Layout final
      p <- p %>% layout(
        title = list(
          text = paste0("Evolución Anual por Sexo (2017-", anio_actual(), ") - Residentes en el Extranjero"),
          font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
          x = 0.5,
          xanchor = "center"
        ),
        xaxis = list(title = "", type = 'category'),
        yaxis = list(title = "Número de Electores", separatethousands = TRUE),
        legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.20),
        margin = list(t = 120, b = 140, l = 90, r = 50),
        hovermode = 'x unified',
        annotations = list(
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
      )
      
      message("✅ Gráfico 3: Evolución anual por sexo Extranjero renderizado")
      return(p)
    }
    
  }) %>%
    bindEvent(
      estado_app(),
      input$btn_consultar,
      input$ambito_datos,
      ignoreNULL = FALSE,
      ignoreInit = FALSE
    )
  
  message("✅ graficas_historico_3 v2.2 inicializado (LIMPIEZA COMPLETA)")
}
