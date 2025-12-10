# modules/lista_nominal_graficas/graficas_semanal.R
# Gráficas para datos semanales: Barras y Tasa de inclusión
# Versión: 1.0 - Modularizada

graficas_semanal <- function(input, output, session, datos_columnas, combinacion_valida, texto_alcance) {
  
  message("📊 Inicializando graficas_semanal")
  
  # ========== CONTENEDOR UI PARA GRÁFICO PRINCIPAL ==========
  
  output$`main-plot_container` <- renderUI({
    plotlyOutput(session$ns("main-grafico_barras"), width = "100%", height = "450px")
  })
  
  # ========== GRÁFICO PRINCIPAL SEMANALES (BARRAS) ==========
  
  output$`main-grafico_barras` <- renderPlotly({
    req(input$tipo_corte == "semanal")
    req(input$ambito_datos)
    req(combinacion_valida())
    
    datos <- datos_columnas()
    
    if (is.null(datos) || is.null(datos$datos) || nrow(datos$datos) == 0) {
      p <- plot_ly() %>%
        layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(
            list(
              text = "No hay datos disponibles con los filtros seleccionados",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5,
              xanchor = "center", yanchor = "middle",
              showarrow = FALSE,
              font = list(size = 16, color = "#666")
            )
          )
        )
      return(p)
    }
    
    df <- datos$datos
    desglose_actual <- isolate(input$desglose) %||% "Sexo"
    
    message("📊 Renderizando gráfico semanal: ", desglose_actual, " - Ámbito: ", input$ambito_datos)
    
    if (input$ambito_datos == "nacional") {
      col_padron <- "padron_nacional"
      col_lista <- "lista_nacional"
      titulo_base <- "Nacional"
    } else {
      col_padron <- "padron_extranjero"
      col_lista <- "lista_extranjero"
      titulo_base <- "Extranjero"
      
      if (!col_padron %in% colnames(df) || !col_lista %in% colnames(df)) {
        return(plot_ly() %>%
                 layout(
                   xaxis = list(visible = FALSE),
                   yaxis = list(visible = FALSE),
                   annotations = list(
                     list(
                       text = "Datos de extranjero no disponibles para este corte",
                       xref = "paper", yref = "paper",
                       x = 0.5, y = 0.5,
                       showarrow = FALSE,
                       font = list(size = 14, color = "#666")
                     )
                   )
                 ))
      }
    }
    
    # ========== DESGLOSE POR SEXO ==========
    if (desglose_actual == "Sexo") {
      
      if (input$ambito_datos == "nacional") {
        cols_sexo <- c("padron_nacional_hombres", "padron_nacional_mujeres", 
                       "lista_nacional_hombres", "lista_nacional_mujeres")
      } else {
        return(plot_ly() %>%
                 layout(
                   xaxis = list(visible = FALSE),
                   yaxis = list(visible = FALSE),
                   annotations = list(
                     list(
                       text = "Desglose por sexo no disponible para ámbito Extranjero",
                       xref = "paper", yref = "paper",
                       x = 0.5, y = 0.5,
                       showarrow = FALSE,
                       font = list(size = 14, color = "#666")
                     )
                   )
                 ))
      }
      
      if (all(cols_sexo %in% colnames(df))) {
        padron_h <- sum(df$padron_nacional_hombres, na.rm = TRUE)
        padron_m <- sum(df$padron_nacional_mujeres, na.rm = TRUE)
        lista_h <- sum(df$lista_nacional_hombres, na.rm = TRUE)
        lista_m <- sum(df$lista_nacional_mujeres, na.rm = TRUE)
        
        datos_grafico <- data.frame(
          Categoria = rep(c("Hombres", "Mujeres"), 2),
          Tipo = rep(c("Padrón Electoral", "Lista Nominal"), each = 2),
          Cantidad = c(padron_h, padron_m, lista_h, lista_m),
          stringsAsFactors = FALSE
        )
        
        p <- plot_ly(
          data = datos_grafico,
          x = ~Categoria,
          y = ~Cantidad,
          color = ~Tipo,
          type = 'bar',
          colors = c("#44559B", "#C0311A"),
          text = ~paste0(format(Cantidad, big.mark = ","), " electores"),
          hovertemplate = '<b>%{x}</b><br>%{text}<extra></extra>'
        ) %>%
          layout(
            title = list(
              text = paste0("Padrón Electoral y Lista Nominal por Sexo - ", titulo_base),
              font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
              x = 0.5, xanchor = "center"
            ),
            xaxis = list(title = ""),
            yaxis = list(title = "Número de Electores", separatethousands = TRUE),
            barmode = 'group',
            margin = list(t = 120, b = 140, l = 90, r = 50),
            legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.15),
            annotations = list(
              list(
                text = texto_alcance(),
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
                xanchor = "left", yanchor = "top",
                showarrow = FALSE,
                font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
                align = "left"
              )
            )
          )
        
      } else {
        total_padron <- sum(df[[col_padron]], na.rm = TRUE)
        total_lista <- sum(df[[col_lista]], na.rm = TRUE)
        
        datos_grafico <- data.frame(
          Tipo = c("Padrón Electoral", "Lista Nominal"),
          Cantidad = c(total_padron, total_lista),
          stringsAsFactors = FALSE
        )
        
        colores <- if (input$ambito_datos == "nacional") {
          c("#003E66", "#AE0E35")
        } else {
          c("#EAC43E", "#B3D491")
        }
        
        p <- plot_ly(
          data = datos_grafico,
          x = ~Tipo,
          y = ~Cantidad,
          type = 'bar',
          marker = list(color = colores),
          text = ~paste0(format(Cantidad, big.mark = ","), " electores"),
          hovertemplate = '<b>%{x}</b><br>%{text}<extra></extra>'
        ) %>%
          layout(
            title = list(
              text = paste0("Padrón Electoral y Lista Nominal - ", titulo_base),
              font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
              x = 0.5, xanchor = "center"
            ),
            xaxis = list(title = ""),
            yaxis = list(title = "Número de Electores", separatethousands = TRUE),
            margin = list(t = 120, b = 140, l = 90, r = 50),
            annotations = list(
              list(
                text = texto_alcance(),
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
                xanchor = "left", yanchor = "top",
                showarrow = FALSE,
                font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
                align = "left"
              )
            )
          )
      }
      
      # ========== DESGLOSE POR RANGO DE EDAD ==========
    } else if (desglose_actual == "Rango de Edad") {
      
      cols_edad_lista <- grep("^lista_(\\d+|\\d+_\\d+)", colnames(df), value = TRUE, ignore.case = TRUE)
      
      if (length(cols_edad_lista) > 0) {
        
        grupos_raw <- gsub("lista_", "", cols_edad_lista, ignore.case = TRUE)
        grupos_raw <- gsub("_(hombres|mujeres|nobinario).*", "", grupos_raw, ignore.case = TRUE)
        grupos <- unique(grupos_raw)
        
        datos_grafico <- data.frame(
          Grupo = character(),
          Lista_Nominal = numeric(),
          stringsAsFactors = FALSE
        )
        
        for (grupo in grupos) {
          cols_grupo <- grep(paste0("^lista_", grupo, "($|_)"), colnames(df), value = TRUE, ignore.case = TRUE)
          total <- sum(df[, cols_grupo, drop = FALSE], na.rm = TRUE)
          nombre_grupo <- gsub("_", "-", grupo)
          nombre_grupo <- gsub("y-mas", "y más", nombre_grupo, ignore.case = TRUE)
          
          datos_grafico <- rbind(
            datos_grafico, 
            data.frame(
              Grupo = nombre_grupo,
              Lista_Nominal = total,
              stringsAsFactors = FALSE
            )
          )
        }
        
        orden_edad <- c("18", "19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                        "45-49", "50-54", "55-59", "60-64", "65-y-más", "65-y-mas")
        datos_grafico$Grupo <- factor(
          datos_grafico$Grupo, 
          levels = intersect(orden_edad, datos_grafico$Grupo)
        )
        datos_grafico <- datos_grafico[order(datos_grafico$Grupo), ]
        
        color_edad <- if (input$ambito_datos == "nacional") "#C0311A" else "#B3D491"
        
        p <- plot_ly(
          data = datos_grafico,
          x = ~Grupo,
          y = ~Lista_Nominal,
          type = 'bar',
          marker = list(color = color_edad),
          text = ~paste0(format(Lista_Nominal, big.mark = ","), " electores"),
          hovertemplate = '<b>%{x}</b><br>%{text}<extra></extra>'
        ) %>%
          layout(
            title = list(text = paste0("Lista Nominal por Grupo de Edad - ", titulo_base),
                         font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
                         x = 0.5,
                         xanchor = "center"
            ),
            xaxis = list(title = "Grupo de Edad"),
            yaxis = list(
              title = "Número de Electores",
              separatethousands = TRUE
            ),
            margin = list(t = 120, b = 140, l = 90, r = 50),
            annotations = list(
              list(
                text = texto_alcance(),
                x = 0.5, y = 1.12,
                xref = "paper", yref = "paper",
                xanchor = "center", yanchor = "top",
                showarrow = FALSE,
                font = list(size = 13, color = "#555555", family = "Arial, sans-serif"),
                align = "center"
              ),
              list(
                text = "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado",
                x = 0.0, y = -0.25,
                xref = "paper", yref = "paper",
                xanchor = "left", yanchor = "top",
                showarrow = FALSE,
                font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
                align = "left"
              )
            )
          )
        
      } else {
        p <- plot_ly() %>%
          layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              list(
                text = "Datos de edad no disponibles para este corte",
                xref = "paper", yref = "paper",
                x = 0.5, y = 0.5,
                showarrow = FALSE,
                font = list(size = 14, color = "#666")
              )
            )
          )
      }
      
      # ========== DESGLOSE POR ENTIDAD DE ORIGEN ==========
    } else if (desglose_actual == "Entidad de Origen") {
      
      if ("nombre_entidad" %in% colnames(df) && col_lista %in% colnames(df)) {
        
        datos_grafico <- df %>%
          group_by(Entidad = nombre_entidad) %>%
          summarise(
            Lista_Nominal = sum(.data[[col_lista]], na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          arrange(desc(Lista_Nominal)) %>%
          head(10)
        
        datos_grafico <- as.data.frame(datos_grafico)
        
        color_entidad <- if (input$ambito_datos == "nacional") "#44559B" else "#EAC43E"
        
        p <- plot_ly(
          data = datos_grafico,
          y = ~reorder(Entidad, Lista_Nominal),
          x = ~Lista_Nominal,
          type = 'bar',
          orientation = 'h',
          marker = list(color = color_entidad),
          text = ~paste0(format(Lista_Nominal, big.mark = ","), " electores"),
          hovertemplate = '<b>%{y}</b><br>%{text}<extra></extra>'
        ) %>%
          layout(
            title = list(
              text = paste0("Top 10 Entidades por Lista Nominal - ", titulo_base),
              font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
              x = 0.5,
              xanchor = "center"
            ),
            xaxis = list(
              title = "Número de Electores",
              separatethousands = TRUE
            ),
            yaxis = list(title = ""),
            margin = list(t = 120, b = 140, l = 180, r = 50),
            annotations = list(
              list(
                text = texto_alcance(),
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
                xanchor = "left", yanchor = "top",
                showarrow = FALSE,
                font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
                align = "left"
              )
            )
          )
        
      } else {
        p <- plot_ly() %>%
          layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              list(
                text = "Datos de origen no disponibles para este corte",
                xref = "paper", yref = "paper",
                x = 0.5, y = 0.5,
                showarrow = FALSE,
                font = list(size = 14, color = "#666")
              )
            )
          )
      }
      
    } else {
      p <- plot_ly() %>%
        layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(
            list(
              text = "Tipo de desglose no reconocido",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5,
              showarrow = FALSE,
              font = list(size = 14, color = "#666")
            )
          )
        )
    }
    
    message("✅ Gráfico semanal renderizado: ", desglose_actual, " - ", titulo_base)
    return(p)
  })
  
  # ========== GRÁFICO DE TASA DE INCLUSIÓN (SOLO SEMANALES) ==========
  
  output$`main-tasa_inclusion_plot` <- renderPlotly({
    req(input$tipo_corte == "semanal")
    req(input$ambito_datos)
    req(combinacion_valida())
    
    datos <- datos_columnas()
    
    if (is.null(datos) || is.null(datos$datos) || nrow(datos$datos) == 0) {
      return(NULL)
    }
    
    df <- datos$datos
    
    if (input$ambito_datos == "nacional") {
      col_padron <- "padron_nacional"
      col_lista <- "lista_nacional"
      titulo_ambito <- "Nacional"
      color_lista <- "#4CAF50"
      color_diferencia <- "#FFC107"
    } else {
      col_padron <- "padron_extranjero"
      col_lista <- "lista_extranjero"
      titulo_ambito <- "Extranjero"
      color_lista <- "#8BC34A"
      color_diferencia <- "#FFB74D"
      
      if (!col_padron %in% colnames(df) || !col_lista %in% colnames(df)) {
        return(plot_ly() %>%
                 layout(
                   xaxis = list(visible = FALSE),
                   yaxis = list(visible = FALSE),
                   annotations = list(
                     list(
                       text = "Datos de extranjero no disponibles para este corte",
                       xref = "paper", yref = "paper",
                       x = 0.5, y = 0.5,
                       showarrow = FALSE,
                       font = list(size = 14, color = "#666")
                     )
                   )
                 ))
      }
    }
    
    total_padron <- sum(df[[col_padron]], na.rm = TRUE)
    total_lista <- sum(df[[col_lista]], na.rm = TRUE)
    
    if (total_padron == 0) {
      return(NULL)
    }
    
    tasa_inclusion <- round((total_lista / total_padron) * 100, 2)
    tasa_exclusion <- round(100 - tasa_inclusion, 2)
    
    datos_grafico <- data.frame(
      grupo = c(
        paste0("Lista Nominal:<br>", sprintf("%.2f%%", tasa_inclusion)),
        sprintf("Diferencia: %.2f%%", tasa_exclusion)
      ),
      valor = c(tasa_inclusion, tasa_exclusion),
      stringsAsFactors = FALSE
    )
    
    p <- plot_ly(
      data = datos_grafico,
      values = ~valor,
      labels = ~grupo,
      type = "pie",
      hole = 0.6,
      textinfo = "label",
      textposition = "outside",
      textfont = list(
        color = c(color_lista, color_diferencia),
        size = 14
      ),
      marker = list(colors = c(color_lista, color_diferencia)),
      showlegend = FALSE,
      hoverinfo = "none"
    ) %>%
      layout(
        title = list(
          text = paste0("Tasa de Inclusión en Lista Nominal - ", titulo_ambito),
          x = 0.5,
          xanchor = "center",
          y = 0.95,
          yanchor = "top",
          font = list(size = 20, color = "black", family = "Arial, sans-serif")
        ),
        annotations = list(
          list(
            text = paste0("Padrón Total: ", format(total_padron, big.mark = ",")),
            x = 0.5,
            xref = "paper",
            y = 1.15,
            yref = "paper",
            xanchor = "center",
            yanchor = "top",
            showarrow = FALSE,
            font = list(size = 16, color = "black", family = "Arial, sans-serif")
          ),
          list(
            text = texto_alcance(),
            x = 0.5,
            xref = "paper",
            y = 1.05,
            yref = "paper",
            xanchor = "center",
            yanchor = "top",
            showarrow = FALSE,
            font = list(size = 13, color = "#555555", family = "Arial, sans-serif"),
            align = "center"
          ),
          list(
            text = "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado",
            xref = "paper", yref = "paper",
            x = 0.5, y = -0.35,
            font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
            showarrow = FALSE,
            align = "left"
          )
        ),
        margin = list(t = 120, b = 140, l = 50, r = 50),
        showlegend = FALSE
      )
    
    message("✅ Gráfico de tasa de inclusión renderizado - ", titulo_ambito)
    return(p)
  })
  
  message("✅ graficas_semanal inicializado")
}