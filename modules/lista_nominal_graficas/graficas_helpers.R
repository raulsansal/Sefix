# modules/lista_nominal_graficas/graficas_helpers.R
# Funciones auxiliares para cálculos y proyecciones
# Versión: 2.2 - Card NB responsiva para móvil (60% reducción)

# ========== FUNCIÓN: GENERAR TEXTO DE ALCANCE ==========

generar_texto_alcance <- function(input) {
  
  partes <- c()
  
  entidad <- input$entidad %||% "Nacional"
  partes <- c(partes, paste0("Estado: ", entidad))
  
  distrito <- input$distrito %||% "Todos"
  partes <- c(partes, paste0("Distrito: ", distrito))
  
  municipio <- input$municipio %||% "Todos"
  partes <- c(partes, paste0("Municipio: ", municipio))
  
  seccion <- input$seccion
  if (is.null(seccion) || length(seccion) == 0) {
    partes <- c(partes, "Sección: Todas")
  } else if ("Todas" %in% seccion) {
    partes <- c(partes, "Sección: Todas")
  } else if (length(seccion) == 1) {
    partes <- c(partes, paste0("Sección: ", seccion))
  } else if (length(seccion) <= 5) {
    partes <- c(partes, paste0("Secciones: ", paste(seccion, collapse = ", ")))
  } else {
    partes <- c(partes, paste0("Secciones: ", length(seccion), " seleccionadas"))
  }
  
  texto <- paste(partes, collapse = " - ")
  
  message("📋 [generar_texto_alcance] ", texto)
  return(texto)
}

# ========== FUNCIÓN: CREAR CARD NO BINARIO - RESPONSIVA ==========
# ✅ v2.2: Acepta parámetro screen_width para ajustar tamaño

crear_card_no_binario <- function(datos, ambito = "nacional", tipo_periodo = "mensual", 
                                  año_consultado = NULL, screen_width = 1200) {
  
  resultado <- tryCatch({
    
    if (is.null(datos) || !is.data.frame(datos) || nrow(datos) == 0) {
      message("⚠️ [card_nb] Sin datos")
      return(NULL)
    }
    
    # ========== DETECTAR DISPOSITIVO ==========
    is_mobile <- screen_width <= 768
    is_tablet <- screen_width > 768 && screen_width <= 1024
    
    # ========== CONFIGURACIÓN SEGÚN DISPOSITIVO ==========
    if (is_mobile) {
      # Móvil: 60% reducción
      font_size_title <- 8
      font_size_content <- 6
      font_size_hint <- 5
      border_width <- 1.5
      border_pad <- 4
      tooltip_font_size <- 8
      pos_x <- 0.97
      pos_y <- 0.97
      x_anchor <- "right"
      etiqueta_padron <- "P"
      etiqueta_lista <- "L"
      show_hint <- FALSE  # No mostrar hint en móvil (muy pequeño)
    } else if (is_tablet) {
      # Tablet: 30% reducción
      font_size_title <- 10
      font_size_content <- 8
      font_size_hint <- 7
      border_width <- 2
      border_pad <- 6
      tooltip_font_size <- 9
      pos_x <- 0.08
      pos_y <- 0.92
      x_anchor <- "left"
      etiqueta_padron <- if(ambito == "nacional") "Padrón" else "Padrón Ext."
      etiqueta_lista <- if(ambito == "nacional") "Lista" else "Lista Ext."
      show_hint <- TRUE
    } else {
      # Desktop: tamaño normal
      font_size_title <- 12
      font_size_content <- 10
      font_size_hint <- 8
      border_width <- 2.5
      border_pad <- 8
      tooltip_font_size <- 11
      pos_x <- 0.08
      pos_y <- 0.92
      x_anchor <- "left"
      etiqueta_padron <- if(ambito == "nacional") "Padrón Nacional" else "Padrón Extranjero"
      etiqueta_lista <- if(ambito == "nacional") "Lista Nacional" else "Lista Extranjero"
      show_hint <- TRUE
    }
    
    # ========== DETECTAR COLUMNAS DISPONIBLES ==========
    cols_disponibles <- colnames(datos)
    
    if (ambito == "nacional") {
      posibles_padron <- c("padron_nacional_no_binario", "padron_no_binario", "padron_nb")
      posibles_lista <- c("lista_nacional_no_binario", "lista_no_binario", "lista_nb")
    } else {
      posibles_padron <- c("padron_extranjero_no_binario", "padron_no_binario", "padron_nb")
      posibles_lista <- c("lista_extranjero_no_binario", "lista_no_binario", "lista_nb")
    }
    
    col_padron <- NULL
    for (col in posibles_padron) {
      if (col %in% cols_disponibles) {
        col_padron <- col
        break
      }
    }
    
    col_lista <- NULL
    for (col in posibles_lista) {
      if (col %in% cols_disponibles) {
        col_lista <- col
        break
      }
    }
    
    if (is.null(col_padron) || is.null(col_lista)) {
      message("ℹ️ [card_nb] Columnas NB no disponibles para ", ambito)
      return(NULL)
    }
    
    # Reemplazar NA con 0
    datos[[col_padron]][is.na(datos[[col_padron]])] <- 0
    datos[[col_lista]][is.na(datos[[col_lista]])] <- 0
    
    # ========== CALCULAR TOTAL (ÚLTIMO PERÍODO CON DATOS) ==========
    datos_ordenados <- datos[order(datos$fecha), ]
    datos_con_casos <- datos_ordenados[datos_ordenados[[col_padron]] + datos_ordenados[[col_lista]] > 0, ]
    
    if (nrow(datos_con_casos) == 0) {
      message("ℹ️ [card_nb] Sin casos NB en ningún período")
      return(NULL)
    }
    
    ultima_fila <- datos_con_casos[nrow(datos_con_casos), ]
    total_padron <- ultima_fila[[col_padron]]
    total_lista <- ultima_fila[[col_lista]]
    
    # Período texto (formato según dispositivo)
    if (tipo_periodo == "mensual") {
      if (is_mobile) {
        periodo_texto <- format(ultima_fila$fecha, "%b")  # "Dic"
      } else {
        periodo_texto <- format(ultima_fila$fecha, "%B %Y")  # "Diciembre 2024"
      }
    } else {
      periodo_texto <- format(ultima_fila$fecha, "%Y")
    }
    
    message("📊 [card_nb] Dispositivo: ", 
            if(is_mobile) "MÓVIL" else if(is_tablet) "TABLET" else "DESKTOP",
            " | Período: ", periodo_texto, " | P:", total_padron, " L:", total_lista)
    
    # ========== PREPARAR DATOS PARA TOOLTIP ==========
    if (tipo_periodo == "mensual") {
      datos$periodo <- format(datos$fecha, "%b")
      datos$periodo_orden <- as.integer(format(datos$fecha, "%m"))
      fechas_unicas <- aggregate(fecha ~ periodo, data = datos, FUN = max)
      datos <- merge(datos, fechas_unicas, by = c("periodo", "fecha"))
    } else {
      datos$periodo <- as.character(datos$año)
      datos$periodo_orden <- as.integer(datos$año)
    }
    
    datos$padron_nb <- datos[[col_padron]]
    datos$lista_nb <- datos[[col_lista]]
    
    datos_con_casos <- datos[datos$padron_nb + datos$lista_nb > 0, ]
    
    if (nrow(datos_con_casos) == 0) {
      return(NULL)
    }
    
    datos_con_casos <- datos_con_casos[order(datos_con_casos$periodo_orden), ]
    
    # ========== CONSTRUIR TEXTO DE CARD SEGÚN DISPOSITIVO ==========
    
    if (is_mobile) {
      # Card ultra-compacta para móvil
      texto_card <- paste0(
        "<span style='color:#9B59B6; font-size:", font_size_title, "px; font-weight:bold'>⚧</span><br>",
        "<span style='font-size:", font_size_content, "px'>",
        etiqueta_padron, ":", total_padron, "<br>",
        etiqueta_lista, ":", total_lista,
        "</span>"
      )
    } else {
      # Card normal para tablet/desktop
      hint_text <- if(show_hint) {
        paste0("<br><span style='font-size:", font_size_hint, "px; color:#888; font-style:italic'>",
               "(Hover para desglose)</span>")
      } else ""
      
      texto_card <- paste0(
        "<b style='color:#9B59B6; font-size:", font_size_title, "px'>⚧ No Binario</b><br>",
        "<span style='font-size:", font_size_content, "px; line-height:1.4'>",
        "<b>", periodo_texto, "</b><br>",
        etiqueta_padron, ": ", total_padron, "<br>",
        etiqueta_lista, ": ", total_lista,
        hint_text,
        "</span>"
      )
    }
    
    # ========== CONSTRUIR TEXTO DE TOOLTIP ==========
    
    desglose <- datos_con_casos[, c("periodo", "padron_nb", "lista_nb")]
    filas_tabla <- c()
    
    for (i in 1:nrow(desglose)) {
      periodo <- desglose$periodo[i]
      padron <- desglose$padron_nb[i]
      lista <- desglose$lista_nb[i]
      
      if (is_mobile) {
        fila <- sprintf("%s P:%d L:%d", periodo, padron, lista)
      } else if (tipo_periodo == "mensual") {
        fila <- sprintf("%-7s %4d  %4d", periodo, padron, lista)
      } else {
        fila <- sprintf("%4s  %4d  %4d", periodo, padron, lista)
      }
      filas_tabla <- c(filas_tabla, fila)
    }
    
    tabla_texto <- paste(filas_tabla, collapse = "\n")
    
    if (is_mobile) {
      texto_tooltip <- paste0(
        "No Binario\n",
        tabla_texto, "\n",
        "(Tap para cerrar)"
      )
    } else {
      encabezado_tooltip <- if(tipo_periodo == "mensual") "Desglose mensual" else "Desglose anual"
      header_tabla <- if(tipo_periodo == "mensual") "Mes     Padrón  Lista" else "Año   Padrón  Lista"
      
      texto_tooltip <- paste0(
        encabezado_tooltip, "\n",
        strrep("─", 22), "\n",
        header_tabla, "\n",
        tabla_texto
      )
    }
    
    # ========== CREAR ANNOTATION ==========
    
    annotation <- list(
      text = texto_card,
      hovertext = texto_tooltip,
      hoverlabel = list(
        bgcolor = "rgba(255, 255, 255, 0.98)",
        bordercolor = "#9B59B6",
        font = list(
          family = if(is_mobile) "Arial, sans-serif" else "Courier New, monospace",
          size = tooltip_font_size,
          color = "#333"
        )
      ),
      x = pos_x,
      y = pos_y,
      xref = "paper",
      yref = "paper",
      xanchor = x_anchor,
      yanchor = "top",
      showarrow = FALSE,
      bgcolor = "rgba(255, 255, 255, 0.92)",
      bordercolor = "#9B59B6",
      borderwidth = border_width,
      borderpad = border_pad,
      font = list(
        size = font_size_content,
        color = "#333",
        family = "Arial, sans-serif"
      ),
      captureevents = TRUE  # Permite capturar clicks en móvil
    )
    
    message("✅ [card_nb] Card creada para ", 
            if(is_mobile) "MÓVIL" else if(is_tablet) "TABLET" else "DESKTOP")
    return(annotation)
    
  }, error = function(e) {
    message("❌ [card_nb] Error creando card: ", e$message)
    return(NULL)
  })
  
  return(resultado)
}

# ========== FUNCIÓN: PROYECCIÓN CON TASA DE CRECIMIENTO ==========

proyectar_con_tasa_crecimiento <- function(datos, meses_proyectar = 5, usar_columnas_separadas = FALSE) {
  
  if (is.null(datos) || nrow(datos) < 2) {
    message("⚠️ [proyectar_con_tasa_crecimiento] Datos insuficientes para proyectar")
    return(NULL)
  }
  
  n <- nrow(datos)
  
  if (usar_columnas_separadas) {
    valor_inicial <- datos$lista_nacional[1]
    valor_final <- datos$lista_nacional[n]
    padron_inicial <- datos$padron_nacional[1]
    padron_final <- datos$padron_nacional[n]
  } else {
    valor_inicial <- datos$lista_nominal[1]
    valor_final <- datos$lista_nominal[n]
    padron_inicial <- datos$padron_electoral[1]
    padron_final <- datos$padron_electoral[n]
  }
  
  if (valor_inicial == 0 || is.na(valor_inicial) || is.na(valor_final)) {
    message("⚠️ [proyectar_con_tasa_crecimiento] Valores inválidos para proyectar")
    return(NULL)
  }
  
  if (padron_inicial == 0 || is.na(padron_inicial) || is.na(padron_final)) {
    message("⚠️ [proyectar_con_tasa_crecimiento] Valores de padrón inválidos")
    return(NULL)
  }
  
  tasa_mensual_lista <- ((valor_final / valor_inicial) ^ (1 / (n - 1))) - 1
  tasa_mensual_padron <- ((padron_final / padron_inicial) ^ (1 / (n - 1))) - 1
  
  message("📊 [proyectar_con_tasa_crecimiento] Tasa mensual lista: ", round(tasa_mensual_lista * 100, 4), "%")
  message("📊 [proyectar_con_tasa_crecimiento] Tasa mensual padrón: ", round(tasa_mensual_padron * 100, 4), "%")
  
  ultima_fecha <- max(datos$fecha)
  anio_base <- as.integer(format(ultima_fecha, "%Y"))
  mes_base <- as.integer(format(ultima_fecha, "%m"))
  
  message("📅 [proyectar_con_tasa_crecimiento] Última fecha base: ", ultima_fecha, " (", mes_base, "/", anio_base, ")")
  
  fechas_proyectadas <- list()
  
  for (i in 1:meses_proyectar) {
    mes_proyectado <- mes_base + i
    anio_proyectado <- anio_base
    
    if (mes_proyectado > 12) {
      anio_proyectado <- anio_base + floor((mes_proyectado - 1) / 12)
      mes_proyectado <- ((mes_proyectado - 1) %% 12) + 1
    }
    
    if (mes_proyectado == 12) {
      ultimo_dia <- as.Date(paste0(anio_proyectado + 1, "-01-01")) - 1
    } else {
      ultimo_dia <- as.Date(paste0(anio_proyectado, "-", sprintf("%02d", mes_proyectado + 1), "-01")) - 1
    }
    
    fechas_proyectadas[[i]] <- ultimo_dia
  }
  
  fechas_proyectadas <- do.call(c, fechas_proyectadas)
  
  message("📅 [proyectar_con_tasa_crecimiento] Fechas proyectadas: ", paste(fechas_proyectadas, collapse = ", "))
  
  lista_proyectada <- numeric(meses_proyectar)
  padron_proyectado <- numeric(meses_proyectar)
  
  for (i in 1:meses_proyectar) {
    lista_proyectada[i] <- valor_final * ((1 + tasa_mensual_lista) ^ i)
    padron_proyectado[i] <- padron_final * ((1 + tasa_mensual_padron) ^ i)
  }
  
  proyecciones <- data.frame(
    fecha = fechas_proyectadas,
    lista_proyectada = lista_proyectada,
    padron_proyectado = padron_proyectado,
    tipo = "Proyección",
    stringsAsFactors = FALSE
  )
  
  message("✅ [proyectar_con_tasa_crecimiento] Proyección calculada: ", nrow(proyecciones), " meses")
  
  return(proyecciones)
}

# ========== FUNCIÓN: ORDENAR TRAZAS POR VALOR FINAL ==========

ordenar_trazas_por_valor <- function(datos, cols_valores) {
  
  if (is.null(datos) || nrow(datos) == 0 || length(cols_valores) == 0) {
    return(NULL)
  }
  
  valores_finales <- sapply(cols_valores, function(col) {
    if (col %in% colnames(datos)) {
      vals <- datos[[col]][!is.na(datos[[col]])]
      if (length(vals) > 0) {
        return(tail(vals, 1))
      }
    }
    return(0)
  })
  
  orden_df <- data.frame(
    columna = cols_valores,
    valor_final = valores_finales,
    stringsAsFactors = FALSE
  )
  
  orden_df <- orden_df[order(orden_df$valor_final, decreasing = TRUE), ]
  
  return(orden_df$columna)
}

# ========== FUNCIÓN: VALIDAR EXISTENCIA DE COLUMNAS ==========

validar_columnas <- function(datos, columnas_requeridas) {
  
  if (is.null(datos) || !is.data.frame(datos)) {
    return(FALSE)
  }
  
  columnas_faltantes <- setdiff(columnas_requeridas, colnames(datos))
  
  if (length(columnas_faltantes) > 0) {
    message("⚠️ [validar_columnas] Columnas faltantes: ", paste(columnas_faltantes, collapse = ", "))
    return(FALSE)
  }
  
  return(TRUE)
}

# ========== FUNCIÓN: VERIFICAR DATOS VÁLIDOS EN COLUMNAS ==========

tiene_datos_validos <- function(datos, columnas) {
  
  if (is.null(datos) || !is.data.frame(datos) || nrow(datos) == 0) {
    return(FALSE)
  }
  
  for (col in columnas) {
    if (col %in% colnames(datos)) {
      if (any(!is.na(datos[[col]]))) {
        return(TRUE)
      }
    }
  }
  
  return(FALSE)
}

message("✅ graficas_helpers v2.2 cargado")
message("   ✅ Card NB responsiva según screen_width")
message("   ✅ Móvil: 60% reducción, posición esquina superior derecha")
message("   ✅ Tablet: 30% reducción")
message("   ✅ Desktop: tamaño normal")
