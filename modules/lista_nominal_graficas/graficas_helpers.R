# modules/lista_nominal_graficas/graficas_helpers.R
# Funciones auxiliares para cálculos y proyecciones
# Versión: 2.5 - Card NB corregida para móvil + texto alcance reposicionado
# Cambios v2.5:
#   - CORREGIDO: Error de sintaxis en texto_card de desktop
#   - CORREGIDO: Detección de screen_width más robusta
#   - Card móvil rectangular (reducida 50%) con line-height 1.5
#   - Hint correcto "(Clic para desglose)" en móvil
#   - Texto alcance subido 20px (y=1.15 en lugar de y=1.12)

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

# ========== FUNCIÓN: CREAR CARD NO BINARIO - v2.5 CORREGIDA ==========

crear_card_no_binario <- function(datos, ambito = "nacional", tipo_periodo = "mensual", 
                                  año_consultado = NULL, screen_width = NULL) {
  
  resultado <- tryCatch({
    
    if (is.null(datos) || !is.data.frame(datos) || nrow(datos) == 0) {
      message("⚠️ [card_nb] Sin datos")
      return(NULL)
    }
    
    # ========== DETECTAR DISPOSITIVO - MEJORADO ==========
    # Si screen_width es NULL o no es numérico, asumir desktop
    if (is.null(screen_width) || !is.numeric(screen_width) || length(screen_width) == 0) {
      screen_width <- 1200
      message("⚠️ [card_nb] screen_width no disponible, usando default: 1200")
    }
    
    is_mobile <- screen_width <= 768
    is_tablet <- screen_width > 768 && screen_width <= 1024
    
    message("📱 [card_nb] screen_width=", screen_width, " | Dispositivo: ", 
            if(is_mobile) "MÓVIL" else if(is_tablet) "TABLET" else "DESKTOP",
            " | Ámbito: ", ambito)
    
    # ========== DETECTAR COLUMNAS DISPONIBLES ==========
    cols_disponibles <- colnames(datos)
    message("📋 [card_nb] Columnas disponibles: ", paste(head(cols_disponibles, 10), collapse = ", "), "...")
    
    if (ambito == "nacional") {
      posibles_padron <- c("padron_nacional_no_binario", "padron_no_binario", "padron_nb")
      posibles_lista <- c("lista_nacional_no_binario", "lista_no_binario", "lista_nb")
      etiqueta_padron_full <- "Padrón Nacional"
      etiqueta_lista_full <- "Lista Nacional"
      etiqueta_padron_short <- "Padrón Nac."
      etiqueta_lista_short <- "Lista Nac."
    } else {
      posibles_padron <- c("padron_extranjero_no_binario", "padron_ext_no_binario", "padron_no_binario", "padron_nb")
      posibles_lista <- c("lista_extranjero_no_binario", "lista_ext_no_binario", "lista_no_binario", "lista_nb")
      etiqueta_padron_full <- "Padrón Extranjero"
      etiqueta_lista_full <- "Lista Extranjero"
      etiqueta_padron_short <- "Padrón Ext."
      etiqueta_lista_short <- "Lista Ext."
    }
    
    col_padron <- NULL
    for (col in posibles_padron) {
      if (col %in% cols_disponibles) {
        col_padron <- col
        message("✅ [card_nb] Columna padrón encontrada: ", col)
        break
      }
    }
    
    col_lista <- NULL
    for (col in posibles_lista) {
      if (col %in% cols_disponibles) {
        col_lista <- col
        message("✅ [card_nb] Columna lista encontrada: ", col)
        break
      }
    }
    
    if (is.null(col_padron) || is.null(col_lista)) {
      message("ℹ️ [card_nb] Columnas NB no disponibles para ", ambito)
      message("   Buscadas padrón: ", paste(posibles_padron, collapse = ", "))
      message("   Buscadas lista: ", paste(posibles_lista, collapse = ", "))
      return(NULL)
    }
    
    # Reemplazar NA con 0
    datos[[col_padron]][is.na(datos[[col_padron]])] <- 0
    datos[[col_lista]][is.na(datos[[col_lista]])] <- 0
    
    # ========== CALCULAR TOTAL (ÚLTIMO PERÍODO CON DATOS) ==========
    datos_ordenados <- datos[order(datos$fecha), ]
    datos_con_casos <- datos_ordenados[datos_ordenados[[col_padron]] + datos_ordenados[[col_lista]] > 0, ]
    
    if (nrow(datos_con_casos) == 0) {
      message("ℹ️ [card_nb] Sin casos NB en ningún período para ", ambito)
      return(NULL)
    }
    
    ultima_fila <- datos_con_casos[nrow(datos_con_casos), ]
    total_padron <- ultima_fila[[col_padron]]
    total_lista <- ultima_fila[[col_lista]]
    
    # Período texto
    periodo_texto <- format(ultima_fila$fecha, "%Y")
    
    message("📊 [card_nb] Período: ", periodo_texto, " | Padrón: ", total_padron, " | Lista: ", total_lista)
    
    # ========== PREPARAR DATOS PARA TOOLTIP DE DESGLOSE ==========
    if (tipo_periodo == "mensual") {
      datos$periodo <- format(datos$fecha, "%b")
      datos$periodo_orden <- as.integer(format(datos$fecha, "%m"))
    } else {
      datos$periodo <- as.character(datos$año)
      datos$periodo_orden <- as.integer(datos$año)
    }
    
    datos$padron_nb <- datos[[col_padron]]
    datos$lista_nb <- datos[[col_lista]]
    
    datos_con_casos_full <- datos[datos$padron_nb + datos$lista_nb > 0, ]
    datos_con_casos_full <- datos_con_casos_full[order(datos_con_casos_full$periodo_orden), ]
    
    # ========== CONSTRUIR TOOLTIP (igual para todos los dispositivos) ==========
    texto_tooltip <- construir_tooltip_tabla(datos_con_casos_full, tipo_periodo, ambito)
    
    # ========== CONFIGURACIÓN SEGÚN DISPOSITIVO ==========
    
    if (is_mobile) {
      # ============================================================
      # MÓVIL: Card compacta rectangular (~50% más pequeña)
      # ============================================================
      message("🔧 [card_nb] Aplicando configuración MÓVIL")
      
      # Tamaños de fuente MÁS PEQUEÑOS
      font_size_icon <- 8
      font_size_title <- 7
      font_size_periodo <- 6
      font_size_datos <- 6
      font_size_hint <- 5
      
      # Diseño compacto rectangular
      border_width <- 1
      border_pad <- 2
      
      # Posición: esquina superior IZQUIERDA
      pos_x <- 0.02
      pos_y <- 0.98
      x_anchor <- "left"
      y_anchor <- "top"
      
      # Etiquetas según ámbito
      etiqueta_padron <- etiqueta_padron_short
      etiqueta_lista <- etiqueta_lista_short
      
      # ✅ v2.5: Hint correcto para móvil
      hint_text <- "(Clic para desglose)"
      
      # Construir HTML con line-height 1.5
      texto_card <- paste0(
        "<span style='font-size:", font_size_icon, "px; color:#9B59B6; line-height:1.5;'>⚧</span>",
        "<span style='font-size:", font_size_title, "px; font-weight:bold; color:#9B59B6; line-height:1.5;'> No Binario</span><br>",
        "<span style='font-size:", font_size_periodo, "px; color:#555; line-height:1.5;'>", periodo_texto, "</span><br>",
        "<span style='font-size:", font_size_datos, "px; color:#333; line-height:1.5;'>", etiqueta_padron, ": ", total_padron, "</span><br>",
        "<span style='font-size:", font_size_datos, "px; color:#333; line-height:1.5;'>", etiqueta_lista, ": ", total_lista, "</span><br>",
        "<span style='font-size:", font_size_hint, "px; color:#888; font-style:italic; line-height:1.5;'>", hint_text, "</span>"
      )
      
      tooltip_font_size <- 8
      
    } else if (is_tablet) {
      # ============================================================
      # TABLET: Card mediana
      # ============================================================
      message("🔧 [card_nb] Aplicando configuración TABLET")
      
      font_size_icon <- 11
      font_size_title <- 9
      font_size_periodo <- 8
      font_size_datos <- 8
      font_size_hint <- 7
      
      border_width <- 1.5
      border_pad <- 4
      
      pos_x <- 0.03
      pos_y <- 0.97
      x_anchor <- "left"
      y_anchor <- "top"
      
      etiqueta_padron <- etiqueta_padron_full
      etiqueta_lista <- etiqueta_lista_full
      hint_text <- "(Clic para desglose)"
      
      texto_card <- paste0(
        "<span style='font-size:", font_size_icon, "px; color:#9B59B6;'>⚧</span> ",
        "<span style='font-size:", font_size_title, "px; font-weight:bold; color:#9B59B6;'>No Binario</span><br>",
        "<span style='font-size:", font_size_periodo, "px; color:#555; font-weight:500;'>", periodo_texto, "</span><br>",
        "<span style='font-size:", font_size_datos, "px; color:#333;'>", etiqueta_padron, ": ", total_padron, "</span><br>",
        "<span style='font-size:", font_size_datos, "px; color:#333;'>", etiqueta_lista, ": ", total_lista, "</span><br>",
        "<span style='font-size:", font_size_hint, "px; color:#888; font-style:italic;'>", hint_text, "</span>"
      )
      
      tooltip_font_size <- 9
      
    } else {
      # ============================================================
      # DESKTOP: Card normal
      # ============================================================
      message("🔧 [card_nb] Aplicando configuración DESKTOP")
      
      font_size_icon <- 14
      font_size_title <- 12
      font_size_periodo <- 11
      font_size_datos <- 10
      font_size_hint <- 9
      
      border_width <- 2
      border_pad <- 8
      
      pos_x <- 0.03
      pos_y <- 0.97
      x_anchor <- "left"
      y_anchor <- "top"
      
      etiqueta_padron <- etiqueta_padron_full
      etiqueta_lista <- etiqueta_lista_full
      hint_text <- "(Ver desglose)"
      
      # ✅ v2.5: CORREGIDO error de sintaxis
      texto_card <- paste0(
        "<span style='font-size:", font_size_icon, "px; color:#9B59B6;'>⚧</span> ",
        "<span style='font-size:", font_size_title, "px; font-weight:bold; color:#9B59B6;'>No Binario</span><br>",
        "<span style='font-size:", font_size_periodo, "px; color:#555; font-weight:600;'>", periodo_texto, "</span><br>",
        "<span style='font-size:", font_size_datos, "px; color:#333;'>", etiqueta_padron, ": ", total_padron, "</span><br>",
        "<span style='font-size:", font_size_datos, "px; color:#333;'>", etiqueta_lista, ": ", total_lista, "</span><br>",
        "<span style='font-size:", font_size_hint, "px; color:#888; font-style:italic;'>", hint_text, "</span>"
      )
      
      tooltip_font_size <- 11
    }
    
    # ========== CREAR ANNOTATION ==========
    
    annotation <- list(
      text = texto_card,
      hovertext = texto_tooltip,
      hoverlabel = list(
        bgcolor = "rgba(255, 255, 255, 0.98)",
        bordercolor = "#9B59B6",
        font = list(
          family = "Courier New, monospace",
          size = tooltip_font_size,
          color = "#333"
        )
      ),
      x = pos_x,
      y = pos_y,
      xref = "paper",
      yref = "paper",
      xanchor = x_anchor,
      yanchor = y_anchor,
      showarrow = FALSE,
      bgcolor = "rgba(255, 255, 255, 0.95)",
      bordercolor = "#9B59B6",
      borderwidth = border_width,
      borderpad = border_pad,
      font = list(
        size = font_size_datos,
        color = "#333",
        family = "Arial, sans-serif"
      ),
      captureevents = TRUE,
      name = "card-nb"
    )
    
    message("✅ [card_nb] Card creada para ", 
            if(is_mobile) "MÓVIL" else if(is_tablet) "TABLET" else "DESKTOP",
            " | Ámbito: ", ambito,
            " | Pos: (", pos_x, ", ", pos_y, ")",
            " | Hint: ", hint_text)
    
    return(annotation)
    
  }, error = function(e) {
    message("❌ [card_nb] Error creando card: ", e$message)
    return(NULL)
  })
  
  return(resultado)
}

# ========== FUNCIÓN AUXILIAR: CONSTRUIR TOOLTIP TABLA ==========

construir_tooltip_tabla <- function(datos_con_casos, tipo_periodo, ambito = "nacional") {
  
  if (is.null(datos_con_casos) || nrow(datos_con_casos) == 0) {
    return("Sin datos de desglose")
  }
  
  encabezado <- if(tipo_periodo == "mensual") {
    paste0("Desglose mensual - ", if(ambito == "nacional") "Nacional" else "Extranjero")
  } else {
    paste0("Desglose anual - ", if(ambito == "nacional") "Nacional" else "Extranjero")
  }
  
  header_tabla <- if(tipo_periodo == "mensual") "Mes     Padrón  Lista" else "Año   Padrón  Lista"
  
  filas <- c()
  for (i in 1:nrow(datos_con_casos)) {
    periodo <- datos_con_casos$periodo[i]
    padron <- datos_con_casos$padron_nb[i]
    lista <- datos_con_casos$lista_nb[i]
    
    if (tipo_periodo == "mensual") {
      fila <- sprintf("%-7s %4d  %4d", periodo, padron, lista)
    } else {
      fila <- sprintf("%4s  %4d  %4d", periodo, padron, lista)
    }
    filas <- c(filas, fila)
  }
  
  tabla_texto <- paste(filas, collapse = "\n")
  
  texto_tooltip <- paste0(
    encabezado, "\n",
    strrep("─", 24), "\n",
    header_tabla, "\n",
    tabla_texto
  )
  
  return(texto_tooltip)
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
  
  ultima_fecha <- max(datos$fecha)
  anio_base <- as.integer(format(ultima_fecha, "%Y"))
  mes_base <- as.integer(format(ultima_fecha, "%m"))
  
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

message("✅ graficas_helpers v2.5 cargado")
message("   ✅ CORREGIDO: Error de sintaxis en texto_card desktop")
message("   ✅ CORREGIDO: Detección de screen_width más robusta")
message("   ✅ Card NB compacta para móvil con line-height 1.5")
message("   ✅ Hint '(Clic para desglose)' en móvil")
