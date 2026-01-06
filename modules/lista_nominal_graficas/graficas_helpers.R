# modules/lista_nominal_graficas/graficas_helpers.R
# Funciones auxiliares para cálculos y proyecciones
# Versión: 2.1 - CORRECCIÓN: Card condensada con tooltip hover para desglose

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

# ========== FUNCIÓN: CREAR CARD NO BINARIO ==========
# ✅ v2.1: Card condensada + tooltip hover con desglose completo

crear_card_no_binario <- function(datos, ambito = "nacional", tipo_periodo = "mensual", año_consultado = NULL) {
  
  resultado <- tryCatch({
    
    if (is.null(datos) || !is.data.frame(datos) || nrow(datos) == 0) {
      message("⚠️ [card_nb] Sin datos")
      return(NULL)
    }
    
    # ========== DETECTAR COLUMNAS DISPONIBLES ==========
    cols_disponibles <- colnames(datos)
    
    # Posibles nombres de columnas NB
    if (ambito == "nacional") {
      posibles_padron <- c("padron_nacional_no_binario", "padron_no_binario", "padron_nb")
      posibles_lista <- c("lista_nacional_no_binario", "lista_no_binario", "lista_nb")
      etiqueta_padron <- "Padrón Nacional"
      etiqueta_lista <- "Lista Nacional"
    } else {
      posibles_padron <- c("padron_extranjero_no_binario", "padron_no_binario", "padron_nb")
      posibles_lista <- c("lista_extranjero_no_binario", "lista_no_binario", "lista_nb")
      etiqueta_padron <- "Padrón Extranjero"
      etiqueta_lista <- "Lista Extranjero"
    }
    
    # Buscar primera columna que exista
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
    
    # Si no se encuentran columnas, retornar NULL
    if (is.null(col_padron) || is.null(col_lista)) {
      message("ℹ️ [card_nb] Columnas NB no disponibles para ", ambito)
      return(NULL)
    }
    
    # Reemplazar NA con 0
    datos[[col_padron]][is.na(datos[[col_padron]])] <- 0
    datos[[col_lista]][is.na(datos[[col_lista]])] <- 0
    
    # ========== CALCULAR TOTAL (ÚLTIMO PERÍODO CON DATOS) ==========
    datos_ordenados <- datos[order(datos$fecha), ]
    
    # Encontrar último período con datos > 0
    datos_con_casos <- datos_ordenados[datos_ordenados[[col_padron]] + datos_ordenados[[col_lista]] > 0, ]
    
    if (nrow(datos_con_casos) == 0) {
      message("ℹ️ [card_nb] Sin casos NB en ningún período")
      return(NULL)
    }
    
    ultima_fila <- datos_con_casos[nrow(datos_con_casos), ]
    
    total_padron <- ultima_fila[[col_padron]]
    total_lista <- ultima_fila[[col_lista]]
    
    # Determinar mes/año del último dato
    if (tipo_periodo == "mensual") {
      periodo_texto <- format(ultima_fila$fecha, "%B %Y")  # "Diciembre 2024"
    } else {
      periodo_texto <- format(ultima_fila$fecha, "%Y")  # "2024"
    }
    
    message("📊 [card_nb] Último período con datos: ", periodo_texto, " | P:", total_padron, " L:", total_lista)
    
    # ========== PREPARAR DATOS PARA TOOLTIP ==========
    
    if (tipo_periodo == "mensual") {
      datos$periodo <- format(datos$fecha, "%b")
      datos$periodo_orden <- as.integer(format(datos$fecha, "%m"))
      
      # Eliminar duplicados de mes (usar fecha más reciente)
      fechas_unicas <- aggregate(
        fecha ~ periodo,
        data = datos,
        FUN = max
      )
      datos <- merge(datos, fechas_unicas, by = c("periodo", "fecha"))
      
    } else {
      datos$periodo <- as.character(datos$año)
      datos$periodo_orden <- as.integer(datos$año)
    }
    
    datos$padron_nb <- datos[[col_padron]]
    datos$lista_nb <- datos[[col_lista]]
    
    # ========== FILTRAR SOLO PERÍODOS CON CASOS ==========
    datos_con_casos <- datos[datos$padron_nb + datos$lista_nb > 0, ]
    
    if (nrow(datos_con_casos) == 0) {
      message("ℹ️ [card_nb] Sin casos NB")
      return(NULL)
    }
    
    # Ordenar
    datos_con_casos <- datos_con_casos[order(datos_con_casos$periodo_orden), ]
    
    # ========== CREAR TABLA PARA TOOLTIP ==========
    
    desglose <- datos_con_casos[, c("periodo", "padron_nb", "lista_nb")]
    
    # Construir filas de la tabla
    filas_tabla <- c()
    for (i in 1:nrow(desglose)) {
      periodo <- desglose$periodo[i]
      padron <- desglose$padron_nb[i]
      lista <- desglose$lista_nb[i]
      
      if (tipo_periodo == "mensual") {
        # "Jan    25    21"
        fila <- sprintf("%-7s %4d  %4d", periodo, padron, lista)
      } else {
        # "2023    80    75"
        fila <- sprintf("%4s  %4d  %4d", periodo, padron, lista)
      }
      
      filas_tabla <- c(filas_tabla, fila)
    }
    
    # Unir filas (sin total, valores ya son acumulativos)
    tabla_texto <- paste(filas_tabla, collapse = "\n")
    
    # ========== CONSTRUIR TEXTO DE CARD CONDENSADA ==========
    
    texto_card <- paste0(
      "<b style='color:#9B59B6; font-size:12px'>⚧ No Binario</b><br>",
      "<span style='font-size:10px; line-height:1.5'>",
      "<b>", periodo_texto, "</b><br>",
      "<b>", etiqueta_padron, ": ", total_padron, "</b><br>",
      "<b>", etiqueta_lista, ": ", total_lista, "</b><br>",
      "<span style='font-size:8px; color:#666; font-style:italic'>",
      "(Pase el mouse para ver desglose)",
      "</span>",
      "</span>"
    )
    
    # ========== CONSTRUIR TEXTO DE TOOLTIP ==========
    
    if (tipo_periodo == "mensual") {
      encabezado_tooltip <- "Desglose mensual"
      header_tabla <- "Mes     Padrón  Lista"
    } else {
      encabezado_tooltip <- "Desglose anual"
      header_tabla <- "Año   Padrón  Lista"
    }
    
    texto_tooltip <- paste0(
      encabezado_tooltip, "\n",
      strrep("─", 22), "\n",
      header_tabla, "\n",
      tabla_texto
    )
    
    message("✅ [card_nb] Card creada con ", nrow(desglose), " períodos en tooltip")
    
    # ========== POSICIONAMIENTO INTELIGENTE ==========
    
    if (tipo_periodo == "anual") {
      pos_x <- 0.08
      pos_y <- 0.92
    } else {
      # Mensual: detectar tendencia
      if (nrow(datos) >= 3) {
        ultimos_3 <- tail(datos, 3)
        
        col_ref <- NULL
        if ("lista_nacional" %in% colnames(ultimos_3)) {
          col_ref <- "lista_nacional"
        } else if ("lista_extranjero" %in% colnames(ultimos_3)) {
          col_ref <- "lista_extranjero"
        }
        
        if (!is.null(col_ref)) {
          valores <- ultimos_3[[col_ref]][!is.na(ultimos_3[[col_ref]])]
          if (length(valores) >= 2) {
            pendiente <- valores[length(valores)] - valores[1]
            pos_x <- if(pendiente > 0) 0.75 else 0.75
            pos_y <- if(pendiente > 0) 0.15 else 0.85
          } else {
            pos_x <- 0.75
            pos_y <- 0.85
          }
        } else {
          pos_x <- 0.75
          pos_y <- 0.85
        }
      } else {
        pos_x <- 0.75
        pos_y <- 0.85
      }
    }
    
    # ========== CREAR ANNOTATION CON HOVERTEXT ==========
    
    annotation <- list(
      text = texto_card,
      hovertext = texto_tooltip,  # ✅ v2.1: TOOLTIP
      hoverlabel = list(
        bgcolor = "rgba(255, 255, 255, 0.98)",
        bordercolor = "#9B59B6",
        font = list(
          family = "Courier New, monospace",
          size = 11,
          color = "#333"
        )
      ),
      x = pos_x,
      y = pos_y,
      xref = "paper",
      yref = "paper",
      xanchor = "left",
      yanchor = "top",
      showarrow = FALSE,
      bgcolor = "rgba(255, 255, 255, 0.95)",
      bordercolor = "#9B59B6",
      borderwidth = 2.5,
      borderpad = 8,
      font = list(
        size = 10,
        color = "#333",
        family = "Arial, sans-serif"
      )
    )
    
    message("✅ [card_nb] Card creada - Hover habilitado")
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

message("✅ graficas_helpers v2.1 cargado")
message("   ✅ CORRECCIÓN: Card condensada con tooltip hover")
message("   ✅ Solo muestra períodos con datos > 0")
message("   ✅ Tooltip con desglose completo (sin total, valores acumulativos)")
message("   ✅ Indicador visual: '(Pase el mouse para ver desglose)'")

