# modules/lista_nominal_graficas/graficas_helpers.R
# Funciones auxiliares para cálculos y proyecciones
# Versión: 1.7 - CORRECCIÓN: Card NB con tabla de desglose anual

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
# ✅ v1.7: Card con tabla de desglose anual para gráficas anuales

crear_card_no_binario <- function(datos, ambito = "nacional", tipo_periodo = "mensual", año_consultado = NULL) {
  
  resultado <- tryCatch({
    
    if (is.null(datos) || !is.data.frame(datos) || nrow(datos) == 0) {
      message("⚠️ [card_nb] Sin datos")
      return(NULL)
    }
    
    # ========== DETECTAR COLUMNAS DISPONIBLES ==========
    cols_disponibles <- colnames(datos)
    message("📋 [card_nb] Columnas disponibles: ", paste(head(cols_disponibles, 10), collapse = ", "))
    
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
      message("   Buscadas padrón: ", paste(posibles_padron, collapse = ", "))
      message("   Buscadas lista: ", paste(posibles_lista, collapse = ", "))
      return(NULL)
    }
    
    message("✅ [card_nb] Columnas encontradas: ", col_padron, ", ", col_lista)
    
    # Reemplazar NA con 0
    datos[[col_padron]][is.na(datos[[col_padron]])] <- 0
    datos[[col_lista]][is.na(datos[[col_lista]])] <- 0
    
    # Calcular totales
    total_padron <- sum(datos[[col_padron]], na.rm = TRUE)
    total_lista <- sum(datos[[col_lista]], na.rm = TRUE)
    total_casos <- total_padron + total_lista
    
    # Si no hay casos, no mostrar card
    if (total_casos == 0) {
      message("ℹ️ [card_nb] Sin casos NB en este período (P:", total_padron, " L:", total_lista, ")")
      return(NULL)
    }
    
    # Construir lista de períodos con casos
    if (tipo_periodo == "mensual") {
      if ("fecha" %in% cols_disponibles) {
        datos$periodo <- format(datos$fecha, "%b")
      } else {
        message("⚠️ [card_nb] No hay columna 'fecha' para períodos mensuales")
        return(NULL)
      }
    } else {
      if ("año" %in% cols_disponibles) {
        datos$periodo <- as.character(datos$año)
      } else {
        message("⚠️ [card_nb] No hay columna 'año' para períodos anuales")
        return(NULL)
      }
    }
    
    datos$padron_nb <- datos[[col_padron]]
    datos$lista_nb <- datos[[col_lista]]
    
    # Filtrar solo con casos
    datos_con_casos <- datos[datos$padron_nb + datos$lista_nb > 0, ]
    
    if (nrow(datos_con_casos) == 0) {
      message("ℹ️ [card_nb] No hay períodos con casos NB")
      return(NULL)
    }
    
    # ========== DIFERENCIAR ENTRE MENSUAL Y ANUAL ==========
    
    if (tipo_periodo == "mensual") {
      # ========== CARD MENSUAL (SIN CAMBIOS) ==========
      
      # Obtener meses con casos (solo nombres)
      meses_con_casos <- datos_con_casos$periodo
      
      # Limitar a 5 meses + "..."
      if (length(meses_con_casos) > 5) {
        meses_texto <- paste0(paste(head(meses_con_casos, 5), collapse=", "), "...")
      } else {
        meses_texto <- paste(meses_con_casos, collapse=", ")
      }
      
      # Determinar año consultado
      if (is.null(año_consultado)) {
        año_consultado <- format(datos$fecha[1], "%Y")
      }
      
      # ========== CALCULAR HISTÓRICO ==========
      # Para histórico necesitamos datos de años anteriores
      # Como solo tenemos datos del año actual, usamos placeholder
      historico_padron <- total_padron
      historico_lista <- total_lista
      año_inicio_historico <- año_consultado
      
      # Texto condensado
      texto_card <- paste0(
        "<b style='color:#9B59B6; font-size:12px'>⚧ No Binario</b><br>",
        "<span style='font-size:10px; line-height:1.4'>",
        año_consultado, ": <b>", total_casos, "</b><br>",
        "P:", total_padron, " | L:", total_lista, "<br>",
        "<i>", meses_texto, "</i><br>",
        "Hist: P:", historico_padron, " L:", historico_lista,
        "</span>"
      )
      
    } else {
      # ========== ✅ CARD ANUAL CON TABLA DE DESGLOSE (v1.7) ==========
      
      # Crear tabla de desglose por año
      desglose <- datos_con_casos[, c("periodo", "padron_nb", "lista_nb")]
      desglose <- desglose[order(desglose$periodo), ]
      
      # Construir filas de la tabla
      filas_tabla <- c()
      for (i in 1:nrow(desglose)) {
        año <- desglose$periodo[i]
        padron <- desglose$padron_nb[i]
        lista <- desglose$lista_nb[i]
        
        # Formatear números con espacios para alineación
        padron_str <- sprintf("%4d", padron)
        lista_str <- sprintf("%4d", lista)
        
        fila <- paste0(año, "  ", padron_str, "  ", lista_str)
        filas_tabla <- c(filas_tabla, fila)
      }
      
      # Agregar fila de totales
      total_padron_str <- sprintf("%4d", total_padron)
      total_lista_str <- sprintf("%4d", total_lista)
      fila_total <- paste0("Total ", total_padron_str, "  ", total_lista_str)
      filas_tabla <- c(filas_tabla, fila_total)
      
      # Unir todas las filas
      tabla_texto <- paste(filas_tabla, collapse = "<br>")
      
      # Construir texto completo de la card
      texto_card <- paste0(
        "<b style='color:#9B59B6; font-size:12px'>⚧ No Binario</b><br>",
        "<span style='font-size:10px; line-height:1.5'>",
        "<b>", etiqueta_padron, ": ", total_padron, "</b><br>",
        "<b>", etiqueta_lista, ": ", total_lista, "</b><br>",
        "<br>",
        "<b>Desglose por año:</b><br>",
        "<span style='font-family:monospace; font-size:9px'>",
        "Año  Padrón  Lista<br>",
        tabla_texto,
        "</span>",
        "</span>"
      )
      
      message("✅ [card_nb] Tabla anual creada con ", nrow(desglose), " años")
    }
    
    # ========== POSICIONAMIENTO INTELIGENTE ==========
    
    if (tipo_periodo == "anual") {
      # Anual: siempre arriba izquierda
      pos_x <- 0.08
      pos_y <- 0.92
      message("📍 [card_nb] Posición ANUAL: arriba izquierda")
    } else {
      # Mensual: detectar tendencia para evitar superposición
      if (nrow(datos) >= 3) {
        ultimos_3 <- tail(datos, 3)
        
        # Buscar columna de referencia
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
            if (pendiente > 0) {
              # Tendencia ascendente → Card abajo derecha
              pos_x <- 0.75
              pos_y <- 0.15
              message("📈 [card_nb] Tendencia ascendente → Card abajo derecha")
            } else {
              # Tendencia descendente → Card arriba derecha
              pos_x <- 0.75
              pos_y <- 0.85
              message("📉 [card_nb] Tendencia descendente → Card arriba derecha")
            }
          } else {
            # Por defecto: arriba derecha
            pos_x <- 0.75
            pos_y <- 0.85
            message("📍 [card_nb] Posición por defecto: arriba derecha")
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
    
    # ========== CREAR ANNOTATION ==========
    
    annotation <- list(
      text = texto_card,
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
    
    message("✅ [card_nb] Card creada - Casos totales: ", total_casos, " | Posición: (", pos_x, ", ", pos_y, ")")
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

message("✅ graficas_helpers v1.7 cargado")
message("   ✅ NUEVA CARACTERÍSTICA: Card NB con tabla de desglose anual")
message("   ✅ Formato: Padrón Nacional/Extranjero + Lista Nacional/Extranjero + Tabla por año")
