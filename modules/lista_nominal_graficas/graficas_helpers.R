# modules/lista_nominal_graficas/graficas_helpers.R
# Funciones auxiliares para cálculos y proyecciones
# Versión: 1.2 - CORRECCIÓN CRÍTICA: generar_texto_alcance siempre muestra filtros geográficos

# ========== FUNCIÓN: GENERAR TEXTO DE ALCANCE ==========
# ✅ CORRECCIÓN: SIEMPRE mostrar filtros geográficos completos
# El TÍTULO de la gráfica ya indica si es Nacional o Extranjero
# El SUBTÍTULO debe mostrar el alcance geográfico configurado por el usuario

generar_texto_alcance <- function(input) {
  
  # ✅ SIEMPRE construir el alcance con los 4 niveles de filtros geográficos
  partes <- c()
  
  # 1. Estado/Entidad
  entidad <- input$entidad %||% "Nacional"
  partes <- c(partes, paste0("Estado: ", entidad))
  
  # 2. Distrito
  distrito <- input$distrito %||% "Todos"
  partes <- c(partes, paste0("Distrito: ", distrito))
  
  # 3. Municipio
  municipio <- input$municipio %||% "Todos"
  partes <- c(partes, paste0("Municipio: ", municipio))
  
  # 4. Secciones
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
  
  # Unir con " - "
  texto <- paste(partes, collapse = " - ")
  
  message("📋 [generar_texto_alcance] ", texto)
  return(texto)
}

# ========== FUNCIÓN: PROYECCIÓN CON TASA DE CRECIMIENTO ==========
# Proyecta valores futuros basándose en tasa de crecimiento mensual promedio
# Utilizada en Gráfica 1 (Proyección mensual año actual)

proyectar_con_tasa_crecimiento <- function(datos, meses_proyectar = 5, usar_columnas_separadas = FALSE) {
  
  if (is.null(datos) || nrow(datos) < 2) {
    message("⚠️ [proyectar_con_tasa_crecimiento] Datos insuficientes para proyectar")
    return(NULL)
  }
  
  # Calcular tasa de crecimiento mensual promedio
  n <- nrow(datos)
  
  if (usar_columnas_separadas) {
    # Para Nacional: usar lista_nacional y padron_nacional
    valor_inicial <- datos$lista_nacional[1]
    valor_final <- datos$lista_nacional[n]
    padron_inicial <- datos$padron_nacional[1]
    padron_final <- datos$padron_nacional[n]
  } else {
    # Para totales (retrocompatibilidad)
    valor_inicial <- datos$lista_nominal[1]
    valor_final <- datos$lista_nominal[n]
    padron_inicial <- datos$padron_electoral[1]
    padron_final <- datos$padron_electoral[n]
  }
  
  # Validar valores
  if (valor_inicial == 0 || is.na(valor_inicial) || is.na(valor_final)) {
    message("⚠️ [proyectar_con_tasa_crecimiento] Valores inválidos para proyectar")
    return(NULL)
  }
  
  if (padron_inicial == 0 || is.na(padron_inicial) || is.na(padron_final)) {
    message("⚠️ [proyectar_con_tasa_crecimiento] Valores de padrón inválidos")
    return(NULL)
  }
  
  # Calcular tasas mensuales
  tasa_mensual_lista <- ((valor_final / valor_inicial) ^ (1 / (n - 1))) - 1
  tasa_mensual_padron <- ((padron_final / padron_inicial) ^ (1 / (n - 1))) - 1
  
  message("📊 [proyectar_con_tasa_crecimiento] Tasa mensual lista: ", round(tasa_mensual_lista * 100, 4), "%")
  message("📊 [proyectar_con_tasa_crecimiento] Tasa mensual padrón: ", round(tasa_mensual_padron * 100, 4), "%")
  
  # Crear fechas proyectadas - FORZAR ÚLTIMO DÍA DEL MES
  ultima_fecha <- max(datos$fecha)
  anio_base <- as.integer(format(ultima_fecha, "%Y"))
  mes_base <- as.integer(format(ultima_fecha, "%m"))
  
  message("📅 [proyectar_con_tasa_crecimiento] Última fecha base: ", ultima_fecha, " (", mes_base, "/", anio_base, ")")
  
  # Crear lista para almacenar fechas
  fechas_proyectadas <- list()
  
  for (i in 1:meses_proyectar) {
    mes_proyectado <- mes_base + i
    anio_proyectado <- anio_base
    
    # Ajustar si pasa de diciembre
    if (mes_proyectado > 12) {
      anio_proyectado <- anio_base + floor((mes_proyectado - 1) / 12)
      mes_proyectado <- ((mes_proyectado - 1) %% 12) + 1
    }
    
    # Obtener último día del mes
    # Crear fecha del día 1 del mes siguiente, luego restar 1 día
    if (mes_proyectado == 12) {
      ultimo_dia <- as.Date(paste0(anio_proyectado + 1, "-01-01")) - 1
    } else {
      ultimo_dia <- as.Date(paste0(anio_proyectado, "-", sprintf("%02d", mes_proyectado + 1), "-01")) - 1
    }
    
    fechas_proyectadas[[i]] <- ultimo_dia
  }
  
  # Convertir lista a vector de fechas
  fechas_proyectadas <- do.call(c, fechas_proyectadas)
  
  message("📅 [proyectar_con_tasa_crecimiento] Fechas proyectadas: ", paste(fechas_proyectadas, collapse = ", "))
  
  # Proyectar valores
  lista_proyectada <- numeric(meses_proyectar)
  padron_proyectado <- numeric(meses_proyectar)
  
  for (i in 1:meses_proyectar) {
    lista_proyectada[i] <- valor_final * ((1 + tasa_mensual_lista) ^ i)
    padron_proyectado[i] <- padron_final * ((1 + tasa_mensual_padron) ^ i)
  }
  
  # Crear dataframe de proyecciones
  proyecciones <- data.frame(
    fecha = fechas_proyectadas,
    lista_proyectada = lista_proyectada,
    padron_proyectado = padron_proyectado,
    tipo = "Proyección",
    stringsAsFactors = FALSE
  )
  
  message("✅ [proyectar_con_tasa_crecimiento] Proyección calculada: ", nrow(proyecciones), " meses")
  message("   Primer mes proyectado: ", fechas_proyectadas[1], " - Lista: ", format(lista_proyectada[1], big.mark = ","))
  message("   Último mes proyectado: ", fechas_proyectadas[meses_proyectar], " - Lista: ", format(lista_proyectada[meses_proyectar], big.mark = ","))
  
  return(proyecciones)
}

# ========== FUNCIÓN: ORDENAR TRAZAS POR VALOR FINAL ==========
# Ordena las trazas de una gráfica según el último valor (de mayor a menor)
# Útil para mantener orden visual correcto en leyendas y tooltips

ordenar_trazas_por_valor <- function(datos, cols_valores) {
  
  if (is.null(datos) || nrow(datos) == 0 || length(cols_valores) == 0) {
    return(NULL)
  }
  
  # Extraer últimos valores de cada columna
  valores_finales <- sapply(cols_valores, function(col) {
    if (col %in% colnames(datos)) {
      vals <- datos[[col]][!is.na(datos[[col]])]
      if (length(vals) > 0) {
        return(tail(vals, 1))
      }
    }
    return(0)
  })
  
  # Crear dataframe con orden
  orden_df <- data.frame(
    columna = cols_valores,
    valor_final = valores_finales,
    stringsAsFactors = FALSE
  )
  
  # Ordenar de mayor a menor
  orden_df <- orden_df[order(orden_df$valor_final, decreasing = TRUE), ]
  
  return(orden_df$columna)
}

# ========== FUNCIÓN: VALIDAR EXISTENCIA DE COLUMNAS ==========
# Verifica que todas las columnas necesarias existan en un dataframe

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
# Verifica que al menos una fila tenga valores no-NA en las columnas especificadas

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

message("✅ graficas_helpers v1.2 cargado (CORRECCIÓN: generar_texto_alcance siempre muestra filtros completos)")

