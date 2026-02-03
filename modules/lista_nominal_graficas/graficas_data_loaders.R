# modules/lista_nominal_graficas/graficas_data_loaders.R
# Reactives de carga de datos: year_actual, year_consulta, anuales
# Versión: 2.12 - CORRECCIÓN: Mapeo correcto de columnas de sexo para archivos modificados pre-2020
# Cambios vs v2.11:
#   - Agregado diagnóstico detallado de columnas disponibles en cada fecha
#   - Corrección en búsqueda de columnas: soporta tanto padron_hombres como padron_nacional_hombres
#   - Eliminado bindCache en datos_year_consulta para evitar servir datos obsoletos

graficas_data_loaders <- function(input, output, session, anio_actual, anio_consultado, filtros_usuario, estado_app) {
  
  message("📥 Inicializando graficas_data_loaders v2.12")
  
  # Obtener función cache_valido del entorno padre
  cache_valido <- function(timestamp, max_horas = 24) {
    if (is.null(timestamp)) return(FALSE)
    difftime(Sys.time(), timestamp, units = "hours") < max_horas
  }
  
  # ========== HELPER: Extraer columna de sexo con fallback ==========
  # Busca primero el nombre específico, luego alternativas comunes
  extraer_col_sexo <- function(obj, col_preferida, col_alternativa = NULL, es_lista = FALSE) {
    nombres <- if (es_lista) names(obj) else colnames(obj)
    
    # Intentar nombre preferido
    if (col_preferida %in% nombres) {
      if (es_lista) {
        return(as.numeric(gsub(",", "", as.character(obj[[col_preferida]]))))
      } else {
        return(sum(obj[[col_preferida]], na.rm = TRUE))
      }
    }
    
    # Intentar alternativa
    if (!is.null(col_alternativa) && col_alternativa %in% nombres) {
      if (es_lista) {
        return(as.numeric(gsub(",", "", as.character(obj[[col_alternativa]]))))
      } else {
        return(sum(obj[[col_alternativa]], na.rm = TRUE))
      }
    }
    
    return(NA)
  }
  
  # ========== REACTIVE: DATOS DEL AÑO ACTUAL (PARA GRÁFICAS 1, 2, 3) ==========
  
  datos_year_actual <- reactive({
    año_actual_valor <- anio_actual()
    estado_actual <- estado_app()
    
    # ✅ SOLUCIÓN DEFINITIVA: En estado restablecido, usar valores por defecto HARDCODEADOS
    if (estado_actual == "restablecido") {
      filtros <- list(
        entidad = "Nacional",
        distrito = "Todos",
        municipio = "Todos",
        seccion = "Todas",
        ambito = "nacional"
      )
      
      # ✅ CRÍTICO: Invalidar caché en estado restablecido
      if (exists("LNE_CACHE_GRAFICAS", envir = .GlobalEnv)) {
        cache_actual <- get("LNE_CACHE_GRAFICAS", envir = .GlobalEnv)
        cache_actual$datos_year_actual <- NULL
        cache_actual$timestamp_year <- NULL
        cache_actual$año_cacheado <- NULL
        assign("LNE_CACHE_GRAFICAS", cache_actual, envir = .GlobalEnv)
      }
      
    } else {
      # En estado consultado, leer inputs normalmente
      filtros <- list(
        entidad = input$entidad %||% "Nacional",
        distrito = input$distrito %||% "Todos",
        municipio = input$municipio %||% "Todos",
        seccion = input$seccion %||% "Todas",
        ambito = input$ambito_datos %||% "nacional"
      )
    }
    
    # ========== DETERMINAR SI USAR CACHÉ ==========
    es_nacional_sin_filtros <- (
      filtros$entidad == "Nacional" && 
        filtros$distrito == "Todos" && 
        filtros$municipio == "Todos" && 
        (is.null(filtros$seccion) || "Todas" %in% filtros$seccion || length(filtros$seccion) == 0) &&
        filtros$ambito == "nacional"
    )
    
    cache <- get("LNE_CACHE_GRAFICAS", envir = .GlobalEnv)
    
    message("🔵 [datos_year_actual] Solicitando datos del año actual: ", año_actual_valor)
    message("   Estado app: ", estado_app())
    message("   Ámbito: ", filtros$ambito)
    message("   Es Nacional sin filtros: ", es_nacional_sin_filtros)
    
    # ========== VERIFICAR CACHÉ ==========
    if (es_nacional_sin_filtros &&
        !is.null(cache$datos_year_actual) && 
        !is.null(cache$año_cacheado) &&
        cache$año_cacheado == año_actual_valor &&
        cache_valido(cache$timestamp_year, max_horas = 24)) {
      
      message("✅ [CACHÉ HIT] Usando datos cacheados del año actual ", año_actual_valor)
      return(cache$datos_year_actual)
    }
    
    message("📥 [CACHÉ MISS O FILTROS] Cargando datos del año actual ", año_actual_valor, " desde archivos...")
    
    if (!exists("LNE_CATALOG", envir = .GlobalEnv)) {
      return(NULL)
    }
    
    catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
    fechas_anio_actual <- catalog$historico[format(catalog$historico, "%Y") == año_actual_valor]
    
    if (length(fechas_anio_actual) == 0) {
      message("⚠️ Sin fechas para año actual")
      return(NULL)
    }
    
    message("📥 Cargando ", length(fechas_anio_actual), " fechas del año ", año_actual_valor)
    
    lista_datos <- list()
    
    estado_filtro <- if (filtros$entidad == "Nacional") "Nacional" else filtros$entidad
    message("   📍 Estado: ", estado_filtro, " | Ámbito: ", filtros$ambito)
    
    for (i in seq_along(fechas_anio_actual)) {
      fecha <- fechas_anio_actual[i]
      
      datos_temp <- tryCatch({
        cargar_lne(
          tipo_corte = "historico",
          fecha = as.Date(fecha, origin = "1970-01-01"),
          dimension = "completo",
          estado = estado_filtro,
          distrito = filtros$distrito,
          municipio = filtros$municipio,
          seccion = filtros$seccion,
          incluir_extranjero = TRUE
        )
      }, error = function(e) {
        message("⚠️ Error cargando fecha ", fecha, ": ", e$message)
        return(NULL)
      })
      
      if (!is.null(datos_temp)) {
        if (estado_filtro == "Nacional" && !is.null(datos_temp$totales)) {
          totales_fila <- datos_temp$totales
          
          # ========== EXTRACCIÓN COLUMNAS PRINCIPALES ==========
          padron_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional)))
          padron_extranjero <- extraer_col_sexo(totales_fila, "padron_extranjero", es_lista = TRUE)
          lista_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional)))
          lista_extranjero <- extraer_col_sexo(totales_fila, "lista_extranjero", es_lista = TRUE)
          
          # ========== EXTRACCIÓN COLUMNAS SEXO NACIONAL ==========
          # v2.12: Soportar tanto padron_hombres como padron_nacional_hombres
          padron_hombres <- extraer_col_sexo(totales_fila, "padron_hombres", "padron_nacional_hombres", es_lista = TRUE)
          padron_mujeres <- extraer_col_sexo(totales_fila, "padron_mujeres", "padron_nacional_mujeres", es_lista = TRUE)
          lista_hombres <- extraer_col_sexo(totales_fila, "lista_hombres", "lista_nacional_hombres", es_lista = TRUE)
          lista_mujeres <- extraer_col_sexo(totales_fila, "lista_mujeres", "lista_nacional_mujeres", es_lista = TRUE)
          
          # ========== COLUMNAS NO BINARIO NACIONAL ==========
          padron_nacional_no_binario <- extraer_col_sexo(totales_fila, "padron_nacional_no_binario", es_lista = TRUE)
          lista_nacional_no_binario <- extraer_col_sexo(totales_fila, "lista_nacional_no_binario", es_lista = TRUE)
          
          # ========== EXTRACCIÓN COLUMNAS SEXO EXTRANJERO ==========
          padron_extranjero_hombres <- extraer_col_sexo(totales_fila, "padron_extranjero_hombres", es_lista = TRUE)
          padron_extranjero_mujeres <- extraer_col_sexo(totales_fila, "padron_extranjero_mujeres", es_lista = TRUE)
          lista_extranjero_hombres <- extraer_col_sexo(totales_fila, "lista_extranjero_hombres", es_lista = TRUE)
          lista_extranjero_mujeres <- extraer_col_sexo(totales_fila, "lista_extranjero_mujeres", es_lista = TRUE)
          
          # ========== COLUMNAS NO BINARIO EXTRANJERO ==========
          padron_extranjero_no_binario <- extraer_col_sexo(totales_fila, "padron_extranjero_no_binario", es_lista = TRUE)
          lista_extranjero_no_binario <- extraer_col_sexo(totales_fila, "lista_extranjero_no_binario", es_lista = TRUE)
          
          # ========== CREAR REGISTRO CON TODAS LAS COLUMNAS ==========
          if (!is.na(padron_nacional) && !is.na(lista_nacional)) {
            registro <- data.frame(
              fecha = as.Date(fecha, origin = "1970-01-01"),
              padron_nacional = padron_nacional,
              padron_extranjero = ifelse(is.na(padron_extranjero), NA, padron_extranjero),
              lista_nacional = lista_nacional,
              lista_extranjero = ifelse(is.na(lista_extranjero), NA, lista_extranjero),
              padron_electoral = padron_nacional + ifelse(is.na(padron_extranjero), 0, padron_extranjero),
              lista_nominal = lista_nacional + ifelse(is.na(lista_extranjero), 0, lista_extranjero),
              padron_hombres = padron_hombres,
              padron_mujeres = padron_mujeres,
              lista_hombres = lista_hombres,
              lista_mujeres = lista_mujeres,
              padron_nacional_no_binario = padron_nacional_no_binario,
              lista_nacional_no_binario = lista_nacional_no_binario,
              padron_extranjero_hombres = padron_extranjero_hombres,
              padron_extranjero_mujeres = padron_extranjero_mujeres,
              lista_extranjero_hombres = lista_extranjero_hombres,
              lista_extranjero_mujeres = lista_extranjero_mujeres,
              padron_extranjero_no_binario = padron_extranjero_no_binario,
              lista_extranjero_no_binario = lista_extranjero_no_binario,
              stringsAsFactors = FALSE
            )
            
            message("   ✅ ", format(fecha, "%Y-%m-%d"))
            lista_datos[[length(lista_datos) + 1]] <- registro
          }
        } else if (!is.null(datos_temp$datos) && nrow(datos_temp$datos) > 0) {
          # ========== AGREGACIÓN DESDE DATAFRAME ==========
          df <- datos_temp$datos
          
          padron_nacional <- sum(df$padron_nacional, na.rm = TRUE)
          padron_extranjero <- sum(df$padron_extranjero, na.rm = TRUE)
          lista_nacional <- sum(df$lista_nacional, na.rm = TRUE)
          lista_extranjero <- sum(df$lista_extranjero, na.rm = TRUE)
          
          # v2.12: Soportar ambos nombres de columna
          padron_hombres <- extraer_col_sexo(df, "padron_hombres", "padron_nacional_hombres")
          padron_mujeres <- extraer_col_sexo(df, "padron_mujeres", "padron_nacional_mujeres")
          lista_hombres <- extraer_col_sexo(df, "lista_hombres", "lista_nacional_hombres")
          lista_mujeres <- extraer_col_sexo(df, "lista_mujeres", "lista_nacional_mujeres")
          
          padron_nacional_no_binario <- extraer_col_sexo(df, "padron_nacional_no_binario")
          lista_nacional_no_binario <- extraer_col_sexo(df, "lista_nacional_no_binario")
          
          padron_extranjero_hombres <- extraer_col_sexo(df, "padron_extranjero_hombres")
          padron_extranjero_mujeres <- extraer_col_sexo(df, "padron_extranjero_mujeres")
          lista_extranjero_hombres <- extraer_col_sexo(df, "lista_extranjero_hombres")
          lista_extranjero_mujeres <- extraer_col_sexo(df, "lista_extranjero_mujeres")
          
          padron_extranjero_no_binario <- extraer_col_sexo(df, "padron_extranjero_no_binario")
          lista_extranjero_no_binario <- extraer_col_sexo(df, "lista_extranjero_no_binario")
          
          registro <- data.frame(
            fecha = as.Date(fecha, origin = "1970-01-01"),
            padron_nacional = padron_nacional,
            padron_extranjero = ifelse(is.na(padron_extranjero) || padron_extranjero == 0, NA, padron_extranjero),
            lista_nacional = lista_nacional,
            lista_extranjero = ifelse(is.na(lista_extranjero) || lista_extranjero == 0, NA, lista_extranjero),
            padron_electoral = padron_nacional + ifelse(is.na(padron_extranjero), 0, padron_extranjero),
            lista_nominal = lista_nacional + ifelse(is.na(lista_extranjero), 0, lista_extranjero),
            padron_hombres = padron_hombres,
            padron_mujeres = padron_mujeres,
            lista_hombres = lista_hombres,
            lista_mujeres = lista_mujeres,
            padron_nacional_no_binario = padron_nacional_no_binario,
            lista_nacional_no_binario = lista_nacional_no_binario,
            padron_extranjero_hombres = padron_extranjero_hombres,
            padron_extranjero_mujeres = padron_extranjero_mujeres,
            lista_extranjero_hombres = lista_extranjero_hombres,
            lista_extranjero_mujeres = lista_extranjero_mujeres,
            padron_extranjero_no_binario = padron_extranjero_no_binario,
            lista_extranjero_no_binario = lista_extranjero_no_binario,
            stringsAsFactors = FALSE
          )
          
          message("   ✅ ", format(fecha, "%Y-%m-%d"), " (sumado desde ", nrow(df), " filas)")
          lista_datos[[length(lista_datos) + 1]] <- registro
        }
      }
    }
    
    if (length(lista_datos) == 0) {
      return(NULL)
    }
    
    if (length(lista_datos) > 1) {
      cols_primer_df <- names(lista_datos[[1]])
      
      for (i in 2:length(lista_datos)) {
        cols_actual <- names(lista_datos[[i]])
        
        if (!identical(cols_primer_df, cols_actual)) {
          cols_faltantes <- setdiff(cols_primer_df, cols_actual)
          if (length(cols_faltantes) > 0) {
            for (col in cols_faltantes) {
              lista_datos[[i]][[col]] <- NA
            }
          }
          lista_datos[[i]] <- lista_datos[[i]][, cols_primer_df]
        }
      }
    }
    
    datos_completos <- do.call(rbind, lista_datos)
    datos_completos <- datos_completos[order(datos_completos$fecha), ]
    
    message("✅ Datos del año actual cargados: ", nrow(datos_completos), " registros")
    
    if (es_nacional_sin_filtros) {
      cache$datos_year_actual <- datos_completos
      cache$timestamp_year <- Sys.time()
      cache$año_cacheado <- año_actual_valor
      assign("LNE_CACHE_GRAFICAS", cache, envir = .GlobalEnv)
      message("💾 Datos del año actual cacheados")
    }
    
    return(datos_completos)
  }) %>%
    bindEvent(estado_app(), input$btn_consultar, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ========== REACTIVE: DATOS DEL AÑO CONSULTADO (PARA GRÁFICAS 4, 5) ==========
  # v2.12: ELIMINADO bindCache para evitar servir datos obsoletos en consultas filtradas
  
  datos_year_consulta <- reactive({
    req(input$tipo_corte == "historico")
    
    year <- isolate(input$year)
    entidad <- isolate(input$entidad)
    distrito <- isolate(input$distrito %||% "Todos")
    municipio <- isolate(input$municipio %||% "Todos")
    seccion <- isolate(input$seccion %||% "Todas")
    ambito <- isolate(input$ambito_datos %||% "nacional")
    
    message("🔄 [datos_year_consulta] CONSULTA - Año ", year, ", Ámbito: ", ambito)
    message("   Entidad: ", entidad, " | Distrito: ", distrito)
    
    if (is.null(year)) {
      message("⚠️ [datos_year_consulta] year es NULL, usando año actual")
      year <- anio_actual()
    }
    
    if (year == anio_actual() && 
        ambito == "nacional" &&
        entidad == "Nacional" && 
        distrito == "Todos" && 
        municipio == "Todos" && 
        (is.null(seccion) || length(seccion) == 0 || "Todas" %in% seccion)) {
      
      message("✅ Redirigiendo a datos_year_actual()")
      return(datos_year_actual())
    }
    
    if (!exists("LNE_CATALOG", envir = .GlobalEnv)) {
      return(NULL)
    }
    
    catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
    fechas_year <- catalog$historico[format(catalog$historico, "%Y") == year]
    
    if (length(fechas_year) == 0) {
      return(NULL)
    }
    
    estado_filtro <- if (entidad == "Nacional") "Nacional" else entidad
    message("📍 Estado: ", estado_filtro, " | Ámbito: ", ambito)
    message("📥 Cargando ", length(fechas_year), " fechas del año ", year)
    
    lista_datos <- list()
    
    for (i in seq_along(fechas_year)) {
      fecha <- fechas_year[i]
      
      datos_temp <- tryCatch({
        cargar_lne(
          tipo_corte = "historico",
          fecha = as.Date(fecha, origin = "1970-01-01"),
          dimension = "completo",
          estado = estado_filtro,
          distrito = distrito,
          municipio = municipio,
          seccion = seccion,
          incluir_extranjero = TRUE
        )
      }, error = function(e) {
        message("⚠️ Error cargando fecha ", fecha, ": ", e$message)
        return(NULL)
      })
      
      if (!is.null(datos_temp)) {
        # v2.12: Diagnóstico de columnas disponibles (solo primera fecha)
        if (i == 1 && !is.null(datos_temp$datos) && nrow(datos_temp$datos) > 0) {
          cols_disponibles <- colnames(datos_temp$datos)
          cols_extranjero <- grep("extranjero", cols_disponibles, value = TRUE)
          message("   📋 Columnas extranjero disponibles: ", paste(cols_extranjero, collapse = ", "))
        }
        
        if (estado_filtro == "Nacional" && !is.null(datos_temp$totales)) {
          totales_fila <- datos_temp$totales
          
          # Igual que en datos_year_actual - usar helper extraer_col_sexo
          padron_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional)))
          padron_extranjero <- extraer_col_sexo(totales_fila, "padron_extranjero", es_lista = TRUE)
          lista_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional)))
          lista_extranjero <- extraer_col_sexo(totales_fila, "lista_extranjero", es_lista = TRUE)
          
          padron_hombres <- extraer_col_sexo(totales_fila, "padron_hombres", "padron_nacional_hombres", es_lista = TRUE)
          padron_mujeres <- extraer_col_sexo(totales_fila, "padron_mujeres", "padron_nacional_mujeres", es_lista = TRUE)
          lista_hombres <- extraer_col_sexo(totales_fila, "lista_hombres", "lista_nacional_hombres", es_lista = TRUE)
          lista_mujeres <- extraer_col_sexo(totales_fila, "lista_mujeres", "lista_nacional_mujeres", es_lista = TRUE)
          
          padron_nacional_no_binario <- extraer_col_sexo(totales_fila, "padron_nacional_no_binario", es_lista = TRUE)
          lista_nacional_no_binario <- extraer_col_sexo(totales_fila, "lista_nacional_no_binario", es_lista = TRUE)
          
          padron_extranjero_hombres <- extraer_col_sexo(totales_fila, "padron_extranjero_hombres", es_lista = TRUE)
          padron_extranjero_mujeres <- extraer_col_sexo(totales_fila, "padron_extranjero_mujeres", es_lista = TRUE)
          lista_extranjero_hombres <- extraer_col_sexo(totales_fila, "lista_extranjero_hombres", es_lista = TRUE)
          lista_extranjero_mujeres <- extraer_col_sexo(totales_fila, "lista_extranjero_mujeres", es_lista = TRUE)
          
          padron_extranjero_no_binario <- extraer_col_sexo(totales_fila, "padron_extranjero_no_binario", es_lista = TRUE)
          lista_extranjero_no_binario <- extraer_col_sexo(totales_fila, "lista_extranjero_no_binario", es_lista = TRUE)
          
          if (!is.na(padron_nacional) && !is.na(lista_nacional)) {
            registro <- data.frame(
              fecha = as.Date(fecha, origin = "1970-01-01"),
              padron_nacional = padron_nacional,
              padron_extranjero = ifelse(is.na(padron_extranjero), NA, padron_extranjero),
              lista_nacional = lista_nacional,
              lista_extranjero = ifelse(is.na(lista_extranjero), NA, lista_extranjero),
              padron_electoral = padron_nacional + ifelse(is.na(padron_extranjero), 0, padron_extranjero),
              lista_nominal = lista_nacional + ifelse(is.na(lista_extranjero), 0, lista_extranjero),
              padron_hombres = padron_hombres,
              padron_mujeres = padron_mujeres,
              lista_hombres = lista_hombres,
              lista_mujeres = lista_mujeres,
              padron_nacional_no_binario = padron_nacional_no_binario,
              lista_nacional_no_binario = lista_nacional_no_binario,
              padron_extranjero_hombres = padron_extranjero_hombres,
              padron_extranjero_mujeres = padron_extranjero_mujeres,
              lista_extranjero_hombres = lista_extranjero_hombres,
              lista_extranjero_mujeres = lista_extranjero_mujeres,
              padron_extranjero_no_binario = padron_extranjero_no_binario,
              lista_extranjero_no_binario = lista_extranjero_no_binario,
              stringsAsFactors = FALSE
            )
            
            lista_datos[[length(lista_datos) + 1]] <- registro
          }
        } else if (!is.null(datos_temp$datos) && nrow(datos_temp$datos) > 0) {
          df <- datos_temp$datos
          
          # Igual agregación con helper
          padron_nacional <- sum(df$padron_nacional, na.rm = TRUE)
          padron_extranjero <- sum(df$padron_extranjero, na.rm = TRUE)
          lista_nacional <- sum(df$lista_nacional, na.rm = TRUE)
          lista_extranjero <- sum(df$lista_extranjero, na.rm = TRUE)
          
          padron_hombres <- extraer_col_sexo(df, "padron_hombres", "padron_nacional_hombres")
          padron_mujeres <- extraer_col_sexo(df, "padron_mujeres", "padron_nacional_mujeres")
          lista_hombres <- extraer_col_sexo(df, "lista_hombres", "lista_nacional_hombres")
          lista_mujeres <- extraer_col_sexo(df, "lista_mujeres", "lista_nacional_mujeres")
          
          padron_nacional_no_binario <- extraer_col_sexo(df, "padron_nacional_no_binario")
          lista_nacional_no_binario <- extraer_col_sexo(df, "lista_nacional_no_binario")
          
          padron_extranjero_hombres <- extraer_col_sexo(df, "padron_extranjero_hombres")
          padron_extranjero_mujeres <- extraer_col_sexo(df, "padron_extranjero_mujeres")
          lista_extranjero_hombres <- extraer_col_sexo(df, "lista_extranjero_hombres")
          lista_extranjero_mujeres <- extraer_col_sexo(df, "lista_extranjero_mujeres")
          
          padron_extranjero_no_binario <- extraer_col_sexo(df, "padron_extranjero_no_binario")
          lista_extranjero_no_binario <- extraer_col_sexo(df, "lista_extranjero_no_binario")
          
          # v2.12: Diagnóstico de valores extraídos (solo primera fecha)
          if (i == 1) {
            message("   📊 Valores extranjero extraídos:")
            message("      padron_extranjero: ", padron_extranjero)
            message("      padron_extranjero_hombres: ", padron_extranjero_hombres)
            message("      padron_extranjero_mujeres: ", padron_extranjero_mujeres)
          }
          
          registro <- data.frame(
            fecha = as.Date(fecha, origin = "1970-01-01"),
            padron_nacional = padron_nacional,
            padron_extranjero = ifelse(is.na(padron_extranjero) || padron_extranjero == 0, NA, padron_extranjero),
            lista_nacional = lista_nacional,
            lista_extranjero = ifelse(is.na(lista_extranjero) || lista_extranjero == 0, NA, lista_extranjero),
            padron_electoral = padron_nacional + ifelse(is.na(padron_extranjero), 0, padron_extranjero),
            lista_nominal = lista_nacional + ifelse(is.na(lista_extranjero), 0, lista_extranjero),
            padron_hombres = padron_hombres,
            padron_mujeres = padron_mujeres,
            lista_hombres = lista_hombres,
            lista_mujeres = lista_mujeres,
            padron_nacional_no_binario = padron_nacional_no_binario,
            lista_nacional_no_binario = lista_nacional_no_binario,
            padron_extranjero_hombres = padron_extranjero_hombres,
            padron_extranjero_mujeres = padron_extranjero_mujeres,
            lista_extranjero_hombres = lista_extranjero_hombres,
            lista_extranjero_mujeres = lista_extranjero_mujeres,
            padron_extranjero_no_binario = padron_extranjero_no_binario,
            lista_extranjero_no_binario = lista_extranjero_no_binario,
            stringsAsFactors = FALSE
          )
          
          message("   ✅ ", format(fecha, "%Y-%m-%d"), " (sumado desde ", nrow(df), " filas)")
          lista_datos[[length(lista_datos) + 1]] <- registro
        }
      }
    }
    
    if (length(lista_datos) == 0) {
      return(NULL)
    }
    
    datos_completos <- do.call(rbind, lista_datos)
    datos_completos <- datos_completos[order(datos_completos$fecha), ]
    
    message("✅ Datos del año ", year, " cargados: ", nrow(datos_completos), " registros")
    
    # v2.12: Diagnóstico final de columnas extranjero
    if (ambito == "extranjero") {
      message("   📊 Resumen extranjero:")
      message("      Rango padron_extranjero_hombres: ", 
              min(datos_completos$padron_extranjero_hombres, na.rm = TRUE), " - ",
              max(datos_completos$padron_extranjero_hombres, na.rm = TRUE))
      message("      Rango padron_extranjero_mujeres: ", 
              min(datos_completos$padron_extranjero_mujeres, na.rm = TRUE), " - ",
              max(datos_completos$padron_extranjero_mujeres, na.rm = TRUE))
    }
    
    return(datos_completos)
  }) %>% 
    # v2.12: ELIMINADO bindCache - causaba datos obsoletos en consultas filtradas
    bindEvent(estado_app(), input$btn_consultar, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ========== REACTIVE: DATOS ANUALES (2017-ACTUAL) ==========
  
  datos_anuales_completos <- reactive({
    estado_actual <- estado_app()
    
    # ✅ SOLUCIÓN DEFINITIVA: En estado restablecido, usar valores por defecto HARDCODEADOS
    if (estado_actual == "restablecido") {
      filtros <- list(
        entidad = "Nacional",
        distrito = "Todos",
        municipio = "Todos",
        seccion = "Todas",
        ambito = "nacional"
      )
      
      # ✅ CRÍTICO: Invalidar caché en estado restablecido
      if (exists("LNE_CACHE_GRAFICAS", envir = .GlobalEnv)) {
        cache_actual <- get("LNE_CACHE_GRAFICAS", envir = .GlobalEnv)
        cache_actual$datos_anuales <- NULL
        cache_actual$timestamp_anuales <- NULL
        assign("LNE_CACHE_GRAFICAS", cache_actual, envir = .GlobalEnv)
        message("🧹 [datos_anuales_completos] Caché invalidado en estado restablecido")
      }
      
    } else {
      # En estado consultado, leer inputs normalmente
      filtros <- list(
        entidad = input$entidad %||% "Nacional",
        distrito = input$distrito %||% "Todos",
        municipio = input$municipio %||% "Todos",
        seccion = input$seccion %||% "Todas",
        ambito = input$ambito_datos %||% "nacional"
      )
    }
    
    es_nacional_sin_filtros <- (
      filtros$entidad == "Nacional" && 
        filtros$distrito == "Todos" && 
        filtros$municipio == "Todos" && 
        (is.null(filtros$seccion) || "Todas" %in% filtros$seccion || length(filtros$seccion) == 0) &&
        filtros$ambito == "nacional"
    )
    
    cache <- get("LNE_CACHE_GRAFICAS", envir = .GlobalEnv)
    
    año_actual_valor <- anio_actual()
    
    if (input$btn_consultar == 0) {
      message("🚀 [datos_anuales_completos] CARGA INICIAL - Evolución 2017-", año_actual_valor)
    } else {
      message("🔄 [datos_anuales_completos] CONSULTA - Evolución 2017-", año_actual_valor)
    }
    
    if (es_nacional_sin_filtros &&
        !is.null(cache$datos_anuales) &&
        cache_valido(cache$timestamp_anuales, max_horas = 24)) {
      
      message("✅ [CACHÉ HIT] Usando datos anuales cacheados")
      return(cache$datos_anuales)
    }
    
    message("📥 [CACHÉ MISS O FILTROS] Cargando datos anuales desde archivos...")
    
    if (!exists("LNE_CATALOG", envir = .GlobalEnv)) {
      return(NULL)
    }
    
    catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
    años <- 2017:año_actual_valor
    
    lista_anuales <- list()
    
    estado_filtro <- if (filtros$entidad == "Nacional") "Nacional" else filtros$entidad
    message("   📍 Estado: ", estado_filtro, " | Ámbito: ", filtros$ambito)
    
    for (año in años) {
      message("🔍 Procesando año: ", año)
      
      fechas_año <- catalog$historico[format(catalog$historico, "%Y") == año]
      
      if (length(fechas_año) == 0) {
        message("   ⚠️ Sin fechas para año ", año)
        next
      }
      
      ultima_fecha <- max(fechas_año)
      message("   📅 Última fecha del año ", año, ": ", as.Date(ultima_fecha, origin = "1970-01-01"))
      
      datos_temp <- tryCatch({
        cargar_lne(
          tipo_corte = "historico",
          fecha = as.Date(ultima_fecha, origin = "1970-01-01"),
          dimension = "completo",
          estado = estado_filtro,
          distrito = filtros$distrito,
          municipio = filtros$municipio,
          seccion = filtros$seccion,
          incluir_extranjero = TRUE
        )
      }, error = function(e) {
        message("   ❌ Error en cargar_lne para año ", año, ": ", e$message)
        return(NULL)
      })
      
      if (!is.null(datos_temp)) {
        if (estado_filtro == "Nacional" && !is.null(datos_temp$totales)) {
          totales_fila <- datos_temp$totales
          
          message("   ✅ Fila totales obtenida para año ", año)
          
          # Igual extracción con helper
          padron_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional)))
          lista_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional)))
          
          padron_extranjero <- extraer_col_sexo(totales_fila, "padron_extranjero", es_lista = TRUE)
          lista_extranjero <- extraer_col_sexo(totales_fila, "lista_extranjero", es_lista = TRUE)
          
          padron_hombres <- extraer_col_sexo(totales_fila, "padron_hombres", "padron_nacional_hombres", es_lista = TRUE)
          padron_mujeres <- extraer_col_sexo(totales_fila, "padron_mujeres", "padron_nacional_mujeres", es_lista = TRUE)
          lista_hombres <- extraer_col_sexo(totales_fila, "lista_hombres", "lista_nacional_hombres", es_lista = TRUE)
          lista_mujeres <- extraer_col_sexo(totales_fila, "lista_mujeres", "lista_nacional_mujeres", es_lista = TRUE)
          
          padron_nacional_no_binario <- extraer_col_sexo(totales_fila, "padron_nacional_no_binario", es_lista = TRUE)
          lista_nacional_no_binario <- extraer_col_sexo(totales_fila, "lista_nacional_no_binario", es_lista = TRUE)
          
          padron_extranjero_hombres <- extraer_col_sexo(totales_fila, "padron_extranjero_hombres", es_lista = TRUE)
          padron_extranjero_mujeres <- extraer_col_sexo(totales_fila, "padron_extranjero_mujeres", es_lista = TRUE)
          lista_extranjero_hombres <- extraer_col_sexo(totales_fila, "lista_extranjero_hombres", es_lista = TRUE)
          lista_extranjero_mujeres <- extraer_col_sexo(totales_fila, "lista_extranjero_mujeres", es_lista = TRUE)
          
          padron_extranjero_no_binario <- extraer_col_sexo(totales_fila, "padron_extranjero_no_binario", es_lista = TRUE)
          lista_extranjero_no_binario <- extraer_col_sexo(totales_fila, "lista_extranjero_no_binario", es_lista = TRUE)
          
          if (!is.na(padron_nacional) && !is.na(lista_nacional)) {
            lista_anuales[[length(lista_anuales) + 1]] <- data.frame(
              año = as.character(año),
              fecha = as.Date(ultima_fecha, origin = "1970-01-01"),
              padron_nacional = padron_nacional,
              padron_extranjero = padron_extranjero,
              lista_nacional = lista_nacional,
              lista_extranjero = lista_extranjero,
              padron_hombres = padron_hombres,
              padron_mujeres = padron_mujeres,
              lista_hombres = lista_hombres,
              lista_mujeres = lista_mujeres,
              padron_nacional_no_binario = padron_nacional_no_binario,
              lista_nacional_no_binario = lista_nacional_no_binario,
              padron_extranjero_hombres = padron_extranjero_hombres,
              padron_extranjero_mujeres = padron_extranjero_mujeres,
              lista_extranjero_hombres = lista_extranjero_hombres,
              lista_extranjero_mujeres = lista_extranjero_mujeres,
              padron_extranjero_no_binario = padron_extranjero_no_binario,
              lista_extranjero_no_binario = lista_extranjero_no_binario,
              stringsAsFactors = FALSE
            )
            
            padron_electoral <- padron_nacional + ifelse(is.na(padron_extranjero), 0, padron_extranjero)
            lista_nominal <- lista_nacional + ifelse(is.na(lista_extranjero), 0, lista_extranjero)
            
            message("   ✅ ", año, " | Padrón: ", format(padron_electoral, big.mark = ","),
                    " | Lista: ", format(lista_nominal, big.mark = ","))
          } else {
            message("   ❌ Valores principales son NA para año ", año)
          }
        } else if (!is.null(datos_temp$datos) && nrow(datos_temp$datos) > 0) {
          df <- datos_temp$datos
          
          # Igual agregación con helper
          padron_nacional <- sum(df$padron_nacional, na.rm = TRUE)
          padron_extranjero <- sum(df$padron_extranjero, na.rm = TRUE)
          lista_nacional <- sum(df$lista_nacional, na.rm = TRUE)
          lista_extranjero <- sum(df$lista_extranjero, na.rm = TRUE)
          
          lista_anuales[[length(lista_anuales) + 1]] <- data.frame(
            año = as.character(año),
            fecha = as.Date(ultima_fecha, origin = "1970-01-01"),
            padron_nacional = padron_nacional,
            padron_extranjero = ifelse(is.na(padron_extranjero) || padron_extranjero == 0, NA, padron_extranjero),
            lista_nacional = lista_nacional,
            lista_extranjero = ifelse(is.na(lista_extranjero) || lista_extranjero == 0, NA, lista_extranjero),
            padron_hombres = extraer_col_sexo(df, "padron_hombres", "padron_nacional_hombres"),
            padron_mujeres = extraer_col_sexo(df, "padron_mujeres", "padron_nacional_mujeres"),
            lista_hombres = extraer_col_sexo(df, "lista_hombres", "lista_nacional_hombres"),
            lista_mujeres = extraer_col_sexo(df, "lista_mujeres", "lista_nacional_mujeres"),
            padron_nacional_no_binario = extraer_col_sexo(df, "padron_nacional_no_binario"),
            lista_nacional_no_binario = extraer_col_sexo(df, "lista_nacional_no_binario"),
            padron_extranjero_hombres = extraer_col_sexo(df, "padron_extranjero_hombres"),
            padron_extranjero_mujeres = extraer_col_sexo(df, "padron_extranjero_mujeres"),
            lista_extranjero_hombres = extraer_col_sexo(df, "lista_extranjero_hombres"),
            lista_extranjero_mujeres = extraer_col_sexo(df, "lista_extranjero_mujeres"),
            padron_extranjero_no_binario = extraer_col_sexo(df, "padron_extranjero_no_binario"),
            lista_extranjero_no_binario = extraer_col_sexo(df, "lista_extranjero_no_binario"),
            stringsAsFactors = FALSE
          )
          
          message("   ✅ ", año, " (sumado desde ", nrow(df), " filas)")
        }
      }
    }
    
    if (length(lista_anuales) == 0) {
      message("⚠️ No se cargaron datos anuales")
      return(NULL)
    }
    
    datos_completos <- do.call(rbind, lista_anuales)
    
    message("✅ Datos anuales cargados: ", nrow(datos_completos), " años (2017-", año_actual_valor, ")")
    
    if (es_nacional_sin_filtros) {
      cache$datos_anuales <- datos_completos
      cache$timestamp_anuales <- Sys.time()
      assign("LNE_CACHE_GRAFICAS", cache, envir = .GlobalEnv)
      message("💾 Datos anuales cacheados")
    }
    
    return(datos_completos)
  }) %>% 
    bindCache(estado_app(), input$btn_consultar, input$tipo_corte, input$ambito_datos) %>%
    bindEvent(estado_app(), input$btn_consultar, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ========== RETORNAR LISTA DE REACTIVES ==========
  
  message("✅ graficas_data_loaders v2.12 inicializado")
  message("   ✅ CORRECCIÓN v2.12: Helper extraer_col_sexo con fallback de nombres")
  message("   ✅ CORRECCIÓN v2.12: Eliminado bindCache en datos_year_consulta")
  message("   ✅ CORRECCIÓN v2.12: Diagnóstico de columnas extranjero en consola")
  
  return(list(
    datos_year_actual = datos_year_actual,
    datos_year_consulta = datos_year_consulta,
    datos_anuales_completos = datos_anuales_completos
  ))
}
