# modules/lista_nominal_graficas/graficas_core.R
# Reactives base: caché, filtros, año, estado
# Versión: 3.0 - OPTIMIZACIÓN: año_ultimo_archivo dinámico desde catálogo
#
# CAMBIOS vs v2.8:
#   - Renombrado anio_actual → año_ultimo_archivo (más preciso semánticamente)
#   - El año se detecta del archivo más reciente en el catálogo, no del sistema
#   - Mejoras en documentación y claridad del código

graficas_core <- function(input, output, session, estado_app) {
  
  message("📦 Inicializando graficas_core v3.0")
  
  
  # ════════════════════════════════════════════════════════════════════════════
  # SISTEMA DE CACHÉ GLOBAL
  # ════════════════════════════════════════════════════════════════════════════
  
  if (!exists("LNE_CACHE_GRAFICAS", envir = .GlobalEnv)) {
    message("📦 Inicializando caché global de gráficas")
    assign("LNE_CACHE_GRAFICAS", list(
      datos_year_actual = NULL,
      datos_anuales = NULL,
      timestamp_year = NULL,
      timestamp_anuales = NULL,
      año_cacheado = NULL
    ), envir = .GlobalEnv)
  }
  
  # ════════════════════════════════════════════════════════════════════════════
  # FUNCIÓN AUXILIAR: VALIDAR CACHÉ
  # ════════════════════════════════════════════════════════════════════════════
  
  cache_valido <- function(timestamp, max_horas = 24) {
    if (is.null(timestamp)) return(FALSE)
    difftime(Sys.time(), timestamp, units = "hours") < max_horas
  }
  
  # ════════════════════════════════════════════════════════════════════════════
  # REACTIVE: AÑO DEL ÚLTIMO ARCHIVO DISPONIBLE (DINÁMICO)
  # ════════════════════════════════════════════════════════════════════════════
  # 
  # IMPORTANTE: Este reactive determina el año basándose en el archivo más
  # reciente disponible en el catálogo, NO en el año del sistema.
  #
  # Ejemplo: Si el archivo más reciente es derfe_pdln_20250731_base.csv,
  #          entonces año_ultimo_archivo = 2025
  #
  # Esto es crítico porque:
  # - La autoridad electoral libera archivos con retraso
  # - En enero 2026, el último archivo podría seguir siendo de 2025
  # - Las gráficas deben mostrar datos del año del último archivo disponible
  # ════════════════════════════════════════════════════════════════════════════
  
  anio_actual <- reactive({
    if (exists("LNE_CATALOG", envir = .GlobalEnv)) {
      catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
      
      if (length(catalog$historico) > 0) {
        # Obtener el año de la fecha más reciente disponible
        ultima_fecha <- max(catalog$historico)
        año_detectado <- as.integer(format(ultima_fecha, "%Y"))
        message("📅 [anio_actual] Año del último archivo: ", año_detectado, 
                " (fecha: ", ultima_fecha, ")")
        return(año_detectado)
      }
    }
    
    # Fallback: usar año del sistema (solo si no hay catálogo)
    año_sistema <- as.integer(format(Sys.Date(), "%Y"))
    message("⚠️ [anio_actual] Catálogo vacío, usando año del sistema: ", año_sistema)
    return(año_sistema)
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # REACTIVE: ÚLTIMA FECHA DISPONIBLE (COMPLETA)
  # ════════════════════════════════════════════════════════════════════════════
  # Útil para saber exactamente hasta qué mes tenemos datos
  
  ultima_fecha_disponible <- reactive({
    if (exists("LNE_CATALOG", envir = .GlobalEnv)) {
      catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
      
      if (length(catalog$historico) > 0) {
        return(max(catalog$historico))
      }
    }
    return(NULL)
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # REACTIVE: AÑO CONSULTADO - SOLO SE ACTUALIZA CON BOTÓN
  # ════════════════════════════════════════════════════════════════════════════
  
  anio_consultado <- reactive({
    estado_actual <- estado_app()
    
    # Estado RESTABLECIDO: usar año del último archivo
    if (estado_actual == "restablecido") {
      año_ultimo <- anio_actual()
      message("📊 [anio_consultado] Estado RESTABLECIDO → Año último archivo: ", año_ultimo)
      return(año_ultimo)
    }
    
    # Estado CONSULTADO: capturar año seleccionado CON ISOLATE
    if (estado_actual == "consultado") {
      año_seleccionado <- isolate(input$year)
      
      if (!is.null(año_seleccionado)) {
        año_consultado_val <- as.integer(año_seleccionado)
        message("📊 [anio_consultado] Estado CONSULTADO → Año capturado: ", año_consultado_val)
        return(año_consultado_val)
      }
    }
    
    # Fallback: año del último archivo
    año_ultimo <- anio_actual()
    message("📊 [anio_consultado] Fallback → Año último archivo: ", año_ultimo)
    return(año_ultimo)
    
  }) %>%
    bindEvent(estado_app(), input$btn_consultar, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ════════════════════════════════════════════════════════════════════════════
  # REACTIVE: CONTROLAR CUÁNDO MOSTRAR GRÁFICAS ANUALES (1, 2, 3)
  # ════════════════════════════════════════════════════════════════════════════
  # 
  # Gráficas 1, 2, 3 muestran:
  # - Gráfica 1: Proyección mensual del año del último archivo
  # - Gráfica 2: Evolución anual 2017 → año del último archivo
  # - Gráfica 3: Evolución anual por sexo 2017 → año del último archivo
  #
  # Se muestran cuando:
  # - Estado es "restablecido" (carga inicial)
  # - Estado es "consultado" Y el año consultado == año del último archivo
  # ════════════════════════════════════════════════════════════════════════════
  
  mostrar_graficas_anuales <- reactive({
    estado_actual <- estado_app()
    anio_consult <- anio_consultado()
    anio_ultimo <- anio_actual()
    ambito <- isolate(input$ambito_datos %||% "nacional")
    
    message("🔍 [mostrar_graficas_anuales] Estado: ", estado_actual, 
            " | Año consultado: ", anio_consult, 
            " | Año último archivo: ", anio_ultimo, 
            " | Ámbito: ", ambito)
    
    if (estado_actual == "restablecido") {
      message("✅ [mostrar_graficas_anuales] Estado RESTABLECIDO → Mostrar gráficas 1, 2, 3")
      return(TRUE)
    }
    
    if (estado_actual == "consultado") {
      mostrar <- (anio_consult == anio_ultimo)
      message(ifelse(mostrar, "✅", "❌"), " [mostrar_graficas_anuales] Estado CONSULTADO → ", 
              ifelse(mostrar, "Mostrar gráficas 1, 2, 3", "NO mostrar (año diferente)"))
      return(mostrar)
    }
    
    message("❌ [mostrar_graficas_anuales] Estado no reconocido → NO mostrar")
    return(FALSE)
  }) %>%
    bindEvent(estado_app(), input$btn_consultar, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ════════════════════════════════════════════════════════════════════════════
  # REACTIVE: CONTROLAR CUÁNDO MOSTRAR GRÁFICAS 4, 5
  # ════════════════════════════════════════════════════════════════════════════
  #
  # Gráficas 4, 5 muestran:
  # - Evolución mensual de un año ANTERIOR al último archivo
  # - Solo se muestran cuando el usuario consulta un año diferente
  # ════════════════════════════════════════════════════════════════════════════
  
  mostrar_graficas_consultadas <- reactive({
    estado_actual <- estado_app()
    anio_consult <- anio_consultado()
    anio_ultimo <- anio_actual()
    ambito <- isolate(input$ambito_datos %||% "nacional")
    
    message("🔍 [mostrar_graficas_consultadas] Estado: ", estado_actual, 
            " | Año consultado: ", anio_consult, 
            " | Año último archivo: ", anio_ultimo, 
            " | Ámbito: ", ambito)
    
    if (estado_actual == "consultado") {
      mostrar <- (anio_consult != anio_ultimo)
      message(ifelse(mostrar, "✅", "❌"), " [mostrar_graficas_consultadas] Estado CONSULTADO → ", 
              ifelse(mostrar, "Mostrar gráficas 4, 5 (año diferente)", "NO mostrar (mismo año)"))
      return(mostrar)
    }
    
    message("❌ [mostrar_graficas_consultadas] Estado ", estado_actual, " → NO mostrar gráficas 4, 5")
    return(FALSE)
  }) %>%
    bindEvent(estado_app(), input$btn_consultar, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ════════════════════════════════════════════════════════════════════════════
  # REACTIVE: FILTROS ACTUALES DEL USUARIO
  # ════════════════════════════════════════════════════════════════════════════
  
  filtros_usuario <- reactive({
    estado_actual <- estado_app()
    
    # En estado inicial o restablecido, usar valores por defecto
    if (estado_actual %in% c("inicial", "restablecido")) {
      return(list(
        entidad = "Nacional",
        distrito = "Todos",
        municipio = "Todos",
        seccion = "Todas"
      ))
    }
    
    # En estado consultado, usar valores de los inputs
    list(
      entidad = input$entidad %||% "Nacional",
      distrito = input$distrito %||% "Todos",
      municipio = input$municipio %||% "Todos",
      seccion = input$seccion %||% "Todas"
    )
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # REACTIVE: TEXTO DE ALCANCE (para subtítulos de gráficas)
  # ════════════════════════════════════════════════════════════════════════════
  
  texto_alcance <- reactive({
    filtros <- filtros_usuario()
    
    partes <- c()
    partes <- c(partes, paste0("Estado: ", filtros$entidad))
    partes <- c(partes, paste0("Distrito: ", filtros$distrito))
    partes <- c(partes, paste0("Municipio: ", filtros$municipio))
    
    seccion <- filtros$seccion
    if (is.null(seccion) || length(seccion) == 0 || (length(seccion) == 1 && seccion == "Todas")) {
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
    message("📋 [texto_alcance] ", texto)
    return(texto)
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # REACTIVE: ÁMBITO (NACIONAL / EXTRANJERO)
  # ════════════════════════════════════════════════════════════════════════════
  
  ambito_reactivo <- reactive({
    estado_actual <- estado_app()
    
    if (estado_actual %in% c("restablecido", "consultado")) {
      ambito <- input$ambito_datos %||% "nacional"
      message("🔄 [ambito_reactivo] Estado: ", estado_actual, " - Ámbito: ", ambito)
      return(ambito)
    }
    
    message("⏸️ [ambito_reactivo] Estado inicial - usando ámbito por defecto: nacional")
    return("nacional")
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # RETORNAR LISTA DE REACTIVES Y FUNCIONES
  # ════════════════════════════════════════════════════════════════════════════
  
  message("✅ graficas_core v3.0 inicializado")
  message("   📅 Año dinámico desde catálogo (no hardcodeado)")
  message("   🔄 Lazy loading compatible")
  
  return(list(
    anio_actual = anio_actual,
    anio_consultado = anio_consultado,
    ultima_fecha_disponible = ultima_fecha_disponible,
    mostrar_graficas_anuales = mostrar_graficas_anuales,
    mostrar_graficas_consultadas = mostrar_graficas_consultadas,
    filtros_usuario = filtros_usuario,
    texto_alcance = texto_alcance,
    cache_valido = cache_valido,
    ambito_reactivo = ambito_reactivo
  ))
}
