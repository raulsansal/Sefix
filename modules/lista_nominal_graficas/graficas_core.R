# modules/lista_nominal_graficas/graficas_core.R
# Reactives base: caché, filtros, año, estado
# Versión: 2.7 - CORRECCIÓN CRÍTICA: texto_alcance usa filtros_usuario en lugar de input

graficas_core <- function(input, output, session, estado_app) {
  
  message("📦 Inicializando graficas_core v2.7")
  
  # ========== SISTEMA DE CACHÉ GLOBAL ==========
  
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
  
  # ========== FUNCIÓN AUXILIAR: VALIDAR CACHÉ ==========
  
  cache_valido <- function(timestamp, max_horas = 24) {
    if (is.null(timestamp)) return(FALSE)
    difftime(Sys.time(), timestamp, units = "hours") < max_horas
  }
  
  # ========== REACTIVE: OBTENER AÑO ACTUAL (DINÁMICO) ==========
  
  anio_actual <- reactive({
    # ✅ CORRECCIÓN CRÍTICA: Detectar año actual automáticamente desde catálogo
    if (exists("LNE_CATALOG", envir = .GlobalEnv)) {
      catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
      
      if (length(catalog$historico) > 0) {
        # Obtener el año de la fecha más reciente disponible
        ultima_fecha <- max(catalog$historico)
        año_detectado <- as.integer(format(ultima_fecha, "%Y"))
        message("📅 [anio_actual] Año detectado automáticamente: ", año_detectado)
        return(año_detectado)
      }
    }
    
    # Fallback: usar año del sistema
    año_sistema <- as.integer(format(Sys.Date(), "%Y"))
    message("⚠️ [anio_actual] Usando año del sistema: ", año_sistema)
    return(año_sistema)
  })
  
  # ========== REACTIVE: AÑO CONSULTADO - SOLO SE ACTUALIZA CON BOTÓN ==========
  # ✅ v2.4 CORRECCIÓN CRÍTICA: Usar isolate() para evitar reactividad a input$year
  
  anio_consultado <- reactive({
    estado_actual <- estado_app()
    
    # Estado RESTABLECIDO: usar año actual
    if (estado_actual == "restablecido") {
      año_actual_val <- anio_actual()
      message("📊 [anio_consultado] Estado RESTABLECIDO → Año actual: ", año_actual_val)
      return(año_actual_val)
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
    
    # Fallback: año actual
    año_actual_val <- anio_actual()
    message("📊 [anio_consultado] Fallback → Año actual: ", año_actual_val)
    return(año_actual_val)
    
  }) %>%
    # ✅ CRÍTICO: Solo se ejecuta cuando cambia estado_app o se presiona botón
    bindEvent(estado_app(), input$btn_consultar, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ========== REACTIVE: CONTROLAR CUÁNDO MOSTRAR GRÁFICAS ANUALES (1, 2, 3) ==========
  
  mostrar_graficas_anuales <- reactive({
    estado_actual <- estado_app()
    anio_consult <- anio_consultado()
    anio_actual_val <- anio_actual()
    ambito <- isolate(input$ambito_datos %||% "nacional")
    
    message("🔍 [mostrar_graficas_anuales] Estado: ", estado_actual, " | Año consultado: ", anio_consult, " | Año actual: ", anio_actual_val, " | Ámbito: ", ambito)
    
    if (estado_actual == "restablecido") {
      message("✅ [mostrar_graficas_anuales] Estado RESTABLECIDO → Mostrar gráficas 1, 2, 3")
      return(TRUE)
    }
    
    if (estado_actual == "consultado") {
      mostrar <- (anio_consult == anio_actual_val)
      message(ifelse(mostrar, "✅", "❌"), " [mostrar_graficas_anuales] Estado CONSULTADO → ", 
              ifelse(mostrar, "Mostrar gráficas 1, 2, 3", "NO mostrar gráficas 1, 2, 3"))
      return(mostrar)
    }
    
    message("❌ [mostrar_graficas_anuales] Estado no reconocido → NO mostrar")
    return(FALSE)
  }) %>%
    bindEvent(estado_app(), input$btn_consultar, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ========== REACTIVE: CONTROLAR CUÁNDO MOSTRAR GRÁFICAS 4, 5 ==========
  
  mostrar_graficas_consultadas <- reactive({
    estado_actual <- estado_app()
    anio_consult <- anio_consultado()
    anio_actual_val <- anio_actual()
    ambito <- isolate(input$ambito_datos %||% "nacional")
    
    message("🔍 [mostrar_graficas_consultadas] Estado: ", estado_actual, " | Año consultado: ", anio_consult, " | Año actual: ", anio_actual_val, " | Ámbito: ", ambito)
    
    if (estado_actual == "consultado") {
      mostrar <- (anio_consult != anio_actual_val)
      message(ifelse(mostrar, "✅", "❌"), " [mostrar_graficas_consultadas] Estado CONSULTADO → ", 
              ifelse(mostrar, "Mostrar gráficas 4, 5", "NO mostrar gráficas 4, 5"))
      return(mostrar)
    }
    
    message("❌ [mostrar_graficas_consultadas] Estado ", estado_actual, " → NO mostrar gráficas 4, 5")
    return(FALSE)
  }) %>%
    bindEvent(estado_app(), input$btn_consultar, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # ========== REACTIVE: FILTROS ACTUALES DEL USUARIO ==========
  
  filtros_usuario <- reactive({
    estado_actual <- estado_app()
    
    # ✅ v2.7: Reaccionar a cambios en estado
    if (estado_actual %in% c("inicial", "restablecido")) {
      return(list(
        entidad = "Nacional",
        distrito = "Todos",
        municipio = "Todos",
        seccion = "Todas"
      ))
    }
    
    list(
      entidad = input$entidad %||% "Nacional",
      distrito = input$distrito %||% "Todos",
      municipio = input$municipio %||% "Todos",
      seccion = input$seccion %||% "Todas"
    )
  })
  
  # ========== REACTIVE: TEXTO DE ALCANCE ==========
  # ✅ v2.7 CORRECCIÓN CRÍTICA: Usar filtros_usuario() en lugar de input
  
  texto_alcance <- reactive({
    # Leer filtros del reactive
    filtros <- filtros_usuario()
    
    # Construir texto
    partes <- c()
    partes <- c(partes, paste0("Estado: ", filtros$entidad))
    partes <- c(partes, paste0("Distrito: ", filtros$distrito))
    partes <- c(partes, paste0("Municipio: ", filtros$municipio))
    
    seccion <- filtros$seccion
    if (is.null(seccion) || length(seccion) == 0 || seccion == "Todas") {
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
  
  # ========== ✅ v2.6: REACTIVE PARA ÁMBITO (DISPARA EN RESTABLECIDO Y CONSULTADO) ==========
  
  ambito_reactivo <- reactive({
    estado_actual <- estado_app()
    
    # ✅ CORRECCIÓN v2.6: Reaccionar si ya salimos del estado inicial
    # Esto incluye: estado "restablecido" (carga automática) O estado "consultado" (consulta manual)
    if (estado_actual %in% c("restablecido", "consultado")) {
      ambito <- input$ambito_datos %||% "nacional"
      message("🔄 [ambito_reactivo] Estado: ", estado_actual, " - Ámbito: ", ambito)
      return(ambito)
    }
    
    # En estado inicial, retornar valor por defecto (evita renderizado prematuro)
    message("⏸️ [ambito_reactivo] Estado inicial - usando ámbito por defecto: nacional")
    return("nacional")
  })
  
  # ========== RETORNAR LISTA DE REACTIVES Y FUNCIONES ==========
  
  message("✅ graficas_core v2.7 inicializado")
  message("   ✅ CORRECCIÓN v2.7: texto_alcance usa filtros_usuario (reactivo a estado)")
  message("   ✅ MANTIENE v2.6: ambito_reactivo reacciona en estado 'restablecido'")
  
  return(list(
    anio_actual = anio_actual,
    anio_consultado = anio_consultado,
    mostrar_graficas_anuales = mostrar_graficas_anuales,
    mostrar_graficas_consultadas = mostrar_graficas_consultadas,
    filtros_usuario = filtros_usuario,
    texto_alcance = texto_alcance,
    cache_valido = cache_valido,
    ambito_reactivo = ambito_reactivo
  ))
}
