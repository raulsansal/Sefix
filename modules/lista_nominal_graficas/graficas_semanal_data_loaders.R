# modules/lista_nominal_graficas/graficas_semanal_data_loaders.R
# Versión: 1.2 — Reactividad directa (sin bindEvent)

graficas_semanal_data_loaders <- function(input, output, session,
                                          estado_app,
                                          filtros_usuario,
                                          ambito_reactivo) {
  
  message("📥 Inicializando graficas_semanal_data_loaders v1.2")
  
  if (!exists("LNE_CACHE_SEMANAL", envir = .GlobalEnv)) {
    assign("LNE_CACHE_SEMANAL", list(edad=NULL, sexo=NULL, origen=NULL, fecha=NULL, timestamp=NULL), envir=.GlobalEnv)
    message("📦 Caché semanal inicializado")
  }
  
  cache_semanal_valido <- function(fecha_requerida) {
    cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
    if (is.null(cache$timestamp) || is.null(cache$fecha)) return(FALSE)
    if (!identical(cache$fecha, fecha_requerida)) return(FALSE)
    difftime(Sys.time(), cache$timestamp, units = "hours") < 24
  }
  obtener_fecha_semanal_reciente <- function() {
    if (!exists("LNE_CATALOG", envir = .GlobalEnv)) return(NULL)
    catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
    if (length(catalog$semanal_comun) == 0) return(NULL)
    as.Date(max(catalog$semanal_comun), origin = "1970-01-01")
  }
  es_nacional_sin_filtros <- function(f) {
    f$entidad == "Nacional" && f$distrito == "Todos" && f$municipio == "Todos" &&
      (is.null(f$seccion) || "Todas" %in% f$seccion || length(f$seccion) == 0)
  }
  cargar_tipo_semanal <- function(fecha, tipo, filtros) {
    tryCatch({
      res <- cargar_lne(tipo_corte="semanal", fecha=fecha, dimension=tipo,
                        estado=filtros$entidad %||% "Nacional", distrito=filtros$distrito %||% "Todos",
                        municipio=filtros$municipio %||% "Todos", seccion=filtros$seccion %||% "Todas",
                        incluir_extranjero=TRUE)
      if (!is.null(res) && !is.null(res$datos) && nrow(res$datos) > 0) return(res$datos)
      return(NULL)
    }, error = function(e) { message("❌ Error semanal/", tipo, ": ", e$message); return(NULL) })
  }
  filtros_defecto <- list(entidad="Nacional", distrito="Todos", municipio="Todos", seccion="Todas")
  
  fecha_semanal_efectiva <- reactive({
    tipo_corte_val <- input$tipo_corte %||% "historico"
    if (tipo_corte_val != "semanal") return(NULL)
    estado_actual <- estado_app()
    if (estado_actual %in% c("restablecido", "inicial")) {
      fecha <- obtener_fecha_semanal_reciente()
      message("📅 [semanal] estado=", estado_actual, " → fecha: ", fecha)
      return(fecha)
    }
    if (estado_actual == "consultado") {
      btn <- input$btn_consultar
      year_sel <- isolate(input$year)
      if (is.null(year_sel) || is.na(year_sel)) return(obtener_fecha_semanal_reciente())
      if (!exists("LNE_CATALOG", envir = .GlobalEnv)) return(NULL)
      catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
      fechas <- catalog$semanal_comun[format(catalog$semanal_comun, "%Y") == as.character(year_sel)]
      if (length(fechas) == 0) { message("⚠️ Sin fechas para año ", year_sel); return(NULL) }
      fecha <- as.Date(max(fechas), origin = "1970-01-01")
      message("📅 [semanal] CONSULTADO → ", fecha)
      return(fecha)
    }
    return(NULL)
  })
  
  anio_semanal <- reactive({
    fecha <- fecha_semanal_efectiva()
    if (is.null(fecha)) return(as.integer(format(Sys.Date(), "%Y")))
    as.integer(format(fecha, "%Y"))
  })
  
  datos_semanal_edad <- reactive({
    tipo_corte_val <- input$tipo_corte %||% "historico"
    if (tipo_corte_val != "semanal") return(NULL)
    fecha <- fecha_semanal_efectiva()
    if (is.null(fecha)) return(NULL)
    estado_actual <- estado_app()
    filtros <- if (estado_actual == "consultado") { btn <- input$btn_consultar; isolate(filtros_usuario()) } else filtros_defecto
    if (es_nacional_sin_filtros(filtros) && cache_semanal_valido(fecha)) {
      cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
      if (!is.null(cache$edad)) { message("✅ [CACHÉ] edad semanal"); return(cache$edad) }
    }
    message("📥 [semanal_edad] ", format(fecha, "%Y%m%d"))
    datos <- cargar_tipo_semanal(fecha, "edad", filtros)
    if (!is.null(datos) && es_nacional_sin_filtros(filtros)) {
      cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
      cache$edad <- datos; cache$fecha <- fecha; cache$timestamp <- Sys.time()
      assign("LNE_CACHE_SEMANAL", cache, envir = .GlobalEnv)
    }
    if (!is.null(datos)) message("✅ [semanal_edad] ", nrow(datos), " filas")
    return(datos)
  })
  
  datos_semanal_sexo <- reactive({
    tipo_corte_val <- input$tipo_corte %||% "historico"
    if (tipo_corte_val != "semanal") return(NULL)
    fecha <- fecha_semanal_efectiva()
    if (is.null(fecha)) return(NULL)
    estado_actual <- estado_app()
    filtros <- if (estado_actual == "consultado") { btn <- input$btn_consultar; isolate(filtros_usuario()) } else filtros_defecto
    if (es_nacional_sin_filtros(filtros) && cache_semanal_valido(fecha)) {
      cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
      if (!is.null(cache$sexo)) { message("✅ [CACHÉ] sexo semanal"); return(cache$sexo) }
    }
    message("📥 [semanal_sexo] ", format(fecha, "%Y%m%d"))
    datos <- cargar_tipo_semanal(fecha, "sexo", filtros)
    if (!is.null(datos) && es_nacional_sin_filtros(filtros)) {
      cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
      cache$sexo <- datos; cache$fecha <- fecha; cache$timestamp <- Sys.time()
      assign("LNE_CACHE_SEMANAL", cache, envir = .GlobalEnv)
    }
    if (!is.null(datos)) message("✅ [semanal_sexo] ", nrow(datos), " filas")
    return(datos)
  })
  
  datos_semanal_origen <- reactive({
    tipo_corte_val <- input$tipo_corte %||% "historico"
    if (tipo_corte_val != "semanal") return(NULL)
    fecha <- fecha_semanal_efectiva()
    if (is.null(fecha)) return(NULL)
    estado_actual <- estado_app()
    filtros <- if (estado_actual == "consultado") { btn <- input$btn_consultar; isolate(filtros_usuario()) } else filtros_defecto
    if (es_nacional_sin_filtros(filtros) && cache_semanal_valido(fecha)) {
      cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
      if (!is.null(cache$origen)) { message("✅ [CACHÉ] origen semanal"); return(cache$origen) }
    }
    message("📥 [semanal_origen] ", format(fecha, "%Y%m%d"))
    datos <- cargar_tipo_semanal(fecha, "origen", filtros)
    if (!is.null(datos) && es_nacional_sin_filtros(filtros)) {
      cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
      cache$origen <- datos; cache$fecha <- fecha; cache$timestamp <- Sys.time()
      assign("LNE_CACHE_SEMANAL", cache, envir = .GlobalEnv)
    }
    if (!is.null(datos)) message("✅ [semanal_origen] ", nrow(datos), " filas")
    return(datos)
  })
  
  message("✅ graficas_semanal_data_loaders v1.2 inicializado")
  message("   ✅ Reactividad directa (sin bindEvent)")
  
  return(list(
    datos_semanal_edad     = datos_semanal_edad,
    datos_semanal_sexo     = datos_semanal_sexo,
    datos_semanal_origen   = datos_semanal_origen,
    fecha_semanal_efectiva = fecha_semanal_efectiva,
    anio_semanal           = anio_semanal
  ))
}
