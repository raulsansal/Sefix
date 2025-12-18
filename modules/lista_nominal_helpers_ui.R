# modules/lista_nominal_helpers_ui.R
# Funciones auxiliares para obtener opciones de filtros geográficos
# Versión: 1.2 - CORRECCIÓN: Validación robusta de fechas antes de operaciones

# ========== FUNCIÓN: OBTENER LISTA DE ENTIDADES ==========
# Retorna lista estática de los 32 estados + Nacional
# NO requiere cargar archivos

get_entidades <- function() {
  # Mapa completo de entidades (sin EXTRANJERO que es código 33)
  entidades_completas <- c(
    "AGUASCALIENTES", "BAJA CALIFORNIA", "BAJA CALIFORNIA SUR",
    "CAMPECHE", "COAHUILA", "COLIMA", "CHIAPAS",
    "CHIHUAHUA", "CIUDAD DE MEXICO", "DURANGO",
    "GUANAJUATO", "GUERRERO", "HIDALGO", "JALISCO",
    "MEXICO", "MICHOACAN", "MORELOS", "NAYARIT",
    "NUEVO LEON", "OAXACA", "PUEBLA", "QUERETARO",
    "QUINTANA ROO", "SAN LUIS POTOSI", "SINALOA",
    "SONORA", "TABASCO", "TAMAULIPAS", "TLAXCALA",
    "VERACRUZ", "YUCATAN", "ZACATECAS"
  )
  
  return(c("Nacional", entidades_completas))
}

# ========== FUNCIÓN AUXILIAR: VALIDAR Y CONVERTIR FECHA ==========
# Asegura que la fecha sea un objeto Date válido
validar_fecha <- function(fecha, nombre_funcion = "funcion") {
  
  # Validar NULL
  if (is.null(fecha)) {
    message("⚠️ [", nombre_funcion, "] Fecha es NULL")
    return(NULL)
  }
  
  # Si ya es Date, validar que no sea NA
  if (inherits(fecha, "Date")) {
    if (is.na(fecha)) {
      message("⚠️ [", nombre_funcion, "] Fecha es NA")
      return(NULL)
    }
    return(fecha)
  }
  
  # Si es character, convertir
  if (is.character(fecha)) {
    if (fecha == "" || fecha == "Sin datos") {
      message("⚠️ [", nombre_funcion, "] Fecha es string vacío o 'Sin datos'")
      return(NULL)
    }
    
    fecha_convertida <- tryCatch({
      as.Date(fecha)
    }, error = function(e) {
      message("❌ [", nombre_funcion, "] Error convirtiendo fecha character: ", e$message)
      return(NULL)
    })
    
    if (is.null(fecha_convertida) || is.na(fecha_convertida)) {
      message("❌ [", nombre_funcion, "] Conversión resultó en NULL o NA")
      return(NULL)
    }
    
    return(fecha_convertida)
  }
  
  # Tipo no soportado
  message("❌ [", nombre_funcion, "] Fecha tiene tipo no soportado: ", class(fecha))
  return(NULL)
}

# ========== FUNCIÓN: OBTENER DISTRITOS POR ENTIDAD ==========
# Carga datos mínimos para obtener lista de distritos
# Parámetros:
#   - entidad: Nombre del estado
#   - fecha: Fecha de corte (para encontrar el archivo correcto)

get_distritos_por_entidad <- function(entidad, fecha) {
  
  # Si es Nacional, retornar solo "Todos"
  if (entidad == "Nacional") {
    return(c("Todos"))
  }
  
  # ✅ v1.2: Validación robusta de fecha
  fecha_validada <- validar_fecha(fecha, "get_distritos")
  
  if (is.null(fecha_validada)) {
    message("⚠️ [get_distritos] Fecha inválida, retornando 'Todos'")
    return(c("Todos"))
  }
  
  # Cargar datos solo del estado seleccionado
  datos_estado <- tryCatch({
    cargar_lne(
      tipo_corte = "historico",
      fecha = fecha_validada,
      dimension = "completo",
      estado = entidad,
      distrito = "Todos",
      municipio = "Todos",
      seccion = "Todas",
      incluir_extranjero = TRUE
    )
  }, error = function(e) {
    message("❌ [get_distritos] Error cargando datos: ", e$message)
    return(NULL)
  })
  
  if (is.null(datos_estado) || is.null(datos_estado$datos) || nrow(datos_estado$datos) == 0) {
    message("⚠️ [get_distritos] Sin datos para ", entidad)
    return(c("Todos"))
  }
  
  # Extraer distritos únicos
  if ("cabecera_distrital" %in% colnames(datos_estado$datos)) {
    distritos_unicos <- sort(unique(datos_estado$datos$cabecera_distrital))
    distritos_unicos <- distritos_unicos[distritos_unicos != "RESIDENTES EXTRANJERO"]
    distritos_unicos <- distritos_unicos[!is.na(distritos_unicos)]
    
    if (length(distritos_unicos) > 0) {
      message("✅ [get_distritos] ", entidad, ": ", length(distritos_unicos), " distritos encontrados")
      return(c("Todos", distritos_unicos))
    }
  }
  
  message("⚠️ [get_distritos] No se encontró columna cabecera_distrital")
  return(c("Todos"))
}

# ========== FUNCIÓN: OBTENER MUNICIPIOS POR DISTRITO ==========
# Carga datos mínimos para obtener lista de municipios
# Parámetros:
#   - entidad: Nombre del estado
#   - distrito: Nombre del distrito
#   - fecha: Fecha de corte

get_municipios_por_distrito <- function(entidad, distrito, fecha) {
  
  # Si es Nacional o Todos, retornar solo "Todos"
  if (entidad == "Nacional" || distrito == "Todos") {
    return(c("Todos"))
  }
  
  # ✅ v1.2: Validación robusta de fecha
  fecha_validada <- validar_fecha(fecha, "get_municipios")
  
  if (is.null(fecha_validada)) {
    message("⚠️ [get_municipios] Fecha inválida, retornando 'Todos'")
    return(c("Todos"))
  }
  
  # Cargar datos del distrito seleccionado
  datos_distrito <- tryCatch({
    cargar_lne(
      tipo_corte = "historico",
      fecha = fecha_validada,
      dimension = "completo",
      estado = entidad,
      distrito = distrito,
      municipio = "Todos",
      seccion = "Todas",
      incluir_extranjero = TRUE
    )
  }, error = function(e) {
    message("❌ [get_municipios] Error cargando datos: ", e$message)
    return(NULL)
  })
  
  if (is.null(datos_distrito) || is.null(datos_distrito$datos) || nrow(datos_distrito$datos) == 0) {
    message("⚠️ [get_municipios] Sin datos para ", entidad, " - ", distrito)
    return(c("Todos"))
  }
  
  # Extraer municipios únicos
  if ("nombre_municipio" %in% colnames(datos_distrito$datos)) {
    municipios_unicos <- sort(unique(datos_distrito$datos$nombre_municipio))
    municipios_unicos <- municipios_unicos[municipios_unicos != "RESIDENTES EXTRANJERO"]
    municipios_unicos <- municipios_unicos[!is.na(municipios_unicos)]
    
    if (length(municipios_unicos) > 0) {
      message("✅ [get_municipios] ", distrito, ": ", length(municipios_unicos), " municipios encontrados")
      return(c("Todos", municipios_unicos))
    }
  }
  
  return(c("Todos"))
}

# ========== FUNCIÓN: OBTENER SECCIONES POR MUNICIPIO ==========
# Carga datos mínimos para obtener lista de secciones
# Parámetros:
#   - entidad: Nombre del estado
#   - distrito: Nombre del distrito
#   - municipio: Nombre del municipio
#   - fecha: Fecha de corte

get_secciones_por_municipio <- function(entidad, distrito, municipio, fecha) {
  
  # Si es Nacional, Todos, retornar solo "Todas"
  if (entidad == "Nacional" || distrito == "Todos" || municipio == "Todos") {
    return(c("Todas"))
  }
  
  # ✅ v1.2: Validación robusta de fecha
  fecha_validada <- validar_fecha(fecha, "get_secciones")
  
  if (is.null(fecha_validada)) {
    message("⚠️ [get_secciones] Fecha inválida, retornando 'Todas'")
    return(c("Todas"))
  }
  
  # Cargar datos del municipio seleccionado
  datos_municipio <- tryCatch({
    cargar_lne(
      tipo_corte = "historico",
      fecha = fecha_validada,
      dimension = "completo",
      estado = entidad,
      distrito = distrito,
      municipio = municipio,
      seccion = "Todas",
      incluir_extranjero = TRUE
    )
  }, error = function(e) {
    message("❌ [get_secciones] Error cargando datos: ", e$message)
    return(NULL)
  })
  
  if (is.null(datos_municipio) || is.null(datos_municipio$datos) || nrow(datos_municipio$datos) == 0) {
    message("⚠️ [get_secciones] Sin datos para ", municipio)
    return(c("Todas"))
  }
  
  # Extraer secciones únicas
  if ("seccion" %in% colnames(datos_municipio$datos)) {
    secciones_unicas <- sort(unique(as.character(datos_municipio$datos$seccion)))
    secciones_unicas <- secciones_unicas[secciones_unicas != "0"]
    secciones_unicas <- secciones_unicas[!is.na(secciones_unicas)]
    
    if (length(secciones_unicas) > 0) {
      message("✅ [get_secciones] ", municipio, ": ", length(secciones_unicas), " secciones encontradas")
      return(c("Todas", secciones_unicas))
    }
  }
  
  return(c("Todas"))
}

message("✅ lista_nominal_helpers_ui.R v1.2 cargado (CORRECCIÓN: Validación robusta de fechas)")
