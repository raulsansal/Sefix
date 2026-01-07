# server/datos.R
# Sistema de carga de resultados electorales desde Firebase Storage
# Versión: 2.0

library(data.table)

# Cargar sistema de Firebase
if (file.exists("firebase_loaders.R")) {
  source("firebase_loaders.R")
  message("✅ firebase_loaders.R cargado en datos.R")
} else {
  stop("❌ No se encontró firebase_loaders.R")
}

cargar_datos <- function(anio, cargo, tipo_eleccion, estado, cabecera, municipio, seccion) {
  
  message("🔧 Cargando datos: anio=", anio, ", cargo=", cargo,
          ", tipo_eleccion=", tipo_eleccion %||% "AMBAS")
  
  # Normalizar entradas
  anio <- as.character(anio)
  tipo_eleccion <- tipo_eleccion %||% "AMBAS"
  estado <- toupper(trimws(estado %||% "Nacional"))
  cabecera <- cabecera %||% "Todos"
  municipio <- municipio %||% "Todos"
  seccion <- if (is.null(seccion) || length(seccion) == 0 || "Todas" %in% seccion) "Todas" else seccion
  
  # Validar combinación año-cargo
  valid_combinations <- list(
    "2024" = c("DIPUTACION FEDERAL", "SENADURIA", "PRESIDENCIA"),
    "2023" = c("SENADURIA"),
    "2021" = c("DIPUTACION FEDERAL", "SENADURIA"),
    "2018" = c("DIPUTACION FEDERAL", "SENADURIA", "PRESIDENCIA"),
    "2015" = c("DIPUTACION FEDERAL"),
    "2012" = c("DIPUTACION FEDERAL", "SENADURIA", "PRESIDENCIA"),
    "2009" = c("DIPUTACION FEDERAL"),
    "2006" = c("DIPUTACION FEDERAL", "SENADURIA", "PRESIDENCIA")
  )
  
  if (!anio %in% names(valid_combinations) || !cargo %in% valid_combinations[[anio]]) {
    message("❌ Combinación no válida: anio=", anio, ", cargo=", cargo)
    return(list(
      datos = data.frame(),
      columnas = NULL,
      tiene_ordinaria = FALSE,
      tiene_extraordinaria = FALSE,
      tiene_mayoria_relativa = FALSE,
      tiene_representacion_proporcional = FALSE,
      todos_estados = character(0),
      todos_cabeceras = character(0),
      todos_municipios = character(0),
      todas_secciones = character(0)
    ))
  }
  
  # Mapear cargo a nombre de archivo
  cargo_map <- c("DIPUTACION FEDERAL" = "dip", "SENADURIA" = "sen", "PRESIDENCIA" = "pdte")
  cargo_name <- cargo_map[cargo]
  
  if (is.na(cargo_name)) {
    message("⚠️ Cargo no mapeado: ", cargo)
    return(list(datos = data.frame()))
  }
  
  # ========== CARGAR DESDE FIREBASE ==========
  
  message("📂 Cargando desde Firebase Storage...")
  
  dt <- cargar_resultados_federales_firebase(anio, cargo_name)
  
  if (is.null(dt) || nrow(dt) == 0) {
    message("❌ No se pudo cargar archivo desde Firebase")
    return(list(datos = data.frame()))
  }
  
  message("📊 Filas totales en CSV: ", format(nrow(dt), big.mark = ","))
  
  # Convertir columnas clave a numéricas
  if ("part_ciud" %in% colnames(dt)) {
    dt[, part_ciud := as.numeric(as.character(part_ciud))]
  }
  if ("lne" %in% colnames(dt)) {
    dt[, lne := as.numeric(as.character(lne))]
  }
  if ("total_votos" %in% colnames(dt)) {
    dt[, total_votos := as.numeric(as.character(total_votos))]
  }
  
  # Eliminar filas con part_ciud NA
  dt <- dt[!is.na(part_ciud)]
  
  # Normalizar columna 'estado'
  if ("estado" %in% colnames(dt)) {
    dt[, estado := toupper(trimws(estado))]
  } else {
    message("❌ Columna 'estado' no encontrada")
    return(list(datos = data.frame()))
  }
  
  # Normalizar columna 'tipo'
  if ("tipo" %in% colnames(dt)) {
    dt[, tipo := toupper(trimws(tipo))]
  } else {
    message("⚠️ Columna 'tipo' no encontrada. Asignando 'ORDINARIA'")
    dt[, tipo := "ORDINARIA"]
  }
  
  # Aplicar filtros
  filtros <- TRUE
  
  if (estado != "NACIONAL") {
    filtros <- filtros & dt$estado == estado
  }
  
  if (cabecera != "Todos" && "cabecera" %in% colnames(dt)) {
    filtros <- filtros & dt$cabecera == cabecera
  }
  
  if (municipio != "Todos" && "municipio" %in% colnames(dt)) {
    filtros <- filtros & dt$municipio == municipio
  }
  
  if (!identical(seccion, "Todas") && "seccion" %in% colnames(dt)) {
    filtros <- filtros & dt$seccion %in% seccion
  }
  
  if (!is.null(tipo_eleccion) && tipo_eleccion != "AMBAS" && "tipo" %in% colnames(dt)) {
    filtros <- filtros & dt$tipo == tipo_eleccion
  }
  
  dt <- dt[filtros]
  message("📊 Filas tras filtros: ", nrow(dt))
  
  # Normalizar principio
  if ("principio" %in% colnames(dt)) {
    dt[, principio := toupper(trimws(principio))]
    dt[, principio := fifelse(principio == "MAYORIA RELATIVA", "MAYORÍA RELATIVA", 
                              fifelse(principio == "REPRESENTACION PROPORCIONAL", 
                                      "REPRESENTACIÓN PROPORCIONAL", principio))]
  }
  
  # Asegurar nombres de distrito completos
  if ("cabecera" %in% colnames(dt)) {
    dt[, cabecera := gsub("^0101.*", "0101 JESUS MARIA", cabecera)]
    dt[, cabecera := gsub("^0102.*", "0102 AGUASCALIENTES", cabecera)]
    dt[, cabecera := gsub("^0103.*", "0103 SAN FRANCISCO DE LOS ROMO", cabecera)]
  }
  
  datos <- as.data.frame(dt)
  
  # Obtener listas únicas
  todos_estados <- sort(unique(datos$estado[!is.na(datos$estado)])) %||% character(0)
  todos_cabeceras <- sort(unique(datos$cabecera[!is.na(datos$cabecera)])) %||% character(0)
  todos_municipios <- sort(unique(datos$municipio[!is.na(datos$municipio)])) %||% character(0)
  todas_secciones <- sort(unique(datos$seccion[!is.na(datos$seccion)])) %||% character(0)
  
  # Evaluar tipos de elección
  tiene_ordinaria <- any(datos$tipo == "ORDINARIA", na.rm = TRUE)
  tiene_extraordinaria <- any(datos$tipo == "EXTRAORDINARIA", na.rm = TRUE)
  tiene_mayoria_relativa <- any(datos$principio == "MAYORÍA RELATIVA", na.rm = TRUE)
  tiene_representacion_proporcional <- any(datos$principio == "REPRESENTACIÓN PROPORCIONAL", na.rm = TRUE)
  
  # Retorno
  list(
    datos = datos,
    columnas = colnames(datos) %||% character(0),
    tiene_ordinaria = tiene_ordinaria,
    tiene_extraordinaria = tiene_extraordinaria,
    tiene_mayoria_relativa = tiene_mayoria_relativa,
    tiene_representacion_proporcional = tiene_representacion_proporcional,
    todos_estados = todos_estados,
    todos_cabeceras = todos_cabeceras,
    todos_municipios = todos_municipios,
    todas_secciones = todas_secciones
  )
}

message("✅ datos.R v2.0 cargado (Firebase Storage)")
