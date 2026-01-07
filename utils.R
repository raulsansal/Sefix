# utils.R
# Utilidades generales para el dashboard
# Versión: 2.0 - Simplificado para Firebase

#' Función obsoleta - datos ahora en Firebase Storage
#' Mantenida para compatibilidad con código existente
find_data_dir <- function() {
  warning("⚠️ find_data_dir() está obsoleta. Los datos ahora se cargan desde Firebase Storage.")
  return("data")  # Placeholder
}

#' Función obsoleta - datos ahora en Firebase Storage
get_data_file_path <- function(archivo) {
  warning("⚠️ get_data_file_path() está obsoleta. Los datos ahora se cargan desde Firebase Storage.")
  return(file.path("data", archivo))  # Placeholder
}

message("✅ utils.R v2.0 cargado (Compatibilidad)")
message("   ⚠️ Funciones de rutas locales obsoletas - usando Firebase Storage")
