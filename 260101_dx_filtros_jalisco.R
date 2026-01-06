# Script de diagnóstico para verificar valores NB con filtros
# Año: 2024, Estado: Jalisco, Filtros: Todos

library(readr)

# ========== CARGAR ARCHIVO 2024 (ÚLTIMO MES) ==========

archivo_2024 <- read_csv(
  "/Volumes/Salamandra/dashboard_rss/data/pdln/historico/derfe_pdln_20241231_base.csv",
  show_col_types = FALSE
)

message("\n", strrep("=", 70))
message("📂 ARCHIVO: derfe_pdln_20241231_base.csv")
message(strrep("=", 70))
message("Dimensiones: ", nrow(archivo_2024), " filas x ", ncol(archivo_2024), " columnas")

# ========== NORMALIZAR NOMBRES DE COLUMNAS ==========

colnames(archivo_2024) <- tolower(colnames(archivo_2024))
colnames(archivo_2024) <- gsub("\\s+", "_", colnames(archivo_2024))
colnames(archivo_2024) <- gsub("[áàäâ]", "a", colnames(archivo_2024))
colnames(archivo_2024) <- gsub("[éèëê]", "e", colnames(archivo_2024))
colnames(archivo_2024) <- gsub("[íìïî]", "i", colnames(archivo_2024))
colnames(archivo_2024) <- gsub("[óòöô]", "o", colnames(archivo_2024))
colnames(archivo_2024) <- gsub("[úùüû]", "u", colnames(archivo_2024))
colnames(archivo_2024) <- gsub("ñ", "n", colnames(archivo_2024))

message("\n📋 Columnas disponibles:")
message("   ", paste(head(names(archivo_2024), 15), collapse = ", "))

# ========== ELIMINAR FILA TOTALES ==========

idx_totales <- which(is.na(archivo_2024$cve_entidad))

if (length(idx_totales) > 0) {
  message("\n🗑️ Eliminando fila de totales (índice: ", idx_totales[1], ")")
  archivo_2024 <- archivo_2024[-idx_totales[1], ]
} else {
  message("\n⚠️ No se encontró fila de totales")
}

message("Filas después de eliminar totales: ", nrow(archivo_2024))

# ========== APLICAR FILTRO: JALISCO ==========

# Buscar columna de entidad
if ("nombre_entidad" %in% names(archivo_2024)) {
  archivo_jalisco <- archivo_2024[toupper(archivo_2024$nombre_entidad) == "JALISCO", ]
  message("\n🔍 Filtro aplicado: Estado = JALISCO")
  message("   Filas resultantes: ", nrow(archivo_jalisco))
} else {
  message("\n❌ No se encontró columna 'nombre_entidad'")
  archivo_jalisco <- archivo_2024
}

# ========== VERIFICAR COLUMNAS NB ==========

tiene_padron_nb_nac <- "padron_nacional_no_binario" %in% names(archivo_jalisco)
tiene_lista_nb_nac <- "lista_nacional_no_binario" %in% names(archivo_jalisco)
tiene_padron_nb_ext <- "padron_extranjero_no_binario" %in% names(archivo_jalisco)
tiene_lista_nb_ext <- "lista_extranjero_no_binario" %in% names(archivo_jalisco)

message("\n🔍 Columnas NB disponibles:")
message("   padron_nacional_no_binario: ", ifelse(tiene_padron_nb_nac, "✅ SÍ", "❌ NO"))
message("   lista_nacional_no_binario: ", ifelse(tiene_lista_nb_nac, "✅ SÍ", "❌ NO"))
message("   padron_extranjero_no_binario: ", ifelse(tiene_padron_nb_ext, "✅ SÍ", "❌ NO"))
message("   lista_extranjero_no_binario: ", ifelse(tiene_lista_nb_ext, "✅ SÍ", "❌ NO"))

# ========== CALCULAR TOTALES SUMANDO FILAS ==========

if (tiene_padron_nb_nac && tiene_lista_nb_nac) {
  
  # Convertir a numérico
  archivo_jalisco$padron_nacional_no_binario <- as.numeric(archivo_jalisco$padron_nacional_no_binario)
  archivo_jalisco$lista_nacional_no_binario <- as.numeric(archivo_jalisco$lista_nacional_no_binario)
  
  # Sumar todas las filas
  total_padron_nb <- sum(archivo_jalisco$padron_nacional_no_binario, na.rm = TRUE)
  total_lista_nb <- sum(archivo_jalisco$lista_nacional_no_binario, na.rm = TRUE)
  
  message("\n", strrep("=", 70))
  message("📊 TOTALES NO BINARIO PARA JALISCO (2024)")
  message(strrep("=", 70))
  message("Padrón Nacional NB: ", total_padron_nb)
  message("Lista Nacional NB: ", total_lista_nb)
  message("Total casos: ", total_padron_nb + total_lista_nb)
  
} else {
  message("\n❌ No se pueden calcular totales - columnas NB no disponibles")
}

# ========== VERIFICAR TAMBIÉN PARA NACIONAL (SIN FILTROS) ==========

if (tiene_padron_nb_nac && tiene_lista_nb_nac) {
  
  # Convertir a numérico
  archivo_2024$padron_nacional_no_binario <- as.numeric(archivo_2024$padron_nacional_no_binario)
  archivo_2024$lista_nacional_no_binario <- as.numeric(archivo_2024$lista_nacional_no_binario)
  
  # Sumar todas las filas (sin filtro de Jalisco)
  total_padron_nb_nacional <- sum(archivo_2024$padron_nacional_no_binario, na.rm = TRUE)
  total_lista_nb_nacional <- sum(archivo_2024$lista_nacional_no_binario, na.rm = TRUE)
  
  message("\n", strrep("=", 70))
  message("📊 TOTALES NO BINARIO PARA TODO MÉXICO (2024) - REFERENCIA")
  message(strrep("=", 70))
  message("Padrón Nacional NB: ", total_padron_nb_nacional)
  message("Lista Nacional NB: ", total_lista_nb_nacional)
  message("Total casos: ", total_padron_nb_nacional + total_lista_nb_nacional)
  message("\n✅ Este valor debe coincidir con el de la última fila de totales")
  message("   (derfe_pdln_20241231_base.csv, fila con cve_entidad = NA)")
  
}

# ========== VERIFICAR DISTRIBUCIÓN POR DISTRITO EN JALISCO ==========

if ("cve_distrito" %in% names(archivo_jalisco) && tiene_padron_nb_nac) {
  
  message("\n", strrep("=", 70))
  message("📊 DISTRIBUCIÓN POR DISTRITO EN JALISCO")
  message(strrep("=", 70))
  
  # Agrupar por distrito
  distritos_nb <- aggregate(
    cbind(padron_nacional_no_binario, lista_nacional_no_binario) ~ cve_distrito,
    data = archivo_jalisco,
    FUN = function(x) sum(x, na.rm = TRUE)
  )
  
  # Ordenar por padrón
  distritos_nb <- distritos_nb[order(-distritos_nb$padron_nacional_no_binario), ]
  
  # Mostrar top 10 distritos
  message("\nTop 10 distritos con más registros NB:")
  for (i in 1:min(10, nrow(distritos_nb))) {
    padron <- distritos_nb$padron_nacional_no_binario[i]
    lista <- distritos_nb$lista_nacional_no_binario[i]
    distrito <- distritos_nb$cve_distrito[i]
    message(sprintf("   Distrito %02d: Padrón=%d, Lista=%d", distrito, padron, lista))
  }
  
  message("\nTotal distritos con casos NB: ", sum(distritos_nb$padron_nacional_no_binario + distritos_nb$lista_nacional_no_binario > 0))
}

message("\n", strrep("=", 70))
message("✅ DIAGNÓSTICO COMPLETADO")
message(strrep("=", 70))