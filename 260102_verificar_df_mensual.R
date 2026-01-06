# Script para verificar valores mensuales NB en Jalisco 2024
# Este script simula lo que debería recibir crear_card_no_binario()

library(data.table)

# ========== SIMULAR CARGA DE DATOS COMO LO HACE graficas_data_loaders.R ==========

catalog <- list(
  historico = as.Date(c(
    "2024-01-31", "2024-02-29", "2024-03-31", "2024-05-31", 
    "2024-06-02", "2024-06-30", "2024-07-31", "2024-08-31",
    "2024-09-30", "2024-10-31", "2024-11-30", "2024-12-31"
  ))
)

fechas_2024 <- catalog$historico

message("\n", strrep("=", 70))
message("📅 ARCHIVOS DISPONIBLES PARA 2024")
message(strrep("=", 70))
for (i in seq_along(fechas_2024)) {
  fecha <- as.Date(fechas_2024[i], origin = "1970-01-01")
  archivo <- paste0("derfe_pdln_", format(fecha, "%Y%m%d"), "_base.csv")
  message("   ", archivo)
}

# ========== CARGAR CADA MES Y EXTRAER VALORES NB ==========

lista_meses <- list()

for (i in seq_along(fechas_2024)) {
  fecha <- as.Date(fechas_2024[i], origin = "1970-01-01")
  archivo_path <- paste0(
    "/Volumes/Salamandra/dashboard_rss/data/pdln/historico/derfe_pdln_",
    format(fecha, "%Y%m%d"),
    "_base.csv"
  )
  
  if (!file.exists(archivo_path)) {
    message("⚠️ Archivo no existe: ", basename(archivo_path))
    next
  }
  
  # Cargar archivo
  dt <- fread(archivo_path, encoding = "Latin-1", showProgress = FALSE)
  
  # Normalizar nombres
  colnames(dt) <- tolower(colnames(dt))
  colnames(dt) <- gsub("\\s+", "_", colnames(dt))
  colnames(dt) <- gsub("[áàäâ]", "a", colnames(dt))
  colnames(dt) <- gsub("[éèëê]", "e", colnames(dt))
  colnames(dt) <- gsub("[íìïî]", "i", colnames(dt))
  colnames(dt) <- gsub("[óòöô]", "o", colnames(dt))
  colnames(dt) <- gsub("[úùüû]", "u", colnames(dt))
  colnames(dt) <- gsub("ñ", "n", colnames(dt))
  
  # Eliminar fila totales
  idx_totales <- which(is.na(dt$cve_entidad))
  if (length(idx_totales) > 0) {
    dt <- dt[-idx_totales[1], ]
  }
  
  # Filtrar Jalisco
  dt_jalisco <- dt[toupper(dt$nombre_entidad) == "JALISCO", ]
  
  # Verificar columnas NB
  tiene_nb <- "padron_nacional_no_binario" %in% colnames(dt_jalisco) &&
    "lista_nacional_no_binario" %in% colnames(dt_jalisco)
  
  if (!tiene_nb) {
    message("⚠️ ", format(fecha, "%b %Y"), ": Columnas NB no disponibles")
    next
  }
  
  # Convertir a numérico y sumar
  dt_jalisco$padron_nacional_no_binario <- as.numeric(dt_jalisco$padron_nacional_no_binario)
  dt_jalisco$lista_nacional_no_binario <- as.numeric(dt_jalisco$lista_nacional_no_binario)
  
  padron_nb <- sum(dt_jalisco$padron_nacional_no_binario, na.rm = TRUE)
  lista_nb <- sum(dt_jalisco$lista_nacional_no_binario, na.rm = TRUE)
  
  # Guardar resultado
  lista_meses[[length(lista_meses) + 1]] <- data.frame(
    fecha = fecha,
    mes = format(fecha, "%b"),
    padron_nacional_no_binario = padron_nb,
    lista_nacional_no_binario = lista_nb,
    stringsAsFactors = FALSE
  )
  
  message("✅ ", format(fecha, "%b %Y"), ": P=", padron_nb, " L=", lista_nb)
}

# ========== CONSOLIDAR RESULTADOS ==========

if (length(lista_meses) > 0) {
  df_completo <- do.call(rbind, lista_meses)
  df_completo <- df_completo[order(df_completo$fecha), ]
  
  message("\n", strrep("=", 70))
  message("📊 DATAFRAME CONSOLIDADO (LO QUE RECIBE crear_card_no_binario)")
  message(strrep("=", 70))
  print(df_completo)
  
  # Calcular totales
  total_padron <- sum(df_completo$padron_nacional_no_binario, na.rm = TRUE)
  total_lista <- sum(df_completo$lista_nacional_no_binario, na.rm = TRUE)
  
  message("\n", strrep("=", 70))
  message("📊 TOTALES ANUALES")
  message(strrep("=", 70))
  message("Padrón Nacional NB: ", total_padron)
  message("Lista Nacional NB: ", total_lista)
  message("Total casos: ", total_padron + total_lista)
  
  message("\n", strrep("=", 70))
  message("✅ FORMATO ESPERADO DE LA CARD")
  message(strrep("=", 70))
  message("⚧ No Binario")
  message("Padrón Nacional: ", total_padron)
  message("Lista Nacional: ", total_lista)
  message("")
  message("Desglose por mes:")
  message("Mes    Padrón  Lista")
  
  for (i in 1:nrow(df_completo)) {
    mes <- df_completo$mes[i]
    padron <- df_completo$padron_nacional_no_binario[i]
    lista <- df_completo$lista_nacional_no_binario[i]
    message(sprintf("%-7s %4d  %4d", mes, padron, lista))
  }
  
  message(sprintf("Total   %4d  %4d", total_padron, total_lista))
  
} else {
  message("\n❌ No se cargaron datos")
}

message("\n", strrep("=", 70))
message("✅ VERIFICACIÓN COMPLETADA")
message(strrep("=", 70))
