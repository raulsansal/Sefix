# test_firebase_urls.R
# Script para verificar URLs de Firebase Storage

library(httr)

# URL base
base <- "https://firebasestorage.googleapis.com/v0/b/eskemma-3c4c3.appspot.com/o"

# Función para probar URL
probar_url <- function(path, descripcion) {
  url_encoded <- gsub("/", "%2F", path)
  url_completa <- paste0(base, "/", url_encoded, "?alt=media")
  
  cat("\n", descripcion, "\n")
  cat("Path: ", path, "\n")
  cat("URL: ", url_completa, "\n")
  
  response <- tryCatch({
    HEAD(url_completa, timeout(10))
  }, error = function(e) {
    cat("❌ Error: ", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(response)) {
    status <- status_code(response)
    if (status == 200) {
      cat("✅ EXISTE (", status, ")\n")
      return(TRUE)
    } else {
      cat("❌ NO EXISTE (", status, ")\n")
      return(FALSE)
    }
  }
  
  return(FALSE)
}

# Probar diferentes estructuras posibles
cat("========== PROBANDO ESTRUCTURAS POSIBLES ==========\n")

# Opción 1: Con prefijo "sefix/"
probar_url("sefix/pdln/historico/derfe_pdln_20240131_base.csv", 
           "1. Con prefijo sefix/")

# Opción 2: Sin prefijo "sefix/"
probar_url("pdln/historico/derfe_pdln_20240131_base.csv", 
           "2. Sin prefijo sefix/")

# Opción 3: Directamente en raíz
probar_url("derfe_pdln_20240131_base.csv", 
           "3. Directamente en raíz")

# Opción 4: Otra estructura común
probar_url("data/pdln/historico/derfe_pdln_20240131_base.csv", 
           "4. Con prefijo data/")

# Opción 5: Sin guiones bajos
probar_url("sefix/pdln/historico/derfe-pdln-20240131-base.csv", 
           "5. Con guiones en lugar de underscores")

cat("\n========== FIN DE PRUEBAS ==========\n")