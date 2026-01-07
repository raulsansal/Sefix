# test_bucket_correcto.R

library(httr)

# Probar con AMBOS buckets
buckets <- list(
  legacy = "https://firebasestorage.googleapis.com/v0/b/eskemma-3c4c3.appspot.com/o",
  nuevo = "https://firebasestorage.googleapis.com/v0/b/eskemma-3c4c3.firebasestorage.app/o"
)

path <- "sefix/pdln/historico/derfe_pdln_20240131_base.csv"
path_encoded <- gsub("/", "%2F", path)

cat("========== PROBANDO AMBOS BUCKETS ==========\n\n")

for (nombre in names(buckets)) {
  base <- buckets[[nombre]]
  url <- paste0(base, "/", path_encoded, "?alt=media")
  
  cat("📦 Bucket:", nombre, "\n")
  cat("🔗 URL:", url, "\n")
  
  response <- tryCatch({
    HEAD(url, timeout(10))
  }, error = function(e) {
    cat("❌ Error:", e$message, "\n\n")
    return(NULL)
  })
  
  if (!is.null(response)) {
    status <- status_code(response)
    if (status == 200) {
      cat("✅ FUNCIONA! Status:", status, "\n")
      cat("👉 USAR ESTE BUCKET\n\n")
    } else {
      cat("❌ No funciona. Status:", status, "\n\n")
    }
  }
}

cat("========== FIN ==========\n")