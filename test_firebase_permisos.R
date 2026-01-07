# test_firebase_permisos.R

library(httr)

base <- "https://firebasestorage.googleapis.com/v0/b/eskemma-3c4c3.appspot.com/o"

# Probar URL con sefix/
path <- "sefix/pdln/historico/derfe_pdln_20240131_base.csv"
url_encoded <- gsub("/", "%2F", path)
url_completa <- paste0(base, "/", url_encoded, "?alt=media")

cat("🔍 Probando URL:\n")
cat(url_completa, "\n\n")

response <- HEAD(url_completa, timeout(10))
status <- status_code(response)

if (status == 200) {
  cat("✅ ¡FUNCIONA! Status:", status, "\n")
  cat("📊 Ahora prueba cargar el archivo completo:\n")
  cat("   datos <- cargar_pdln_historico_firebase('20240131')\n")
} else if (status == 403) {
  cat("❌ ERROR 403: Sin permisos de lectura\n")
  cat("   → Verifica que las reglas de Storage estén publicadas\n")
  cat("   → Espera 1 minuto y vuelve a intentar\n")
} else if (status == 404) {
  cat("❌ ERROR 404: Archivo no encontrado\n")
  cat("   → Verifica la estructura en Firebase Console\n")
} else {
  cat("❌ ERROR", status, "\n")
}