// ui/www/custom_reset.js
// Manejador para resetear botones de acción

Shiny.addCustomMessageHandler("resetButton", function(buttonId) {
  console.log("🔄 Reseteando botón: " + buttonId);
  
  // Obtener el botón por ID
  var button = document.getElementById(buttonId);
  
  if (button) {
    // Simular click para incrementar contador
    // Luego forzar reset del input value
    Shiny.setInputValue(buttonId, 0, {priority: "event"});
    console.log("✅ Botón " + buttonId + " reseteado a 0");
  } else {
    console.error("❌ Botón no encontrado: " + buttonId);
  }
});
