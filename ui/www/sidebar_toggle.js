// ui/www/sidebar_toggle.js
// Versión: 3.3 - Overlay robusto para cerrar sidebar al tocar fuera
// Cambios v3.3:
//   - Overlay completamente reescrito con detección táctil mejorada
//   - Listener global en document para capturar toques fuera del sidebar
//   - Debug mejorado para diagnóstico
//   - Sin botón X (cierre por overlay o botones de acción)

$(document).ready(function() {
  
  // ============================================================
  // DETECCIÓN DE DISPOSITIVO
  // ============================================================
  
  function isMobile() {
    return window.innerWidth <= 768;
  }
  
  // ============================================================
  // CONTROL DE ESTADO DEL DRAWER
  // ============================================================
  
  var drawerOpenTime = 0;
  var overlayClickEnabled = false;
  
  // ============================================================
  // INICIALIZACIÓN DESKTOP (código original)
  // ============================================================
  
  $(".sidebar-right").each(function() {
    var sidebarId = $(this).attr("id");
    var toggleBtn = $("[data-sidebar-id='" + sidebarId + "']");
    var toggleContainer = toggleBtn.closest(".toggle-container");
    
    toggleBtn.text(">>");
    toggleContainer.attr("data-for-sidebar", sidebarId);
  });
  
  // ============================================================
  // MANEJADOR TOGGLE DESKTOP (código original)
  // ============================================================
  
  $(document).on("click", ".toggle-sidebar-btn", function() {
    if (isMobile()) return;
    
    var sidebarId = $(this).attr("data-sidebar-id");
    var sidebar = $("#" + sidebarId);
    var toggleContainer = $(this).closest(".toggle-container");
    
    if (sidebar.hasClass("hidden")) {
      sidebar.removeClass("hidden");
      toggleContainer.removeClass("sidebar-hidden");
      $(this).text(">>");
    } else {
      sidebar.addClass("hidden");
      toggleContainer.addClass("sidebar-hidden");
      $(this).text("<<");
    }
  });
  
  // ============================================================
  // SISTEMA MÓVIL - INICIALIZACIÓN v3.3
  // ============================================================
  
  function initMobileUI() {
    if (!isMobile()) {
      // Limpiar UI móvil si existe
      $(".mobile-fab-container").remove();
      $(".mobile-overlay").remove();
      $(".well, [class*='sidebar_panel']").removeClass("mobile-open");
      $(".sidebar-right").removeClass("mobile-open");
      return;
    }
    
    // Evitar duplicados
    if ($(".mobile-fab-container").length > 0) return;
    
    // ✅ v3.3: Crear overlay como primer hijo del body para máxima prioridad
    $(".mobile-overlay").remove(); // Limpiar cualquier overlay existente
    var overlay = $('<div class="mobile-overlay" id="mobile-overlay-main"></div>');
    $("body").prepend(overlay);
    
    // Crear 4 botones flotantes
    var fabContainer = $('<div class="mobile-fab-container">' +
      '<button class="mobile-fab-btn" id="mobile-btn-filters" type="button">' +
        '<span class="fab-icon">⚙️</span>' +
        '<span class="fab-label">Filtros</span>' +
      '</button>' +
      '<button class="mobile-fab-btn" id="mobile-btn-restore" type="button">' +
        '<span class="fab-icon">🔄</span>' +
        '<span class="fab-label">Restablecer</span>' +
      '</button>' +
      '<button class="mobile-fab-btn" id="mobile-btn-projection" type="button">' +
        '<span class="fab-icon">ℹ️</span>' +
        '<span class="fab-label">Proyección</span>' +
      '</button>' +
      '<button class="mobile-fab-btn" id="mobile-btn-analysis" type="button">' +
        '<span class="fab-icon">📊</span>' +
        '<span class="fab-label">Análisis</span>' +
      '</button>' +
    '</div>');
    
    $("body").append(fabContainer);
    
    // Configurar listeners
    setupSidebarAutoClose();
    setupDownloadButtonVisibility();
    preventKeyboardOnSelects();
    
    // ✅ v3.3: Configurar overlay click handler
    setupOverlayClickHandler();
    
    console.log("✅ UI móvil v3.3 inicializada");
  }
  
  // ============================================================
  // ✅ v3.3: OVERLAY CLICK HANDLER - COMPLETAMENTE REESCRITO
  // ============================================================
  
  function setupOverlayClickHandler() {
    var overlay = $("#mobile-overlay-main");
    
    if (overlay.length === 0) {
      console.error("❌ Overlay no encontrado");
      return;
    }
    
    // Remover handlers previos
    overlay.off("click touchstart touchend");
    
    // ✅ Usar touchstart para mejor respuesta en móvil
    overlay.on("touchstart", function(e) {
      if (!isDrawerOpen()) return;
      
      // Verificar tiempo mínimo de apertura
      if (Date.now() - drawerOpenTime < 300) return;
      
      console.log("👆 Touch en overlay detectado");
      e.preventDefault();
      e.stopPropagation();
      closeMobileDrawers();
    });
    
    // ✅ También manejar click para dispositivos híbridos
    overlay.on("click", function(e) {
      if (!isDrawerOpen()) return;
      
      // Verificar tiempo mínimo de apertura
      if (Date.now() - drawerOpenTime < 300) return;
      
      console.log("🖱️ Click en overlay detectado");
      e.preventDefault();
      e.stopPropagation();
      closeMobileDrawers();
    });
    
    console.log("✅ Overlay click handler configurado");
  }
  
  // ============================================================
  // ✅ v3.3: MÉTODO ALTERNATIVO - DETECCIÓN GLOBAL DE TOQUES
  // Si el toque no está dentro de un drawer abierto, cerrar
  // ============================================================
  
  $(document).on("touchstart", function(e) {
    if (!isMobile() || !isDrawerOpen()) return;
    
    // Verificar tiempo mínimo
    if (Date.now() - drawerOpenTime < 300) return;
    
    var $target = $(e.target);
    
    // Si el toque está dentro de un drawer abierto, no cerrar
    if ($target.closest(".mobile-open").length > 0) {
      return;
    }
    
    // Si el toque está en los botones FAB, no cerrar
    if ($target.closest(".mobile-fab-container").length > 0) {
      return;
    }
    
    // Si el toque está en un modal, no cerrar
    if ($target.closest(".modal").length > 0) {
      return;
    }
    
    // Si llegamos aquí, el toque está fuera del sidebar - cerrar
    console.log("👆 Toque fuera del sidebar - cerrando");
    closeMobileDrawers();
  });
  
  // ============================================================
  // PREVENIR TECLADO EN SELECTINPUTS
  // ============================================================
  
  function preventKeyboardOnSelects() {
    $(document).on("focus", ".selectize-input input", function(e) {
      $(this).attr("readonly", "readonly");
      $(this).attr("inputmode", "none");
    });
    
    $(".well select, [class*='sidebar_panel'] select").each(function() {
      $(this).attr("inputmode", "none");
    });
    
    var observer = new MutationObserver(function(mutations) {
      mutations.forEach(function(mutation) {
        if (mutation.addedNodes.length) {
          $(mutation.addedNodes).find(".selectize-input input").attr("readonly", "readonly");
          $(mutation.addedNodes).find("select").attr("inputmode", "none");
        }
      });
    });
    
    observer.observe(document.body, {
      childList: true,
      subtree: true
    });
  }
  
  // ============================================================
  // CONFIGURAR CIERRE AUTOMÁTICO DEL SIDEBAR POR BOTONES
  // ============================================================
  
  function setupSidebarAutoClose() {
    var closeButtonSelectors = [
      "[id$='-btn_consultar']",
      "[id$='btn_consultar']",
      "[id$='-reset_config']",
      "[id$='reset_config']",
      "[id$='-download_csv']",
      "[id$='download_csv']:not([id$='download_csv_mobile'])"
    ];
    
    $(document).off("click.sidebarAutoClose");
    
    closeButtonSelectors.forEach(function(selector) {
      $(document).on("click.sidebarAutoClose", selector, function(e) {
        if (isMobile() && isDrawerOpen()) {
          var buttonId = $(this).attr("id") || "unknown";
          console.log("🔘 Botón de acción presionado:", buttonId);
          
          setTimeout(function() {
            closeMobileDrawers();
          }, 150);
        }
      });
    });
  }
  
  // ============================================================
  // VERIFICAR SI HAY DRAWER ABIERTO
  // ============================================================
  
  function isDrawerOpen() {
    return $(".well.mobile-open").length > 0 || 
           $(".sidebar-right.mobile-open").length > 0 ||
           $("[class*='sidebar_panel'].mobile-open").length > 0;
  }
  
  // ============================================================
  // CONFIGURAR VISIBILIDAD DEL BOTÓN DESCARGA
  // ============================================================
  
  function setupDownloadButtonVisibility() {
    $(".mobile-download-container").addClass("hidden-until-ready");
    
    function checkForData() {
      var $rows = $(".dataTables_wrapper table tbody tr");
      var hasRealData = false;
      
      if ($rows.length > 0) {
        var firstCellText = $rows.first().find("td").first().text().trim();
        hasRealData = firstCellText && 
                      !firstCellText.includes("Configure") &&
                      !firstCellText.includes("No hay datos") &&
                      !firstCellText.includes("Mensaje");
      }
      
      if (hasRealData) {
        $(".mobile-download-container").removeClass("hidden-until-ready");
      } else {
        $(".mobile-download-container").addClass("hidden-until-ready");
      }
      
      return hasRealData;
    }
    
    setInterval(checkForData, 500);
    
    $(document).on("shiny:value", function(event) {
      if (event.name && event.name.includes("table_data")) {
        setTimeout(checkForData, 300);
        setTimeout(checkForData, 600);
        setTimeout(checkForData, 1000);
      }
    });
    
    setTimeout(checkForData, 1000);
    setTimeout(checkForData, 2000);
  }
  
  // ============================================================
  // MANEJADORES MÓVILES - BOTONES FAB
  // ============================================================
  
  $(document).on("click", "#mobile-btn-filters", function(e) {
    e.stopPropagation();
    var sidebar = $(".well").first();
    if (sidebar.length === 0) {
      sidebar = $("[class*='sidebar_panel']").first();
    }
    openMobileDrawer(sidebar, "left");
  });
  
  $(document).on("click", "#mobile-btn-restore", function(e) {
    e.stopPropagation();
    var resetBtn = $("[id$='-reset_config'], [id$='reset_config']").first();
    
    if (resetBtn.length > 0) {
      resetBtn.trigger("click");
      console.log("🔄 Botón Restaurar: click simulado");
      showMobileToast("Consulta restablecida");
    } else {
      showMobileToast("No disponible en esta vista");
    }
  });
  
  $(document).on("click", "#mobile-btn-projection", function(e) {
    e.stopPropagation();
    var metodologiaBtn = $("[id$='-info_grafica1'], [id$='info_grafica1']").first();
    
    if (metodologiaBtn.length > 0) {
      metodologiaBtn.trigger("click");
      console.log("ℹ️ Botón Proyección: click simulado");
    } else {
      showMobileToast("No disponible en esta vista");
    }
  });
  
  $(document).on("click", "#mobile-btn-analysis", function(e) {
    e.stopPropagation();
    var sidebar = $(".sidebar-right:visible").first();
    if (sidebar.length === 0) {
      sidebar = $(".sidebar-right").first();
    }
    openMobileDrawer(sidebar, "right");
  });
  
  // ============================================================
  // CERRAR CON TECLA ESCAPE
  // ============================================================
  
  $(document).on("keydown", function(e) {
    if (e.key === "Escape" && isMobile() && isDrawerOpen()) {
      console.log("⌨️ Cerrando drawer con Escape");
      closeMobileDrawers();
    }
  });
  
  // ============================================================
  // TOAST PARA FEEDBACK VISUAL
  // ============================================================
  
  function showMobileToast(message) {
    $(".mobile-toast").remove();
    
    var toast = $('<div class="mobile-toast">' + message + '</div>');
    $("body").append(toast);
    
    setTimeout(function() {
      toast.addClass("show");
    }, 10);
    
    setTimeout(function() {
      toast.removeClass("show");
      setTimeout(function() {
        toast.remove();
      }, 300);
    }, 2000);
  }
  
  // ============================================================
  // FUNCIONES DE DRAWER
  // ============================================================
  
  function openMobileDrawer(drawer, direction) {
    if (!drawer || drawer.length === 0) {
      console.warn("Drawer no encontrado");
      return;
    }
    
    // Cerrar cualquier drawer abierto primero
    closeMobileDrawers();
    
    // Pequeño delay para asegurar que el cierre se complete
    setTimeout(function() {
      // Registrar tiempo de apertura
      drawerOpenTime = Date.now();
      
      // Agregar clase para abrir
      drawer.addClass("mobile-open");
      
      // Mostrar overlay
      $(".mobile-overlay").addClass("active");
      
      // Prevenir scroll del body
      $("body").addClass("mobile-drawer-open");
      
      console.log("📂 Drawer abierto: " + direction + " (tiempo: " + drawerOpenTime + ")");
    }, 50);
  }
  
  function closeMobileDrawers() {
    // Cerrar todos los drawers
    $(".mobile-open").removeClass("mobile-open");
    $(".sidebar-right").removeClass("mobile-open");
    $(".well").removeClass("mobile-open");
    $("[class*='sidebar_panel']").removeClass("mobile-open");
    
    // Ocultar overlay
    $(".mobile-overlay").removeClass("active");
    
    // Restaurar scroll del body
    $("body").removeClass("mobile-drawer-open");
    
    // Reset del tiempo
    drawerOpenTime = 0;
    
    console.log("📂 Drawers cerrados");
  }
  
  // Exponer función globalmente
  window.closeMobileDrawers = closeMobileDrawers;
  window.isDrawerOpen = isDrawerOpen;
  
  // ============================================================
  // RESIZE HANDLER
  // ============================================================
  
  var resizeTimeout;
  $(window).on("resize", function() {
    clearTimeout(resizeTimeout);
    resizeTimeout = setTimeout(function() {
      if (isMobile()) {
        initMobileUI();
      } else {
        $(".mobile-fab-container").remove();
        $(".mobile-overlay").remove();
        $(".mobile-toast").remove();
        closeMobileDrawers();
        
        $(".sidebar-right").removeClass("mobile-open hidden");
        $(".well").removeClass("mobile-open");
      }
      
      triggerPlotlyResize();
    }, 250);
  });
  
  // ============================================================
  // PLOTLY RESIZE HELPER
  // ============================================================
  
  function triggerPlotlyResize() {
    $(".plotly, .js-plotly-plot").each(function() {
      var plotlyDiv = this;
      if (plotlyDiv && typeof Plotly !== "undefined") {
        try {
          Plotly.Plots.resize(plotlyDiv);
        } catch(e) {}
      }
    });
  }
  
  // ============================================================
  // INICIALIZACIÓN
  // ============================================================
  
  initMobileUI();
  
  $(document).on("shiny:sessioninitialized", function() {
    setTimeout(function() {
      if (isMobile()) {
        initMobileUI();
      }
    }, 500);
  });
  
  $(document).on("shown.bs.tab", function() {
    if (isMobile()) {
      setupSidebarAutoClose();
      preventKeyboardOnSelects();
      setupOverlayClickHandler();
    }
    setTimeout(triggerPlotlyResize, 300);
  });
  
  console.log("✅ sidebar_toggle.js v3.3 cargado");
});
