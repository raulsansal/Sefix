// ui/www/sidebar_toggle.js
// Versión: 3.0 - Barra inferior móvil con 4 botones
// Cambios v3.0:
//   - 4 botones: FILTROS, RESTAURAR, PROYECCIÓN, ANÁLISIS
//   - RESTAURAR: replica funcionalidad de "Restablecer consulta" del sidebar
//   - PROYECCIÓN: abre modal de metodología (reemplaza botón entre gráficas)

$(document).ready(function() {
  
  // ============================================================
  // DETECCIÓN DE DISPOSITIVO
  // ============================================================
  
  function isMobile() {
    return window.innerWidth <= 768;
  }
  
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
  // SISTEMA MÓVIL - INICIALIZACIÓN v3.0
  // ============================================================
  
  function initMobileUI() {
    if (!isMobile()) {
      // Limpiar UI móvil si existe
      $(".mobile-fab-container").remove();
      $(".mobile-overlay").remove();
      $(".mobile-drawer-close").remove();
      $(".well, [class*='sidebar_panel']").removeClass("mobile-open");
      $(".sidebar-right").removeClass("mobile-open");
      return;
    }
    
    // Evitar duplicados
    if ($(".mobile-fab-container").length > 0) return;
    
    // Crear overlay
    if ($(".mobile-overlay").length === 0) {
      $("body").append('<div class="mobile-overlay"></div>');
    }
    
    // ✅ v3.0: Crear 4 botones flotantes
    var fabContainer = $('<div class="mobile-fab-container">' +
      // 1. FILTROS
      '<button class="mobile-fab-btn" id="mobile-btn-filters">' +
        '<span class="fab-icon">⚙️</span>' +
        '<span class="fab-label">Filtros</span>' +
      '</button>' +
      // 2. RESTAURAR (nuevo)
      '<button class="mobile-fab-btn" id="mobile-btn-restore">' +
        '<span class="fab-icon">🔄</span>' +
        '<span class="fab-label">Restaurar</span>' +
      '</button>' +
      // 3. PROYECCIÓN (nuevo - reemplaza Metodología)
      '<button class="mobile-fab-btn" id="mobile-btn-projection">' +
        '<span class="fab-icon">ℹ️</span>' +
        '<span class="fab-label">Proyección</span>' +
      '</button>' +
      // 4. ANÁLISIS
      '<button class="mobile-fab-btn" id="mobile-btn-analysis">' +
        '<span class="fab-icon">📊</span>' +
        '<span class="fab-label">Análisis</span>' +
      '</button>' +
    '</div>');
    
    $("body").append(fabContainer);
    
    // Agregar botones de cerrar a los drawers
    addCloseButtons();
    
    console.log("✅ UI móvil v3.0 inicializada (4 botones)");
  }
  
  // ============================================================
  // AGREGAR BOTONES DE CERRAR A DRAWERS
  // ============================================================
  
  function addCloseButtons() {
    // Botón cerrar para sidebar derecho (análisis)
    $(".sidebar-right").each(function() {
      if ($(this).find(".mobile-drawer-close").length === 0) {
        $(this).prepend('<button class="mobile-drawer-close" aria-label="Cerrar">×</button>');
      }
    });
    
    // Botón cerrar para sidebar izquierdo (filtros)
    $(".well").each(function() {
      if ($(this).find(".mobile-drawer-close").length === 0) {
        $(this).prepend('<button class="mobile-drawer-close" aria-label="Cerrar">×</button>');
      }
    });
  }
  
  // ============================================================
  // MANEJADORES MÓVILES
  // ============================================================
  
  // 1. Abrir filtros (sidebar izquierdo)
  $(document).on("click", "#mobile-btn-filters", function() {
    var sidebar = $(".well").first();
    if (sidebar.length === 0) {
      sidebar = $("[class*='sidebar_panel']").first();
    }
    openMobileDrawer(sidebar, "left");
  });
  
  // 2. ✅ v3.0: RESTAURAR - Simula click en botón "Restablecer consulta"
  $(document).on("click", "#mobile-btn-restore", function() {
    // Buscar el botón de restablecer por ID que termina en "reset_config"
    var resetBtn = $("[id$='-reset_config'], [id$='reset_config']").first();
    
    if (resetBtn.length > 0) {
      // Simular click en el botón original
      resetBtn.trigger("click");
      console.log("🔄 Botón Restaurar: click simulado en", resetBtn.attr("id"));
      
      // Mostrar feedback visual
      showMobileToast("Consulta restablecida");
    } else {
      console.warn("⚠️ No se encontró el botón de restablecer");
      showMobileToast("No disponible en esta vista");
    }
  });
  
  // 3. ✅ v3.0: PROYECCIÓN - Abre modal de metodología
  $(document).on("click", "#mobile-btn-projection", function() {
    // Buscar el botón de metodología por ID que termina en "info_grafica1"
    var metodologiaBtn = $("[id$='-info_grafica1'], [id$='info_grafica1']").first();
    
    if (metodologiaBtn.length > 0) {
      // Simular click en el botón original
      metodologiaBtn.trigger("click");
      console.log("ℹ️ Botón Proyección: click simulado en", metodologiaBtn.attr("id"));
    } else {
      console.warn("⚠️ No se encontró el botón de metodología");
      showMobileToast("No disponible en esta vista");
    }
  });
  
  // 4. Abrir análisis (sidebar derecho)
  $(document).on("click", "#mobile-btn-analysis", function() {
    var sidebar = $(".sidebar-right:visible").first();
    if (sidebar.length === 0) {
      sidebar = $(".sidebar-right").first();
    }
    openMobileDrawer(sidebar, "right");
  });
  
  // Cerrar drawer con botón X
  $(document).on("click", ".mobile-drawer-close", function() {
    closeMobileDrawers();
  });
  
  // Cerrar drawer con overlay
  $(document).on("click", ".mobile-overlay", function() {
    closeMobileDrawers();
  });
  
  // Cerrar con tecla Escape
  $(document).on("keydown", function(e) {
    if (e.key === "Escape" && isMobile()) {
      closeMobileDrawers();
    }
  });
  
  // ============================================================
  // ✅ v3.0: TOAST PARA FEEDBACK VISUAL
  // ============================================================
  
  function showMobileToast(message) {
    // Remover toast existente
    $(".mobile-toast").remove();
    
    var toast = $('<div class="mobile-toast">' + message + '</div>');
    $("body").append(toast);
    
    // Mostrar con animación
    setTimeout(function() {
      toast.addClass("show");
    }, 10);
    
    // Ocultar después de 2 segundos
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
    
    // Agregar clase para abrir
    drawer.addClass("mobile-open");
    
    // Mostrar overlay
    $(".mobile-overlay").addClass("active");
    
    // Prevenir scroll del body
    $("body").addClass("mobile-drawer-open");
    
    // Focus en el drawer para accesibilidad
    drawer.attr("tabindex", "-1").focus();
    
    console.log("📂 Drawer abierto: " + direction);
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
    
    console.log("📂 Drawers cerrados");
  }
  
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
        // Limpiar UI móvil
        $(".mobile-fab-container").remove();
        $(".mobile-overlay").remove();
        $(".mobile-drawer-close").remove();
        $(".mobile-toast").remove();
        closeMobileDrawers();
        
        // Restaurar estado desktop de sidebars
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
        } catch(e) {
          // Silenciar errores si el gráfico aún no está listo
        }
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
      addCloseButtons();
      closeMobileDrawers();
    }
    setTimeout(triggerPlotlyResize, 300);
  });
  
  console.log("✅ sidebar_toggle.js v3.0 cargado (4 botones móviles)");
});
