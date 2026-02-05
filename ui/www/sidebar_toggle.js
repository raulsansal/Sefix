// ui/www/sidebar_toggle.js
// Versión: 2.0 - Soporte completo para móvil con drawers y botones flotantes
// Compatible con desktop (sin cambios en funcionalidad existente)

$(document).ready(function() {
  
  // ============================================================
  // DETECCIÓN DE DISPOSITIVO
  // ============================================================
  
  function isMobile() {
    return window.innerWidth <= 768;
  }
  
  // ============================================================
  // INICIALIZACIÓN DESKTOP (código original corregido)
  // ============================================================
  
  $(".sidebar-right").each(function() {
    var sidebarId = $(this).attr("id");
    // Encontrar el botón de toggle asociado a este sidebar
    var toggleBtn = $("[data-sidebar-id='" + sidebarId + "']");
    var toggleContainer = toggleBtn.closest(".toggle-container");
    
    // Configurar texto inicial
    toggleBtn.text(">>");
    
    // Asociar el contenedor del toggle con el sidebar
    toggleContainer.attr("data-for-sidebar", sidebarId);
  });
  
  // ============================================================
  // MANEJADOR TOGGLE DESKTOP (código original)
  // ============================================================
  
  $(document).on("click", ".toggle-sidebar-btn", function() {
    // Solo ejecutar en desktop
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
  // SISTEMA MÓVIL - INICIALIZACIÓN
  // ============================================================
  
  function initMobileUI() {
    // Solo inicializar si estamos en móvil
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
    
    // Crear botones flotantes con iconos Unicode
    var fabContainer = $('<div class="mobile-fab-container">' +
      '<button class="mobile-fab-btn" id="mobile-btn-filters">' +
        '<span class="fab-icon">⚙️</span>' +
        '<span class="fab-label">Filtros</span>' +
      '</button>' +
      '<button class="mobile-fab-btn" id="mobile-btn-analysis">' +
        '<span class="fab-icon">📊</span>' +
        '<span class="fab-label">Análisis</span>' +
      '</button>' +
    '</div>');
    
    $("body").append(fabContainer);
    
    // Agregar botones de cerrar a los drawers
    addCloseButtons();
    
    console.log("✅ UI móvil inicializada");
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
  
  // Abrir filtros (sidebar izquierdo)
  $(document).on("click", "#mobile-btn-filters", function() {
    var sidebar = $(".well").first();
    if (sidebar.length === 0) {
      // Buscar por clase que contenga "sidebar_panel"
      sidebar = $("[class*='sidebar_panel']").first();
    }
    openMobileDrawer(sidebar, "left");
  });
  
  // Abrir análisis (sidebar derecho)
  $(document).on("click", "#mobile-btn-analysis", function() {
    // Encontrar el sidebar derecho visible o el primero disponible
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
  // RESIZE HANDLER - CAMBIO ENTRE MÓVIL Y DESKTOP
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
        closeMobileDrawers();
        
        // Restaurar estado desktop de sidebars
        $(".sidebar-right").removeClass("mobile-open hidden");
        $(".well").removeClass("mobile-open");
      }
      
      // Forzar resize de gráficos Plotly
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
  
  // Inicializar UI móvil si es necesario
  initMobileUI();
  
  // Re-inicializar cuando Shiny termina de renderizar
  $(document).on("shiny:sessioninitialized", function() {
    setTimeout(function() {
      if (isMobile()) {
        initMobileUI();
      }
    }, 500);
  });
  
  // Re-inicializar cuando cambia de tab
  $(document).on("shown.bs.tab", function() {
    if (isMobile()) {
      addCloseButtons();
      closeMobileDrawers();
    }
    // Resize gráficos después de cambiar tab
    setTimeout(triggerPlotlyResize, 300);
  });
  
  console.log("✅ sidebar_toggle.js v2.0 cargado (con soporte móvil)");
});
