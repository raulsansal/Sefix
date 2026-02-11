// ui/www/sidebar_toggle.js
// Versión: 3.4 - Botón descarga robusto + prevención de teclado mejorada
// Cambios v3.4:
//   - Detección de datos en DataTable completamente reescrita
//   - Múltiples selectores para encontrar el DataTable
//   - Prevención de teclado: blur inmediato + readonly proactivo
//   - Debug mejorado para diagnóstico

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
  // SISTEMA MÓVIL - INICIALIZACIÓN v3.4
  // ============================================================
  
  function initMobileUI() {
    if (!isMobile()) {
      $(".mobile-fab-container").remove();
      $(".mobile-overlay").remove();
      $(".well, [class*='sidebar_panel']").removeClass("mobile-open");
      $(".sidebar-right").removeClass("mobile-open");
      return;
    }
    
    if ($(".mobile-fab-container").length > 0) return;
    
    $(".mobile-overlay").remove();
    var overlay = $('<div class="mobile-overlay" id="mobile-overlay-main"></div>');
    $("body").prepend(overlay);
    
    var fabContainer = $('<div class="mobile-fab-container">' +
      '<button class="mobile-fab-btn" id="mobile-btn-filters" type="button">' +
        '<span class="fab-icon">⚙️</span>' +
        '<span class="fab-label">Filtros</span>' +
      '</button>' +
      '<button class="mobile-fab-btn" id="mobile-btn-restore" type="button">' +
        '<span class="fab-icon">🔄</span>' +
        '<span class="fab-label">Restabecer</span>' +
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
    
    setupSidebarAutoClose();
    setupDownloadButtonVisibility();
    setupOverlayClickHandler();
    
    // ✅ v3.4: Prevención de teclado mejorada
    preventKeyboardOnSelects();
    
    console.log("✅ UI móvil v3.4 inicializada");
  }
  
  // ============================================================
  // ✅ v3.4: PREVENCIÓN DE TECLADO - COMPLETAMENTE REESCRITA
  // ============================================================
  
  function preventKeyboardOnSelects() {
    if (!isMobile()) return;
    
    // ============================================
    // ESTRATEGIA 1: Aplicar readonly ANTES del focus
    // ============================================
    
    function makeSelectizeReadonly() {
      // Buscar todos los inputs dentro de selectize y hacerlos readonly
      $(".selectize-input input").each(function() {
        $(this).attr("readonly", "readonly");
        $(this).attr("inputmode", "none");
        $(this).attr("autocomplete", "off");
        $(this).attr("autocorrect", "off");
        $(this).attr("autocapitalize", "off");
        $(this).attr("spellcheck", "false");
        // CSS para prevenir selección
        $(this).css({
          "caret-color": "transparent",
          "-webkit-user-select": "none",
          "user-select": "none"
        });
      });
      
      // Para selects nativos
      $(".well select, [class*='sidebar_panel'] select").each(function() {
        $(this).attr("inputmode", "none");
      });
    }
    
    // Aplicar inmediatamente
    makeSelectizeReadonly();
    
    // Aplicar después de que Shiny inicialice
    setTimeout(makeSelectizeReadonly, 500);
    setTimeout(makeSelectizeReadonly, 1000);
    setTimeout(makeSelectizeReadonly, 2000);
    
    // ============================================
    // ESTRATEGIA 2: Interceptar mousedown/touchstart
    // ============================================
    
    $(document).on("mousedown touchstart", ".selectize-input", function(e) {
      // Hacer readonly antes de que el input reciba focus
      $(this).find("input").attr("readonly", "readonly").attr("inputmode", "none");
    });
    
    // ============================================
    // ESTRATEGIA 3: Blur inmediato si recibe focus
    // ============================================
    
    $(document).on("focus", ".selectize-input input", function(e) {
      var $input = $(this);
      
      // Asegurar que tenga readonly
      $input.attr("readonly", "readonly");
      $input.attr("inputmode", "none");
      
      // Blur inmediato para cerrar teclado si se abrió
      setTimeout(function() {
        // Solo hacer blur si el teclado virtual podría estar abierto
        // Verificamos que no haya un dropdown abierto que necesite el input
        var $selectize = $input.closest(".selectize-control");
        var isDropdownOpen = $selectize.find(".selectize-dropdown").is(":visible");
        
        if (!isDropdownOpen) {
          $input.blur();
        }
      }, 50);
    });
    
    // ============================================
    // ESTRATEGIA 4: Observar cambios en el DOM
    // ============================================
    
    var observer = new MutationObserver(function(mutations) {
      var needsUpdate = false;
      
      mutations.forEach(function(mutation) {
        if (mutation.addedNodes.length) {
          $(mutation.addedNodes).each(function() {
            if ($(this).find(".selectize-input").length > 0 || 
                $(this).hasClass("selectize-input")) {
              needsUpdate = true;
            }
          });
        }
      });
      
      if (needsUpdate) {
        setTimeout(makeSelectizeReadonly, 100);
      }
    });
    
    observer.observe(document.body, {
      childList: true,
      subtree: true
    });
    
    // ============================================
    // ESTRATEGIA 5: Evento de Shiny
    // ============================================
    
    $(document).on("shiny:inputchanged shiny:value", function() {
      setTimeout(makeSelectizeReadonly, 100);
    });
    
    console.log("✅ Prevención de teclado v3.4 configurada");
  }
  
  // ============================================================
  // OVERLAY CLICK HANDLER
  // ============================================================
  
  function setupOverlayClickHandler() {
    var overlay = $("#mobile-overlay-main");
    
    if (overlay.length === 0) return;
    
    overlay.off("click touchstart touchend");
    
    overlay.on("touchstart", function(e) {
      if (!isDrawerOpen()) return;
      if (Date.now() - drawerOpenTime < 300) return;
      
      e.preventDefault();
      e.stopPropagation();
      closeMobileDrawers();
    });
    
    overlay.on("click", function(e) {
      if (!isDrawerOpen()) return;
      if (Date.now() - drawerOpenTime < 300) return;
      
      e.preventDefault();
      e.stopPropagation();
      closeMobileDrawers();
    });
  }
  
  // ============================================================
  // DETECCIÓN GLOBAL DE TOQUES FUERA DEL SIDEBAR
  // ============================================================
  
  $(document).on("touchstart", function(e) {
    if (!isMobile() || !isDrawerOpen()) return;
    if (Date.now() - drawerOpenTime < 300) return;
    
    var $target = $(e.target);
    
    if ($target.closest(".mobile-open").length > 0) return;
    if ($target.closest(".mobile-fab-container").length > 0) return;
    if ($target.closest(".modal").length > 0) return;
    
    closeMobileDrawers();
  });
  
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
  // ✅ v3.4: VISIBILIDAD DEL BOTÓN DESCARGA - COMPLETAMENTE REESCRITA
  // ============================================================
  
  function setupDownloadButtonVisibility() {
    // Ocultar inicialmente
    $(".mobile-download-container").addClass("hidden-until-ready");
    
    // ============================================
    // FUNCIÓN DE DETECCIÓN ROBUSTA
    // ============================================
    
    function checkForData() {
      var hasRealData = false;
      var debugInfo = [];
      
      // ============================================
      // MÉTODO 1: Buscar DataTable por múltiples selectores
      // ============================================
      
      var $dataTable = null;
      var selectors = [
        ".dataTables_wrapper table.dataTable tbody tr",
        ".dataTables_wrapper tbody tr",
        "table.dataTable tbody tr",
        "#DataTables_Table_0 tbody tr",
        "[id*='table_data'] tbody tr",
        "[id*='main-table_data'] tbody tr"
      ];
      
      for (var i = 0; i < selectors.length; i++) {
        var $found = $(selectors[i]);
        if ($found.length > 0) {
          $dataTable = $found;
          debugInfo.push("Selector encontrado: " + selectors[i] + " (" + $found.length + " filas)");
          break;
        }
      }
      
      if (!$dataTable || $dataTable.length === 0) {
        debugInfo.push("No se encontró DataTable");
        updateButtonVisibility(false, debugInfo);
        return false;
      }
      
      // ============================================
      // MÉTODO 2: Verificar contenido de las celdas
      // ============================================
      
      var $firstRow = $dataTable.first();
      var $cells = $firstRow.find("td");
      
      if ($cells.length === 0) {
        debugInfo.push("No se encontraron celdas (td)");
        updateButtonVisibility(false, debugInfo);
        return false;
      }
      
      // Obtener texto de la primera celda
      var firstCellText = $cells.first().text().trim().toLowerCase();
      debugInfo.push("Primera celda: '" + firstCellText.substring(0, 50) + "'");
      
      // Lista de textos que indican "sin datos"
      var noDataTexts = [
        "configure",
        "no hay datos",
        "mensaje",
        "sin datos",
        "no data",
        "loading",
        "cargando",
        "esperando"
      ];
      
      // Verificar si es un mensaje de "sin datos"
      var isNoDataMessage = noDataTexts.some(function(text) {
        return firstCellText.includes(text);
      });
      
      if (isNoDataMessage) {
        debugInfo.push("Detectado mensaje de 'sin datos'");
        updateButtonVisibility(false, debugInfo);
        return false;
      }
      
      // ============================================
      // MÉTODO 3: Verificar que hay múltiples celdas con contenido
      // ============================================
      
      var cellsWithContent = 0;
      $cells.each(function() {
        var text = $(this).text().trim();
        if (text && text.length > 0) {
          cellsWithContent++;
        }
      });
      
      debugInfo.push("Celdas con contenido: " + cellsWithContent);
      
      // Si hay al menos 2 celdas con contenido, consideramos que hay datos
      hasRealData = cellsWithContent >= 2;
      
      // ============================================
      // MÉTODO 4: Verificar número total de filas
      // ============================================
      
      var totalRows = $dataTable.length;
      debugInfo.push("Total de filas: " + totalRows);
      
      // Si solo hay 1 fila y tiene pocas celdas, probablemente es un mensaje
      if (totalRows === 1 && cellsWithContent < 3) {
        hasRealData = false;
        debugInfo.push("Solo 1 fila con pocas celdas - probablemente mensaje");
      }
      
      updateButtonVisibility(hasRealData, debugInfo);
      return hasRealData;
    }
    
    // ============================================
    // ACTUALIZAR VISIBILIDAD DEL BOTÓN
    // ============================================
    
    function updateButtonVisibility(show, debugInfo) {
      var $container = $(".mobile-download-container");
      
      if ($container.length === 0) {
        console.log("⚠️ Contenedor de botón descarga no encontrado");
        return;
      }
      
      if (show) {
        $container.removeClass("hidden-until-ready");
        console.log("✅ Botón descarga VISIBLE", debugInfo);
      } else {
        $container.addClass("hidden-until-ready");
        // Solo loguear ocasionalmente para no saturar la consola
        if (Math.random() < 0.1) {
          console.log("⏳ Botón descarga OCULTO", debugInfo);
        }
      }
    }
    
    // ============================================
    // EJECUTAR VERIFICACIONES
    // ============================================
    
    // Verificación periódica cada 500ms
    setInterval(checkForData, 500);
    
    // Verificar después de eventos de Shiny
    $(document).on("shiny:value", function(event) {
      if (event.name) {
        var name = event.name.toLowerCase();
        if (name.includes("table") || name.includes("data") || name.includes("main")) {
          console.log("📊 Evento shiny:value detectado:", event.name);
          // Verificar múltiples veces después del evento
          setTimeout(checkForData, 200);
          setTimeout(checkForData, 500);
          setTimeout(checkForData, 1000);
          setTimeout(checkForData, 1500);
          setTimeout(checkForData, 2000);
        }
      }
    });
    
    // Verificar cuando el DataTable se redibuja
    $(document).on("draw.dt", function() {
      console.log("📊 Evento draw.dt detectado");
      setTimeout(checkForData, 100);
      setTimeout(checkForData, 300);
      setTimeout(checkForData, 500);
    });
    
    // Verificar al inicio con delays progresivos
    setTimeout(checkForData, 500);
    setTimeout(checkForData, 1000);
    setTimeout(checkForData, 1500);
    setTimeout(checkForData, 2000);
    setTimeout(checkForData, 3000);
    setTimeout(checkForData, 5000);
    
    console.log("✅ Visibilidad de botón descarga v3.4 configurada");
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
    if (!drawer || drawer.length === 0) return;
    
    closeMobileDrawers();
    
    setTimeout(function() {
      drawerOpenTime = Date.now();
      drawer.addClass("mobile-open");
      $(".mobile-overlay").addClass("active");
      $("body").addClass("mobile-drawer-open");
    }, 50);
  }
  
  function closeMobileDrawers() {
    $(".mobile-open").removeClass("mobile-open");
    $(".sidebar-right").removeClass("mobile-open");
    $(".well").removeClass("mobile-open");
    $("[class*='sidebar_panel']").removeClass("mobile-open");
    $(".mobile-overlay").removeClass("active");
    $("body").removeClass("mobile-drawer-open");
    drawerOpenTime = 0;
  }
  
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
        // Re-aplicar prevención de teclado después de que Shiny esté listo
        preventKeyboardOnSelects();
      }
    }, 500);
  });
  
  $(document).on("shown.bs.tab", function() {
    if (isMobile()) {
      setupSidebarAutoClose();
      setupOverlayClickHandler();
      preventKeyboardOnSelects();
    }
    setTimeout(triggerPlotlyResize, 300);
  });
  
  console.log("✅ sidebar_toggle.js v3.4 cargado");
});
