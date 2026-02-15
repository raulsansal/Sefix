// ui/www/sidebar_toggle.js
// Versión: 3.5 - Botones contextuales por pestaña + FILTROS corregido
// Cambios v3.5:
//   - Botones de barra inferior cambian según pestaña activa
//   - Lista Nominal: FILTROS, RESTABLECER, PROYECCIÓN, ANÁLISIS
//   - Elecciones Federales: FILTROS, RESTABLECER, DESCARGAR, ANÁLISIS
//   - FILTROS ahora funciona con namespaces específicos
//   - Detecta cambios de pestaña via Shiny

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
  // ✅ v3.5: CONFIGURACIÓN DE BOTONES POR PESTAÑA
  // ============================================================
  
  var tabButtonConfigs = {
    "lista": {
      buttons: [
        { id: "mobile-btn-filters", icon: "⚙️", label: "Filtros", action: "filters" },
        { id: "mobile-btn-restore", icon: "🔄", label: "Restablecer", action: "restore" },
        { id: "mobile-btn-projection", icon: "ℹ️", label: "Proyección", action: "projection" },
        { id: "mobile-btn-analysis", icon: "📊", label: "Análisis", action: "analysis" }
      ],
      sidebarSelector: "#lista-sidebar_panel, [id$='lista-sidebar_panel']",
      sidebarRightSelector: "#lista-sidebar-right-lista, [id$='sidebar-right-lista']",
      restoreSelector: "#lista-reset_config, [id$='lista-reset_config']",
      projectionSelector: "#lista-info_grafica1, [id$='lista-info_grafica1']",
      downloadSelector: null
    },
    "federales": {
      buttons: [
        { id: "mobile-btn-filters", icon: "⚙️", label: "Filtros", action: "filters" },
        { id: "mobile-btn-restore", icon: "🔄", label: "Restablecer", action: "restore" },
        { id: "mobile-btn-download", icon: "📥", label: "Descargar", action: "download" },
        { id: "mobile-btn-analysis", icon: "📊", label: "Análisis", action: "analysis" }
      ],
      sidebarSelector: "#federales-sidebar_panel, [id$='federales-sidebar_panel']",
      sidebarRightSelector: "#federales-sidebar-right-federales, [id$='sidebar-right-federales']",
      restoreSelector: "#federales-reset_config, [id$='federales-reset_config']",
      projectionSelector: null,
      downloadSelector: "#federales-download_csv, [id$='federales-download_csv']"
    },
    // Configuración por defecto para otras pestañas
    "default": {
      buttons: [
        { id: "mobile-btn-filters", icon: "⚙️", label: "Filtros", action: "filters" },
        { id: "mobile-btn-restore", icon: "🔄", label: "Restablecer", action: "restore" },
        { id: "mobile-btn-info", icon: "ℹ️", label: "Info", action: "info" },
        { id: "mobile-btn-analysis", icon: "📊", label: "Análisis", action: "analysis" }
      ],
      sidebarSelector: ".well, [class*='sidebar_panel']",
      sidebarRightSelector: ".sidebar-right",
      restoreSelector: "[id$='-reset_config'], [id$='reset_config']",
      projectionSelector: null,
      downloadSelector: null
    }
  };
  
  // Pestaña activa actual
  var currentTab = "lista";
  
  // ============================================================
  // ✅ v3.5: OBTENER CONFIGURACIÓN PARA PESTAÑA ACTUAL
  // ============================================================
  
  function getTabConfig(tabId) {
    if (tabButtonConfigs[tabId]) {
      return tabButtonConfigs[tabId];
    }
    return tabButtonConfigs["default"];
  }
  
  // ============================================================
  // ✅ v3.5: ACTUALIZAR BOTONES DE LA BARRA INFERIOR
  // ============================================================
  
  function updateMobileButtons(tabId) {
    if (!isMobile()) return;
    
    currentTab = tabId || "lista";
    var config = getTabConfig(currentTab);
    
    console.log("📱 [v3.5] Actualizando botones para pestaña: " + currentTab);
    
    // Eliminar barra existente
    $(".mobile-fab-container").remove();
    
    // Crear nueva barra con botones específicos
    var buttonsHtml = config.buttons.map(function(btn) {
      return '<button class="mobile-fab-btn" id="' + btn.id + '" type="button" data-action="' + btn.action + '">' +
        '<span class="fab-icon">' + btn.icon + '</span>' +
        '<span class="fab-label">' + btn.label + '</span>' +
      '</button>';
    }).join('');
    
    var fabContainer = $('<div class="mobile-fab-container">' + buttonsHtml + '</div>');
    $("body").append(fabContainer);
    
    // Configurar handlers para los nuevos botones
    setupButtonHandlers(config);
    
    console.log("✅ [v3.5] Botones actualizados: " + config.buttons.map(function(b) { return b.label; }).join(", "));
  }
  
  // ============================================================
  // ✅ v3.5: CONFIGURAR HANDLERS DE BOTONES
  // ============================================================
  
  function setupButtonHandlers(config) {
    // Remover handlers anteriores
    $(document).off("click.mobileButtons");
    
    // FILTROS
    $(document).on("click.mobileButtons", "#mobile-btn-filters", function(e) {
      e.stopPropagation();
      var sidebar = $(config.sidebarSelector).first();
      
      if (sidebar.length === 0) {
        // Fallback: buscar cualquier sidebar
        sidebar = $(".well").first();
        if (sidebar.length === 0) {
          sidebar = $("[class*='sidebar_panel']").first();
        }
      }
      
      if (sidebar.length > 0) {
        openMobileDrawer(sidebar, "left");
        console.log("✅ [FILTROS] Abriendo sidebar: " + sidebar.attr("id"));
      } else {
        showMobileToast("Filtros no disponibles");
        console.log("⚠️ [FILTROS] No se encontró sidebar con selector: " + config.sidebarSelector);
      }
    });
    
    // RESTABLECER
    $(document).on("click.mobileButtons", "#mobile-btn-restore", function(e) {
      e.stopPropagation();
      var resetBtn = $(config.restoreSelector).first();
      
      if (resetBtn.length > 0) {
        resetBtn.trigger("click");
        showMobileToast("Consulta restablecida");
        console.log("✅ [RESTABLECER] Trigger en: " + resetBtn.attr("id"));
      } else {
        showMobileToast("No disponible en esta vista");
        console.log("⚠️ [RESTABLECER] No se encontró botón con selector: " + config.restoreSelector);
      }
    });
    
    // PROYECCIÓN (solo Lista Nominal)
    $(document).on("click.mobileButtons", "#mobile-btn-projection", function(e) {
      e.stopPropagation();
      if (config.projectionSelector) {
        var metodologiaBtn = $(config.projectionSelector).first();
        
        if (metodologiaBtn.length > 0) {
          metodologiaBtn.trigger("click");
          console.log("✅ [PROYECCIÓN] Trigger en: " + metodologiaBtn.attr("id"));
        } else {
          showMobileToast("No disponible");
          console.log("⚠️ [PROYECCIÓN] No se encontró botón con selector: " + config.projectionSelector);
        }
      } else {
        showMobileToast("No disponible en esta vista");
      }
    });
    
    // DESCARGAR (solo Elecciones Federales)
    $(document).on("click.mobileButtons", "#mobile-btn-download", function(e) {
      e.stopPropagation();
      if (config.downloadSelector) {
        var downloadBtn = $(config.downloadSelector).first();
        
        if (downloadBtn.length > 0) {
          // Para downloadButton de Shiny, necesitamos hacer clic directo
          downloadBtn[0].click();
          showMobileToast("Descargando CSV...");
          console.log("✅ [DESCARGAR] Trigger en: " + downloadBtn.attr("id"));
        } else {
          showMobileToast("No hay datos para descargar");
          console.log("⚠️ [DESCARGAR] No se encontró botón con selector: " + config.downloadSelector);
        }
      } else {
        showMobileToast("No disponible en esta vista");
      }
    });
    
    // ANÁLISIS
    $(document).on("click.mobileButtons", "#mobile-btn-analysis", function(e) {
      e.stopPropagation();
      var sidebar = $(config.sidebarRightSelector).first();
      
      if (sidebar.length === 0) {
        // Fallback
        sidebar = $(".sidebar-right:visible").first();
        if (sidebar.length === 0) {
          sidebar = $(".sidebar-right").first();
        }
      }
      
      if (sidebar.length > 0) {
        openMobileDrawer(sidebar, "right");
        console.log("✅ [ANÁLISIS] Abriendo sidebar: " + sidebar.attr("id"));
      } else {
        showMobileToast("Análisis no disponible");
        console.log("⚠️ [ANÁLISIS] No se encontró sidebar con selector: " + config.sidebarRightSelector);
      }
    });
    
    // INFO (genérico para otras pestañas)
    $(document).on("click.mobileButtons", "#mobile-btn-info", function(e) {
      e.stopPropagation();
      showMobileToast("Información no disponible");
    });
  }
  
  // ============================================================
  // ✅ v3.5: DETECTAR CAMBIOS DE PESTAÑA
  // ============================================================
  
  function detectTabChange() {
    // Método 1: Observar cambios en input$main_tabs via Shiny
    $(document).on("shiny:inputchanged", function(event) {
      if (event.name === "main_tabs") {
        var newTab = event.value;
        console.log("📑 [v3.5] Cambio de pestaña detectado (shiny:inputchanged): " + newTab);
        updateMobileButtons(newTab);
      }
    });
    
    // Método 2: Observar clics en pestañas directamente
    $(document).on("shown.bs.tab", 'a[data-toggle="tab"]', function(e) {
      var tabId = $(e.target).attr("href");
      if (tabId) {
        // Extraer el ID de la pestaña del href (ej: "#shiny-tab-lista" -> "lista")
        var match = tabId.match(/tab[_-](\w+)/i);
        if (match) {
          var newTab = match[1];
          console.log("📑 [v3.5] Cambio de pestaña detectado (shown.bs.tab): " + newTab);
          updateMobileButtons(newTab);
        }
      }
    });
    
    // Método 3: Observar cambios en clases activas del tabsetPanel
    $(document).on("click", ".nav-tabs > li > a", function() {
      var $tab = $(this);
      var tabValue = $tab.attr("data-value") || $tab.closest("li").attr("data-value");
      
      if (tabValue) {
        console.log("📑 [v3.5] Cambio de pestaña detectado (click): " + tabValue);
        setTimeout(function() {
          updateMobileButtons(tabValue);
        }, 100);
      }
    });
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
  // SISTEMA MÓVIL - INICIALIZACIÓN v3.5
  // ============================================================
  
  function initMobileUI() {
    if (!isMobile()) {
      $(".mobile-fab-container").remove();
      $(".mobile-overlay").remove();
      $(".well, [class*='sidebar_panel']").removeClass("mobile-open");
      $(".sidebar-right").removeClass("mobile-open");
      return;
    }
    
    // Crear overlay si no existe
    if ($(".mobile-overlay").length === 0) {
      var overlay = $('<div class="mobile-overlay" id="mobile-overlay-main"></div>');
      $("body").prepend(overlay);
    }
    
    // Detectar pestaña actual
    var activeTab = $(".nav-tabs > li.active > a").attr("data-value") || 
                    $(".nav-tabs > li.active > a").attr("href");
    
    if (activeTab) {
      // Extraer ID si es un href
      var match = activeTab.match(/tab[_-](\w+)/i);
      currentTab = match ? match[1] : activeTab.replace("#", "");
    } else {
      currentTab = "lista"; // Default
    }
    
    console.log("📱 [v3.5] Inicializando UI móvil, pestaña activa: " + currentTab);
    
    // Crear botones para la pestaña actual
    updateMobileButtons(currentTab);
    
    // Configurar otros handlers
    setupSidebarAutoClose();
    setupDownloadButtonVisibility();
    setupOverlayClickHandler();
    preventKeyboardOnSelects();
    detectTabChange();
    
    console.log("✅ UI móvil v3.5 inicializada");
  }
  
  // ============================================================
  // PREVENCIÓN DE TECLADO (igual que v3.4)
  // ============================================================
  
  function preventKeyboardOnSelects() {
    if (!isMobile()) return;
    
    function makeSelectizeReadonly() {
      $(".selectize-input input").each(function() {
        $(this).attr("readonly", "readonly");
        $(this).attr("inputmode", "none");
        $(this).attr("autocomplete", "off");
        $(this).attr("autocorrect", "off");
        $(this).attr("autocapitalize", "off");
        $(this).attr("spellcheck", "false");
        $(this).css({
          "caret-color": "transparent",
          "-webkit-user-select": "none",
          "user-select": "none"
        });
      });
      
      $(".well select, [class*='sidebar_panel'] select").each(function() {
        $(this).attr("inputmode", "none");
      });
    }
    
    makeSelectizeReadonly();
    setTimeout(makeSelectizeReadonly, 500);
    setTimeout(makeSelectizeReadonly, 1000);
    setTimeout(makeSelectizeReadonly, 2000);
    
    $(document).on("mousedown touchstart", ".selectize-input", function(e) {
      $(this).find("input").attr("readonly", "readonly").attr("inputmode", "none");
    });
    
    $(document).on("focus", ".selectize-input input", function(e) {
      var $input = $(this);
      $input.attr("readonly", "readonly");
      $input.attr("inputmode", "none");
      
      setTimeout(function() {
        var $selectize = $input.closest(".selectize-control");
        var isDropdownOpen = $selectize.find(".selectize-dropdown").is(":visible");
        
        if (!isDropdownOpen) {
          $input.blur();
        }
      }, 50);
    });
    
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
    
    $(document).on("shiny:inputchanged shiny:value", function() {
      setTimeout(makeSelectizeReadonly, 100);
    });
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
  // VISIBILIDAD DEL BOTÓN DESCARGA (igual que v3.4)
  // ============================================================
  
  function setupDownloadButtonVisibility() {
    $(".mobile-download-container").addClass("hidden-until-ready");
    
    function checkForData() {
      var hasRealData = false;
      var debugInfo = [];
      
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
          debugInfo.push("Selector: " + selectors[i] + " (" + $found.length + " filas)");
          break;
        }
      }
      
      if (!$dataTable || $dataTable.length === 0) {
        updateButtonVisibility(false, debugInfo);
        return false;
      }
      
      var $firstRow = $dataTable.first();
      var $cells = $firstRow.find("td");
      
      if ($cells.length === 0) {
        updateButtonVisibility(false, debugInfo);
        return false;
      }
      
      var firstCellText = $cells.first().text().trim().toLowerCase();
      
      var noDataTexts = [
        "configure", "no hay datos", "mensaje", "sin datos",
        "no data", "loading", "cargando", "esperando"
      ];
      
      var isNoDataMessage = noDataTexts.some(function(text) {
        return firstCellText.includes(text);
      });
      
      if (isNoDataMessage) {
        updateButtonVisibility(false, debugInfo);
        return false;
      }
      
      var cellsWithContent = 0;
      $cells.each(function() {
        var text = $(this).text().trim();
        if (text && text.length > 0) {
          cellsWithContent++;
        }
      });
      
      hasRealData = cellsWithContent >= 2;
      
      var totalRows = $dataTable.length;
      if (totalRows === 1 && cellsWithContent < 3) {
        hasRealData = false;
      }
      
      updateButtonVisibility(hasRealData, debugInfo);
      return hasRealData;
    }
    
    function updateButtonVisibility(show, debugInfo) {
      var $container = $(".mobile-download-container");
      
      if ($container.length === 0) return;
      
      if (show) {
        $container.removeClass("hidden-until-ready");
      } else {
        $container.addClass("hidden-until-ready");
      }
    }
    
    setInterval(checkForData, 500);
    
    $(document).on("shiny:value", function(event) {
      if (event.name) {
        var name = event.name.toLowerCase();
        if (name.includes("table") || name.includes("data") || name.includes("main")) {
          setTimeout(checkForData, 200);
          setTimeout(checkForData, 500);
          setTimeout(checkForData, 1000);
          setTimeout(checkForData, 1500);
          setTimeout(checkForData, 2000);
        }
      }
    });
    
    $(document).on("draw.dt", function() {
      setTimeout(checkForData, 100);
      setTimeout(checkForData, 300);
      setTimeout(checkForData, 500);
    });
    
    setTimeout(checkForData, 500);
    setTimeout(checkForData, 1000);
    setTimeout(checkForData, 1500);
    setTimeout(checkForData, 2000);
    setTimeout(checkForData, 3000);
    setTimeout(checkForData, 5000);
  }
  
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
  window.updateMobileButtons = updateMobileButtons; // Exponer globalmente
  
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
  
  console.log("✅ sidebar_toggle.js v3.5 cargado");
  console.log("   ✅ Botones contextuales por pestaña");
  console.log("   ✅ Lista Nominal: FILTROS, RESTABLECER, PROYECCIÓN, ANÁLISIS");
  console.log("   ✅ Elecciones Federales: FILTROS, RESTABLECER, DESCARGAR, ANÁLISIS");
});
