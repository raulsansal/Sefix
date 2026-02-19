// ui/www/sidebar_toggle.js
// Versión: 3.11 - Fix completo Mostrar/Buscar
// Cambios v3.11:
//   - Columnas con display:flex y justify-content para alineación correcta
//   - Select y Input con font-size: 12px (reducido de 16px)
//   - font-weight: normal en todos los elementos (sin negritas)
//   - Estilos aplicados al SELECT dentro de Mostrar

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
  
  var currentTab = "lista";
  
  // ============================================================
  // ✅ v3.11: FIX DATATABLE MOSTRAR/BUSCAR EN UNA LÍNEA
  // Enfoque: SOLO aplicar estilos, NO ocultar ni clonar nada
  // ============================================================
  
  function fixDataTableControls() {
    if (!isMobile()) return;
    
    // Buscar todos los wrappers de DataTable
    $(".dataTables_wrapper").each(function() {
      var $wrapper = $(this);
      
      // Buscar la primera fila .row que contiene length y filter
      var $firstRow = $wrapper.children(".row").first();
      
      if ($firstRow.length === 0) {
        // Intentar con div directo que contenga los controles
        $wrapper.children("div").each(function() {
          var $div = $(this);
          if ($div.find(".dataTables_length").length > 0 && $div.find(".dataTables_filter").length > 0) {
            $firstRow = $div;
            return false; // break
          }
        });
      }
      
      if ($firstRow.length === 0) return;
      
      // Verificar si ya tiene la clase de fix
      if ($firstRow.hasClass("mobile-dt-controls-fixed")) return;
      
      // ✅ v3.11: Aplicar estilos flex a la fila contenedora
      $firstRow.attr('style', 
        'display: flex !important;' +
        'flex-direction: row !important;' +
        'flex-wrap: nowrap !important;' +
        'justify-content: space-between !important;' +
        'align-items: center !important;' +
        'width: 100% !important;' +
        'margin: 0 0 10px 0 !important;' +
        'padding: 0 5px !important;' +
        'box-sizing: border-box !important;'
      ).addClass("mobile-dt-controls-fixed");
      
      // ✅ v3.11: Aplicar flex: 1 a ambas columnas para que space-between funcione
      var $children = $firstRow.children();
      
      $children.each(function() {
        $(this).attr('style',
          'width: auto !important;' +
          'max-width: 50% !important;' +
          'flex: 0 1 auto !important;' +
          'padding: 0 !important;' +
          'float: none !important;' +
          'display: flex !important;'
        );
      });
      
      // Primera columna - alinear contenido a la izquierda
      $children.first().css('justify-content', 'flex-start');
      
      // Segunda columna - alinear contenido a la derecha
      $children.last().css('justify-content', 'flex-end');
      
      // ✅ v3.11: Estilos para dataTables_length (Mostrar)
      var $length = $wrapper.find(".dataTables_length");
      $length.attr('style',
        'display: inline-flex !important;' +
        'align-items: center !important;' +
        'float: none !important;' +
        'margin: 0 !important;' +
        'padding: 0 !important;'
      );
      
      $length.find('label').attr('style',
        'display: inline-flex !important;' +
        'align-items: center !important;' +
        'gap: 3px !important;' +
        'margin: 0 !important;' +
        'font-size: 11px !important;' +
        'font-weight: normal !important;' +
        'white-space: nowrap !important;' +
        'color: #333 !important;'
      );
      
      // ✅ v3.11: Estilos para el SELECT dentro de Mostrar
      $length.find('select').attr('style',
        'font-size: 12px !important;' +
        'font-weight: normal !important;' +
        'padding: 2px 4px !important;' +
        'height: 24px !important;' +
        'min-width: 45px !important;' +
        'margin: 0 2px !important;' +
        'border: 1px solid #ccc !important;' +
        'border-radius: 3px !important;'
      );
      
      // ✅ v3.11: Estilos para dataTables_filter (Buscar)
      var $filter = $wrapper.find(".dataTables_filter");
      $filter.attr('style',
        'display: inline-flex !important;' +
        'align-items: center !important;' +
        'float: none !important;' +
        'margin: 0 !important;' +
        'padding: 0 !important;'
      );
      
      $filter.find('label').attr('style',
        'display: inline-flex !important;' +
        'align-items: center !important;' +
        'gap: 3px !important;' +
        'margin: 0 !important;' +
        'font-size: 11px !important;' +
        'font-weight: normal !important;' +
        'white-space: nowrap !important;' +
        'color: #333 !important;'
      );
      
      // ✅ v3.11: Estilos para el INPUT dentro de Buscar
      $filter.find('input').attr('style',
        'width: 70px !important;' +
        'font-size: 12px !important;' +
        'font-weight: normal !important;' +
        'padding: 2px 4px !important;' +
        'height: 24px !important;' +
        'margin-left: 3px !important;' +
        'border: 1px solid #ccc !important;' +
        'border-radius: 3px !important;'
      );
      
      console.log("✅ [v3.11] DataTable controls: estilos aplicados correctamente");
    });
  }
  
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
    
    console.log("📱 [v3.9] Actualizando botones para pestaña: " + currentTab);
    
    $(".mobile-fab-container").remove();
    
    var buttonsHtml = config.buttons.map(function(btn) {
      return '<button class="mobile-fab-btn" id="' + btn.id + '" type="button" data-action="' + btn.action + '">' +
        '<span class="fab-icon">' + btn.icon + '</span>' +
        '<span class="fab-label">' + btn.label + '</span>' +
      '</button>';
    }).join('');
    
    var fabContainer = $('<div class="mobile-fab-container">' + buttonsHtml + '</div>');
    $("body").append(fabContainer);
    
    setupButtonHandlers(config);
    
    console.log("✅ [v3.9] Botones actualizados: " + config.buttons.map(function(b) { return b.label; }).join(", "));
  }
  
  // ============================================================
  // ✅ v3.5: CONFIGURAR HANDLERS DE BOTONES
  // ============================================================
  
  function setupButtonHandlers(config) {
    $(document).off("click.mobileButtons");
    
    // FILTROS
    $(document).on("click.mobileButtons", "#mobile-btn-filters", function(e) {
      e.stopPropagation();
      var sidebar = $(config.sidebarSelector).first();
      
      if (sidebar.length === 0) {
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
      }
    });
    
    // RESTABLECER
    $(document).on("click.mobileButtons", "#mobile-btn-restore", function(e) {
      e.stopPropagation();
      var resetBtn = $(config.restoreSelector).first();
      
      if (resetBtn.length > 0) {
        resetBtn.trigger("click");
        showMobileToast("Consulta restablecida");
      } else {
        showMobileToast("No disponible en esta vista");
      }
    });
    
    // PROYECCIÓN
    $(document).on("click.mobileButtons", "#mobile-btn-projection", function(e) {
      e.stopPropagation();
      if (config.projectionSelector) {
        var metodologiaBtn = $(config.projectionSelector).first();
        
        if (metodologiaBtn.length > 0) {
          metodologiaBtn.trigger("click");
        } else {
          showMobileToast("No disponible");
        }
      } else {
        showMobileToast("No disponible en esta vista");
      }
    });
    
    // DESCARGAR
    $(document).on("click.mobileButtons", "#mobile-btn-download", function(e) {
      e.stopPropagation();
      if (config.downloadSelector) {
        var downloadBtn = $(config.downloadSelector).first();
        
        if (downloadBtn.length > 0) {
          downloadBtn[0].click();
          showMobileToast("Descargando CSV...");
        } else {
          showMobileToast("No hay datos para descargar");
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
        sidebar = $(".sidebar-right:visible").first();
        if (sidebar.length === 0) {
          sidebar = $(".sidebar-right").first();
        }
      }
      
      if (sidebar.length > 0) {
        openMobileDrawer(sidebar, "right");
      } else {
        showMobileToast("Análisis no disponible");
      }
    });
    
    // INFO
    $(document).on("click.mobileButtons", "#mobile-btn-info", function(e) {
      e.stopPropagation();
      showMobileToast("Información no disponible");
    });
  }
  
  // ============================================================
  // ✅ v3.5: DETECTAR CAMBIOS DE PESTAÑA
  // ============================================================
  
  function detectTabChange() {
    $(document).on("shiny:inputchanged", function(event) {
      if (event.name === "main_tabs") {
        var newTab = event.value;
        console.log("📑 [v3.9] Cambio de pestaña detectado: " + newTab);
        updateMobileButtons(newTab);
      }
    });
    
    $(document).on("shown.bs.tab", 'a[data-toggle="tab"]', function(e) {
      var tabId = $(e.target).attr("href");
      if (tabId) {
        var match = tabId.match(/tab[_-](\w+)/i);
        if (match) {
          var newTab = match[1];
          updateMobileButtons(newTab);
        }
      }
    });
    
    $(document).on("click", ".nav-tabs > li > a", function() {
      var $tab = $(this);
      var tabValue = $tab.attr("data-value") || $tab.closest("li").attr("data-value");
      
      if (tabValue) {
        setTimeout(function() {
          updateMobileButtons(tabValue);
        }, 100);
      }
    });
  }
  
  // ============================================================
  // INICIALIZACIÓN DESKTOP
  // ============================================================
  
  $(".sidebar-right").each(function() {
    var sidebarId = $(this).attr("id");
    var toggleBtn = $("[data-sidebar-id='" + sidebarId + "']");
    var toggleContainer = toggleBtn.closest(".toggle-container");
    
    toggleBtn.text(">>");
    toggleContainer.attr("data-for-sidebar", sidebarId);
  });
  
  // ============================================================
  // MANEJADOR TOGGLE DESKTOP
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
  // SISTEMA MÓVIL - INICIALIZACIÓN v3.9
  // ============================================================
  
  function initMobileUI() {
    if (!isMobile()) {
      $(".mobile-fab-container").remove();
      $(".mobile-overlay").remove();
      $(".well, [class*='sidebar_panel']").removeClass("mobile-open");
      $(".sidebar-right").removeClass("mobile-open");
      return;
    }
    
    if ($(".mobile-overlay").length === 0) {
      var overlay = $('<div class="mobile-overlay" id="mobile-overlay-main"></div>');
      $("body").prepend(overlay);
    }
    
    var activeTab = $(".nav-tabs > li.active > a").attr("data-value") || 
                    $(".nav-tabs > li.active > a").attr("href");
    
    if (activeTab) {
      var match = activeTab.match(/tab[_-](\w+)/i);
      currentTab = match ? match[1] : activeTab.replace("#", "");
    } else {
      currentTab = "lista";
    }
    
    console.log("📱 [v3.9] Inicializando UI móvil, pestaña activa: " + currentTab);
    
    updateMobileButtons(currentTab);
    
    setupSidebarAutoClose();
    setupDownloadButtonVisibility();
    setupOverlayClickHandler();
    preventKeyboardOnSelects();
    detectTabChange();
    setupDataTableFix();
    
    console.log("✅ UI móvil v3.9 inicializada");
  }
  
  // ============================================================
  // ✅ v3.9: CONFIGURAR FIX PARA DATATABLE
  // ============================================================
  
  function setupDataTableFix() {
    if (!isMobile()) return;
    
    // Ejecutar inmediatamente
    fixDataTableControls();
    
    // Ejecutar cuando DataTable se dibuja
    $(document).on("draw.dt", function() {
      // Remover marca para permitir re-aplicar estilos
      $(".mobile-dt-controls-fixed").removeClass("mobile-dt-controls-fixed");
      setTimeout(fixDataTableControls, 50);
      setTimeout(fixDataTableControls, 200);
    });
    
    // Ejecutar cuando Shiny actualiza valores
    $(document).on("shiny:value", function(event) {
      if (event.name && event.name.toLowerCase().includes("table")) {
        $(".mobile-dt-controls-fixed").removeClass("mobile-dt-controls-fixed");
        setTimeout(fixDataTableControls, 100);
        setTimeout(fixDataTableControls, 300);
        setTimeout(fixDataTableControls, 500);
      }
    });
    
    // Ejecutar con MutationObserver para detectar nuevos DataTables
    var dtObserver = new MutationObserver(function(mutations) {
      var needsFix = false;
      mutations.forEach(function(mutation) {
        if (mutation.addedNodes.length) {
          $(mutation.addedNodes).each(function() {
            if ($(this).find(".dataTables_wrapper").length > 0 || 
                $(this).hasClass("dataTables_wrapper")) {
              needsFix = true;
            }
          });
        }
      });
      
      if (needsFix) {
        setTimeout(fixDataTableControls, 100);
      }
    });
    
    dtObserver.observe(document.body, {
      childList: true,
      subtree: true
    });
    
    // Ejecutar en intervalos por si acaso
    setTimeout(fixDataTableControls, 500);
    setTimeout(fixDataTableControls, 1000);
    setTimeout(fixDataTableControls, 2000);
    setTimeout(fixDataTableControls, 3000);
  }
  
  // ============================================================
  // PREVENCIÓN DE TECLADO
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
  // VISIBILIDAD DEL BOTÓN DESCARGA
  // ============================================================
  
  function setupDownloadButtonVisibility() {
    $(".mobile-download-container").addClass("hidden-until-ready");
    
    function checkForData() {
      var hasRealData = false;
      
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
          break;
        }
      }
      
      if (!$dataTable || $dataTable.length === 0) {
        updateButtonVisibility(false);
        return false;
      }
      
      var $firstRow = $dataTable.first();
      var $cells = $firstRow.find("td");
      
      if ($cells.length === 0) {
        updateButtonVisibility(false);
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
        updateButtonVisibility(false);
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
      
      updateButtonVisibility(hasRealData);
      return hasRealData;
    }
    
    function updateButtonVisibility(show) {
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
        }
      }
    });
    
    $(document).on("draw.dt", function() {
      setTimeout(checkForData, 100);
      setTimeout(checkForData, 300);
    });
    
    setTimeout(checkForData, 500);
    setTimeout(checkForData, 1000);
    setTimeout(checkForData, 2000);
    setTimeout(checkForData, 3000);
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
  window.updateMobileButtons = updateMobileButtons;
  window.fixDataTableControls = fixDataTableControls;
  
  // ============================================================
  // RESIZE HANDLER
  // ============================================================
  
  var resizeTimeout;
  $(window).on("resize", function() {
    clearTimeout(resizeTimeout);
    resizeTimeout = setTimeout(function() {
      if (isMobile()) {
        // Limpiar y reinicializar
        $(".mobile-dt-controls-fixed").removeClass("mobile-dt-controls-fixed");
        initMobileUI();
      } else {
        $(".mobile-fab-container").remove();
        $(".mobile-overlay").remove();
        $(".mobile-toast").remove();
        $(".mobile-dt-controls-fixed").removeClass("mobile-dt-controls-fixed");
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
      $(".mobile-dt-controls-fixed").removeClass("mobile-dt-controls-fixed");
      fixDataTableControls();
    }
    setTimeout(triggerPlotlyResize, 300);
  });
  
  console.log("✅ sidebar_toggle.js v3.11 cargado");
  console.log("   ✅ Botones contextuales por pestaña");
  console.log("   ✅ Fix DataTable Mostrar(izq)/Buscar(der) con fuentes reducidas");
});
