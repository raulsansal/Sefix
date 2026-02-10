// ui/www/custom.js
// Versión: 2.2 - Limpieza: eliminado código de abreviaturas de leyendas
// Cambios v2.2:
//   - ELIMINADO: legendTextMap, shortenPlotlyLegends, restorePlotlyLegends
//   - ELIMINADO: Toda la lógica de modificación de leyendas post-render
//   - Las leyendas ahora se muestran tal como vienen del servidor R

// ============================================================
// DETECCIÓN DE ANCHO DE PANTALLA PARA SHINY
// ============================================================

$(document).ready(function() {
  
  // Función para enviar ancho de pantalla a Shiny
  function sendScreenWidth() {
    if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
      Shiny.setInputValue('screen_width', window.innerWidth, {priority: 'event'});
      console.log('📐 Ancho de pantalla enviado a Shiny: ' + window.innerWidth + 'px');
    }
  }
  
  // Enviar al cargar (con delay para asegurar que Shiny esté listo)
  setTimeout(sendScreenWidth, 500);
  
  // Enviar cuando Shiny esté conectado
  $(document).on('shiny:connected', function() {
    sendScreenWidth();
  });
  
  // Enviar al redimensionar (con debounce)
  var resizeTimer;
  $(window).on('resize', function() {
    clearTimeout(resizeTimer);
    resizeTimer = setTimeout(function() {
      sendScreenWidth();
    }, 250);
  });
  
  // Enviar al cambiar orientación
  $(window).on('orientationchange', function() {
    setTimeout(sendScreenWidth, 100);
  });
});

// ============================================================
// MANEJO DE OPCIÓN "EXTRAORDINARIA"
// ============================================================

Shiny.addCustomMessageHandler('disableExtraordinaria', function(disable) {
  $('#tipo_eleccion[value=EXTRAORDINARIA]').prop('disabled', disable);
  $('label[for="tipo_eleccionEXTRAORDINARIA"]').toggleClass('disabled-option', disable);
});

// ============================================================
// RESIZE DE GRÁFICOS PLOTLY - MEJORADO PARA MÓVIL
// ============================================================

// Función centralizada para resize de Plotly
function resizePlotlyGraphs(selector) {
  var $plots = selector ? $(selector) : $('.plotly, .js-plotly-plot');
  
  $plots.each(function() {
    var plotlyDiv = this;
    if (plotlyDiv && typeof Plotly !== 'undefined' && plotlyDiv.data) {
      try {
        Plotly.relayout(plotlyDiv, {
          'xaxis.autorange': true,
          'yaxis.autorange': true
        });
        Plotly.Plots.resize(plotlyDiv);
      } catch(e) {
        console.log('Error al redimensionar gráfico:', e);
      }
    }
  });
}

// Forzar resize de gráficos de Plotly para mantener responsividad
$(document).on('shiny:connected', function() {
  setTimeout(function() {
    resizePlotlyGraphs();
  }, 1000);
  
  // Detectar cambios de tab y forzar resize
  $('a[data-toggle="tab"]').on('shown.bs.tab', function (e) {
    setTimeout(function() {
      resizePlotlyGraphs('.tab-pane.active .plotly, .tab-pane.active .js-plotly-plot');
    }, 500);
  });
});

// Resize cuando cambia el tamaño de la ventana
$(window).on('resize', function() {
  clearTimeout(window.resizeTimer);
  window.resizeTimer = setTimeout(function() {
    resizePlotlyGraphs('.tab-pane.active .plotly, .tab-pane.active .js-plotly-plot');
  }, 250);
});

// Resize cuando cambia la orientación del dispositivo
$(window).on('orientationchange', function() {
  setTimeout(function() {
    resizePlotlyGraphs();
  }, 500);
});

// Forzar resize específico cuando se renderiza un nuevo gráfico
$(document).on('shiny:value', function(event) {
  if (event.name && (event.name.includes('grafico') || event.name.includes('plot'))) {
    setTimeout(function() {
      var targetId = '#' + event.name.replace(/:/g, '-');
      resizePlotlyGraphs(targetId);
    }, 500);
  }
});

// ============================================================
// MEJORAS PARA TOUCH EN MÓVIL
// ============================================================

// Detectar si es dispositivo touch
function isTouchDevice() {
  return 'ontouchstart' in window || navigator.maxTouchPoints > 0;
}

// Mejorar interacción táctil con selectize
$(document).on('shiny:connected', function() {
  if (isTouchDevice()) {
    $('body').addClass('touch-device');
    $('.selectize-input').css('min-height', '44px');
    $('.btn').css('min-height', '44px');
  }
});

// ============================================================
// PREVENIR ZOOM EN INPUT FOCUS (iOS)
// ============================================================

if (/iPhone|iPad|iPod/.test(navigator.userAgent)) {
  document.addEventListener('focusin', function(e) {
    if (e.target.tagName === 'INPUT' || e.target.tagName === 'SELECT' || e.target.tagName === 'TEXTAREA') {
      var viewport = document.querySelector('meta[name="viewport"]');
      if (viewport) {
        viewport.setAttribute('content', 'width=device-width, initial-scale=1, maximum-scale=1');
      }
    }
  });
  
  document.addEventListener('focusout', function(e) {
    var viewport = document.querySelector('meta[name="viewport"]');
    if (viewport) {
      viewport.setAttribute('content', 'width=device-width, initial-scale=1');
    }
  });
}

// ============================================================
// AJUSTE DINÁMICO DE PLOTLY PARA MÓVIL
// (Sin modificación de leyendas - v2.2)
// ============================================================

function adjustPlotlyForMobile() {
  if (window.innerWidth <= 768) {
    $('.plotly, .js-plotly-plot').each(function() {
      var plotlyDiv = this;
      if (plotlyDiv && typeof Plotly !== 'undefined' && plotlyDiv.layout) {
        try {
          var updates = {
            'title.font.size': 12,
            'xaxis.tickfont.size': 8,
            'yaxis.tickfont.size': 8,
            'legend.font.size': 7,
            'margin.t': 80,
            'margin.b': 100,
            'margin.l': 50,
            'margin.r': 20
          };
          
          Plotly.relayout(plotlyDiv, updates);
        } catch(e) {
          console.log('Error ajustando Plotly para móvil:', e);
        }
      }
    });
  }
}

// Ejecutar ajuste después de que se rendericen las gráficas
$(document).on('shiny:value', function(event) {
  if (event.name && (event.name.includes('grafico') || event.name.includes('plot'))) {
    setTimeout(adjustPlotlyForMobile, 600);
  }
});

// También ejecutar al cambiar de tab
$(document).on('shown.bs.tab', function() {
  setTimeout(adjustPlotlyForMobile, 600);
});

// Ejecutar al redimensionar ventana
$(window).on('resize', function() {
  clearTimeout(window.adjustTimer);
  window.adjustTimer = setTimeout(function() {
    adjustPlotlyForMobile();
  }, 300);
});

// Ejecutar al cargar página
$(document).ready(function() {
  setTimeout(adjustPlotlyForMobile, 1500);
});

console.log('✅ custom.js v2.2 cargado (sin modificación de leyendas)');
