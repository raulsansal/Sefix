// ui/www/custom.js
// Versión: 2.3 - Ajuste dinámico de Plotly para mobile (posición Fuente y texto alcance)
// Cambios v2.3:
//   - Ajuste de posición "Fuente" según número de trazas en mobile
//   - 4 trazas: y = -0.60 (para liberar espacio a leyendas)
//   - 2 trazas: y = -0.40
//   - Texto de alcance: +2px en mobile (font size 15 en lugar de 13)
//   - Desktop: mantiene valores originales (y = -0.35)

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

// Detectar si es móvil
function isMobileView() {
  return window.innerWidth <= 768;
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
// ✅ v2.3: AJUSTE DINÁMICO DE PLOTLY PARA MÓVIL
// Ajusta posición de "Fuente" y tamaño de texto de alcance
// ============================================================

function adjustPlotlyForMobile() {
  if (!isMobileView()) {
    return; // No hacer nada en desktop
  }
  
  $('.plotly, .js-plotly-plot').each(function() {
    var plotlyDiv = this;
    if (plotlyDiv && typeof Plotly !== 'undefined' && plotlyDiv.layout) {
      try {
        // ========== CONTAR NÚMERO DE TRAZAS ==========
        var numTrazas = 0;
        if (plotlyDiv.data && Array.isArray(plotlyDiv.data)) {
          numTrazas = plotlyDiv.data.length;
        }
        
        // ========== DETERMINAR POSICIÓN Y DE "FUENTE" ==========
        // 4+ trazas: y = -0.60 (más espacio para leyendas en 2 líneas)
        // 2-3 trazas: y = -0.40 (espaciado normal)
        var fuenteYPosition = numTrazas >= 4 ? -0.60 : -0.40;
        
        // ========== AJUSTAR ANNOTATIONS ==========
        if (plotlyDiv.layout.annotations && Array.isArray(plotlyDiv.layout.annotations)) {
          var newAnnotations = [];
          
          plotlyDiv.layout.annotations.forEach(function(ann, index) {
            var updatedAnn = Object.assign({}, ann);
            
            // Detectar annotation de "Fuente" por su contenido
            if (ann.text && ann.text.includes('Fuente:')) {
              updatedAnn.y = fuenteYPosition;
              console.log('📊 Ajustando Fuente a y=' + fuenteYPosition + ' (trazas: ' + numTrazas + ')');
            }
            
            // Detectar annotation de alcance (texto del filtro) y aumentar tamaño
            // El texto de alcance está en y = 1.12 y contiene "Estado:" o "Entidad:" o similar
            if (ann.y === 1.12 || (ann.text && (
                ann.text.includes('Estado:') || 
                ann.text.includes('Entidad:') || 
                ann.text.includes('Nacional') ||
                ann.text.includes('Distrito:') ||
                ann.text.includes('Municipio:')
            ))) {
              // Aumentar 2px el tamaño del texto (de 13 a 15)
              if (ann.font) {
                updatedAnn.font = Object.assign({}, ann.font, { size: 15 });
              } else {
                updatedAnn.font = { size: 15 };
              }
            }
            
            newAnnotations.push(updatedAnn);
          });
          
          // Aplicar cambios usando Plotly.relayout
          Plotly.relayout(plotlyDiv, {
            'annotations': newAnnotations,
            'title.font.size': 12,
            'xaxis.tickfont.size': 8,
            'yaxis.tickfont.size': 8,
            'legend.font.size': 7,
            'margin.t': 80,
            'margin.b': 140,
            'margin.l': 50,
            'margin.r': 20
          });
        } else {
          // Si no hay annotations, solo ajustar otros parámetros
          Plotly.relayout(plotlyDiv, {
            'title.font.size': 12,
            'xaxis.tickfont.size': 8,
            'yaxis.tickfont.size': 8,
            'legend.font.size': 7,
            'margin.t': 80,
            'margin.b': 140,
            'margin.l': 50,
            'margin.r': 20
          });
        }
        
      } catch(e) {
        console.log('Error ajustando Plotly para móvil:', e);
      }
    }
  });
}

// ============================================================
// EJECUTAR AJUSTES EN DIFERENTES MOMENTOS
// ============================================================

// Ejecutar ajuste después de que se rendericen las gráficas
$(document).on('shiny:value', function(event) {
  if (event.name && (event.name.includes('grafico') || event.name.includes('plot'))) {
    // Múltiples intentos para asegurar que el gráfico esté completamente renderizado
    setTimeout(adjustPlotlyForMobile, 300);
    setTimeout(adjustPlotlyForMobile, 600);
    setTimeout(adjustPlotlyForMobile, 1000);
  }
});

// También ejecutar al cambiar de tab
$(document).on('shown.bs.tab', function() {
  setTimeout(adjustPlotlyForMobile, 300);
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
  setTimeout(adjustPlotlyForMobile, 3000);
});

// Ejecutar cuando Shiny esté conectado
$(document).on('shiny:connected', function() {
  setTimeout(adjustPlotlyForMobile, 2000);
});

console.log('✅ custom.js v2.3 cargado (ajuste dinámico de Fuente y alcance para mobile)');
