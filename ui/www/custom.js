// ui/www/custom.js
// Versión: 2.7 - SOLUCIÓN DEFINITIVA: Ajustes via JavaScript post-render
// Cambios v2.7:
//   - Card NB se ajusta DESPUÉS del render via JavaScript (no depende de R)
//   - Texto de alcance se reposiciona a y=1.18 en móvil
//   - Hint cambia a "(Clic para desglose)" en móvil
//   - Card NB se compacta 50% en móvil
//   - NO DEPENDE de input$screen_width en R

// ============================================================
// DETECCIÓN DE ANCHO DE PANTALLA PARA SHINY
// ============================================================

$(document).ready(function() {
  
  function sendScreenWidth() {
    if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
      Shiny.setInputValue('screen_width', window.innerWidth, {priority: 'event'});
      console.log('📐 Ancho de pantalla enviado a Shiny: ' + window.innerWidth + 'px');
    }
  }
  
  // Enviar múltiples veces para asegurar que llegue
  setTimeout(sendScreenWidth, 100);
  setTimeout(sendScreenWidth, 300);
  setTimeout(sendScreenWidth, 500);
  setTimeout(sendScreenWidth, 1000);
  
  $(document).on('shiny:connected', function() {
    sendScreenWidth();
  });
  
  var resizeTimer;
  $(window).on('resize', function() {
    clearTimeout(resizeTimer);
    resizeTimer = setTimeout(function() {
      sendScreenWidth();
    }, 250);
  });
  
  $(window).on('orientationchange', function() {
    setTimeout(sendScreenWidth, 100);
  });
});

// ============================================================
// MANEJO DE OPCIÓN "EXTRAORDINARIA"
// ============================================================

if (typeof Shiny !== 'undefined') {
  Shiny.addCustomMessageHandler('disableExtraordinaria', function(disable) {
    $('#tipo_eleccion[value=EXTRAORDINARIA]').prop('disabled', disable);
    $('label[for="tipo_eleccionEXTRAORDINARIA"]').toggleClass('disabled-option', disable);
  });
}

// ============================================================
// RESIZE DE GRÁFICOS PLOTLY
// ============================================================

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

$(document).on('shiny:connected', function() {
  setTimeout(function() {
    resizePlotlyGraphs();
  }, 1000);
  
  $('a[data-toggle="tab"]').on('shown.bs.tab', function (e) {
    setTimeout(function() {
      resizePlotlyGraphs('.tab-pane.active .plotly, .tab-pane.active .js-plotly-plot');
    }, 500);
  });
});

$(window).on('resize', function() {
  clearTimeout(window.resizeTimer);
  window.resizeTimer = setTimeout(function() {
    resizePlotlyGraphs('.tab-pane.active .plotly, .tab-pane.active .js-plotly-plot');
  }, 250);
});

$(window).on('orientationchange', function() {
  setTimeout(function() {
    resizePlotlyGraphs();
  }, 500);
});

// ============================================================
// MEJORAS PARA TOUCH EN MÓVIL
// ============================================================

function isTouchDevice() {
  return 'ontouchstart' in window || navigator.maxTouchPoints > 0;
}

function isMobileView() {
  return window.innerWidth <= 768;
}

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
// ✅ v2.7: AJUSTE COMPLETO DE PLOTLY PARA MÓVIL (POST-RENDER)
// Esta es la función principal que hace TODO el trabajo
// ============================================================

function adjustPlotlyForMobile() {
  if (!isMobileView()) {
    console.log('🖥️ [JS] Vista desktop detectada, sin ajustes móviles');
    return;
  }
  
  console.log('📱 [JS v2.7] Ajustando gráficas para móvil (width=' + window.innerWidth + 'px)');
  
  $('.plotly, .js-plotly-plot').each(function() {
    var plotlyDiv = this;
    if (!plotlyDiv || typeof Plotly === 'undefined' || !plotlyDiv.layout) {
      return;
    }
    
    try {
      var numTrazas = 0;
      if (plotlyDiv.data && Array.isArray(plotlyDiv.data)) {
        numTrazas = plotlyDiv.data.length;
      }
      
      // Posición de Fuente según número de trazas
      var fuenteYPosition = numTrazas >= 4 ? -0.60 : -0.40;
      
      if (!plotlyDiv.layout.annotations || !Array.isArray(plotlyDiv.layout.annotations)) {
        // Sin annotations, solo ajustar layout general
        Plotly.relayout(plotlyDiv, {
          'title.font.size': 12,
          'xaxis.tickfont.size': 8,
          'yaxis.tickfont.size': 8,
          'legend.font.size': 7,
          'margin.t': 85,
          'margin.b': 140,
          'margin.l': 50,
          'margin.r': 20
        });
        return;
      }
      
      var newAnnotations = [];
      var hasChanges = false;
      
      for (var i = 0; i < plotlyDiv.layout.annotations.length; i++) {
        var ann = plotlyDiv.layout.annotations[i];
        var updatedAnn = JSON.parse(JSON.stringify(ann)); // Deep clone
        
        // =====================================================
        // 1. AJUSTAR TEXTO "FUENTE"
        // =====================================================
        if (ann.text && ann.text.indexOf('Fuente:') !== -1) {
          if (updatedAnn.y !== fuenteYPosition) {
            updatedAnn.y = fuenteYPosition;
            hasChanges = true;
            console.log('   📊 Fuente ajustada a y=' + fuenteYPosition);
          }
        }
        
        // =====================================================
        // 2. AJUSTAR TEXTO DE ALCANCE (subir ~20px)
        // =====================================================
        if (ann.text && (
            ann.text.indexOf('Estado:') !== -1 || 
            ann.text.indexOf('Entidad:') !== -1 || 
            ann.text.indexOf('Distrito:') !== -1 ||
            ann.text.indexOf('Municipio:') !== -1 ||
            ann.text.indexOf('Sección:') !== -1
        )) {
          // Solo si NO es la card NB
          if (ann.text.indexOf('No Binario') === -1 && ann.text.indexOf('⚧') === -1) {
            // Subir el texto de alcance (de ~1.12 a 1.18)
            if (updatedAnn.y < 1.17) {
              updatedAnn.y = 1.18;
              hasChanges = true;
              console.log('   📊 Texto alcance subido a y=1.18');
            }
            // Reducir tamaño de fuente
            updatedAnn.font = updatedAnn.font || {};
            updatedAnn.font.size = 11;
          }
        }
        
        // =====================================================
        // 3. AJUSTAR CARD NB (reducir 50%, cambiar hint)
        // =====================================================
        if (ann.text && (ann.text.indexOf('No Binario') !== -1 || ann.text.indexOf('⚧') !== -1)) {
          console.log('   🎯 Card NB detectada, aplicando ajustes móviles...');
          
          // 3.1 Cambiar hint de "Hover" a "Clic"
          if (updatedAnn.text.indexOf('Hover para desglose') !== -1) {
            updatedAnn.text = updatedAnn.text.replace(/Hover para desglose/g, 'Clic para desglose');
            hasChanges = true;
            console.log('      ✅ Hint cambiado a "Clic para desglose"');
          }
          
          // 3.2 Reducir tamaños de fuente (de desktop a móvil)
          // Desktop: 14px, 12px, 11px, 10px, 9px
          // Móvil:   8px,  7px,  6px,  6px,  5px
          updatedAnn.text = updatedAnn.text
            .replace(/font-size:\s*14px/g, 'font-size:8px')
            .replace(/font-size:\s*12px/g, 'font-size:7px')
            .replace(/font-size:\s*11px/g, 'font-size:6px')
            .replace(/font-size:\s*10px/g, 'font-size:6px')
            .replace(/font-size:\s*9px/g, 'font-size:5px');
          
          // 3.3 Agregar line-height 1.5 a cada span (si no existe)
          if (updatedAnn.text.indexOf('line-height') === -1) {
            updatedAnn.text = updatedAnn.text.replace(/<span style='/g, "<span style='line-height:1.5; ");
          }
          
          // 3.4 Acortar etiquetas
          updatedAnn.text = updatedAnn.text
            .replace(/Padrón Nacional/g, 'Padrón Nac.')
            .replace(/Lista Nacional/g, 'Lista Nac.')
            .replace(/Padrón Extranjero/g, 'Padrón Ext.')
            .replace(/Lista Extranjero/g, 'Lista Ext.');
          
          // 3.5 Reducir padding y borde
          updatedAnn.borderpad = 2;
          updatedAnn.borderwidth = 1;
          
          // 3.6 Mover a esquina superior izquierda
          updatedAnn.x = 0.02;
          updatedAnn.y = 0.98;
          updatedAnn.xanchor = 'left';
          updatedAnn.yanchor = 'top';
          
          hasChanges = true;
          console.log('      ✅ Card NB compactada para móvil');
        }
        
        newAnnotations.push(updatedAnn);
      }
      
      // Aplicar cambios si los hubo
      if (hasChanges) {
        Plotly.relayout(plotlyDiv, {
          'annotations': newAnnotations,
          'title.font.size': 12,
          'xaxis.tickfont.size': 8,
          'yaxis.tickfont.size': 8,
          'legend.font.size': 7,
          'margin.t': 85,
          'margin.b': 140,
          'margin.l': 50,
          'margin.r': 20
        });
        console.log('✅ [JS v2.7] Gráfica ajustada para móvil');
      }
      
    } catch(e) {
      console.log('❌ [JS] Error ajustando Plotly para móvil:', e);
    }
  });
}

// ============================================================
// SIMULAR HOVER EN MÓVIL PARA CARD NB (TOOLTIP)
// ============================================================

var nbTooltipActive = false;
var activeNBAnnotation = null;

function isNBCardElement(element) {
  if (!element) return false;
  var text = element.textContent || element.innerText || '';
  return text.indexOf('No Binario') !== -1 || 
         text.indexOf('⚧') !== -1 || 
         text.indexOf('Clic para desglose') !== -1;
}

function setupMobileNBTooltip() {
  if (!isMobileView()) return;
  
  $(document).off('click.nbtooltip touchend.nbtooltip');
  
  $(document).on('click.nbtooltip touchend.nbtooltip', '.plotly, .js-plotly-plot', function(e) {
    if (!isMobileView()) return;
    
    var target = e.target;
    var $target = $(target);
    var plotlyDiv = this;
    
    var $annotation = $target.closest('.annotation');
    
    if ($annotation.length > 0 && isNBCardElement($annotation[0])) {
      e.preventDefault();
      e.stopPropagation();
      
      console.log('✅ [NB Mobile] Clic en card NB detectado');
      
      if (nbTooltipActive && activeNBAnnotation === $annotation[0]) {
        hideNBTooltip(plotlyDiv);
      } else {
        showNBTooltip(plotlyDiv, $annotation[0]);
      }
      
      return false;
    }
  });
  
  $(document).on('click.nbclose touchend.nbclose', function(e) {
    if (!nbTooltipActive) return;
    
    var $target = $(e.target);
    
    if ($target.closest('.hoverlayer').length > 0) {
      return;
    }
    
    var $annotation = $target.closest('.annotation');
    if ($annotation.length > 0 && isNBCardElement($annotation[0])) {
      return;
    }
    
    hideAllNBTooltips();
  });
  
  console.log('✅ [NB Mobile] Handler de tooltip configurado');
}

function showNBTooltip(plotlyDiv, annotationElement) {
  try {
    var rect = annotationElement.getBoundingClientRect();
    
    var hoverEvent = new MouseEvent('mouseover', {
      bubbles: true,
      cancelable: true,
      clientX: rect.left + rect.width / 2,
      clientY: rect.top + rect.height / 2
    });
    
    annotationElement.dispatchEvent(hoverEvent);
    
    nbTooltipActive = true;
    activeNBAnnotation = annotationElement;
    
    $(annotationElement).addClass('nb-tooltip-active');
    
    console.log('✅ [NB Mobile] Tooltip mostrado');
    
  } catch (e) {
    console.log('⚠️ [NB Mobile] Error mostrando tooltip:', e);
  }
}

function hideNBTooltip(plotlyDiv) {
  try {
    if (activeNBAnnotation) {
      var mouseoutEvent = new MouseEvent('mouseout', {
        bubbles: true,
        cancelable: true
      });
      
      activeNBAnnotation.dispatchEvent(mouseoutEvent);
      $(activeNBAnnotation).removeClass('nb-tooltip-active');
    }
    
    if (plotlyDiv && typeof Plotly !== 'undefined') {
      Plotly.Fx.hover(plotlyDiv, []);
    }
    
    nbTooltipActive = false;
    activeNBAnnotation = null;
    
  } catch (e) {
    console.log('⚠️ [NB Mobile] Error ocultando tooltip:', e);
  }
}

function hideAllNBTooltips() {
  $('.plotly, .js-plotly-plot').each(function() {
    hideNBTooltip(this);
  });
  
  $('.annotation.nb-tooltip-active').removeClass('nb-tooltip-active');
  
  nbTooltipActive = false;
  activeNBAnnotation = null;
}

// ============================================================
// EJECUTAR AJUSTES EN DIFERENTES MOMENTOS
// ============================================================

// Cuando Shiny renderiza una gráfica
$(document).on('shiny:value', function(event) {
  if (event.name && (event.name.indexOf('grafico') !== -1 || event.name.indexOf('plot') !== -1)) {
    console.log('📊 [JS] Gráfica renderizada: ' + event.name);
    // Múltiples intentos para asegurar que se apliquen
    setTimeout(adjustPlotlyForMobile, 300);
    setTimeout(adjustPlotlyForMobile, 600);
    setTimeout(adjustPlotlyForMobile, 1000);
    setTimeout(adjustPlotlyForMobile, 1500);
    setTimeout(adjustPlotlyForMobile, 2500);
  }
});

// Al cambiar de pestaña
$(document).on('shown.bs.tab', function() {
  setTimeout(adjustPlotlyForMobile, 400);
  setTimeout(adjustPlotlyForMobile, 800);
  hideAllNBTooltips();
});

// Al redimensionar
$(window).on('resize', function() {
  clearTimeout(window.adjustTimer);
  window.adjustTimer = setTimeout(function() {
    adjustPlotlyForMobile();
  }, 300);
  hideAllNBTooltips();
});

// Al cargar el documento
$(document).ready(function() {
  setupMobileNBTooltip();
  setTimeout(adjustPlotlyForMobile, 2000);
  setTimeout(adjustPlotlyForMobile, 3500);
  setTimeout(adjustPlotlyForMobile, 5000);
});

// Cuando Shiny se conecta
$(document).on('shiny:connected', function() {
  setTimeout(adjustPlotlyForMobile, 2500);
  setTimeout(adjustPlotlyForMobile, 4000);
  setTimeout(adjustPlotlyForMobile, 6000);
});

console.log('✅ custom.js v2.7 cargado - SOLUCIÓN DEFINITIVA');
console.log('   ✅ Ajustes via JavaScript post-render (no depende de R)');
console.log('   ✅ Card NB: hint, tamaños, posición ajustados en móvil');
console.log('   ✅ Texto alcance: subido a y=1.18 en móvil');
