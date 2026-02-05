// ui/www/custom_select_input.js
$(document).ready(function() {
  function initializeSelectizeRemoval() {
    $('.selectize-control').each(function() {
      var $control = $(this);
      var selectizeId = $control.prev('select').attr('id');
      
      if (selectizeId) {
        $control.on('click', '.item', function(e) {
          var $item = $(this);
          if ($item.find('.remove-option').length === 0) {
            $item.append('<span class="remove-option" style="margin-left: 5px; cursor: pointer; color: #999;">×</span>');
          }
        });
        
        $control.on('click', '.remove-option', function(e) {
          e.stopPropagation();
          e.preventDefault();
          
          var $item = $(this).closest('.item');
          var value = $item.attr('data-value');
          var selectize = $control.prev('select')[0].selectize;
          
          if (selectize) {
            selectize.removeItem(value);
            Shiny.setInputValue(selectizeId + '_removed', {
              value: value,
              timestamp: new Date().getTime()
            });
          }
        });
      }
    });
  }
  
  setTimeout(initializeSelectizeRemoval, 1000);
  
  $(document).on('shiny:inputchanged', function(event) {
    setTimeout(initializeSelectizeRemoval, 200);
  });
  
  $(document).on('shown.bs.tab', function(e) {
    setTimeout(initializeSelectizeRemoval, 200);
  });
});

$(document).ready(function() {
  Shiny.addCustomMessageHandler("updateSelectInput", function(message) {
    var $select = $("#" + message.id);
    $select.empty();
    $.each(message.choices, function(value, label) {
      $select.append($("<option>").attr("value", value).text(label));
    });
    $select.val(message.selected);
    $select.trigger("change");
    setTimeout(initializeSelectizeRemoval, 200);
  });
});
