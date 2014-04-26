(function($) {
  var hostname = "http://psychedelizer.0x80.ru:3334"
  $(document).ready(function() {
    $('.save-key form').submit(function(e) {
      e.preventDefault();
      
      var key = $('input[name="key"]').val();
      var value = $('input[name="value"]').val();
      
      var data = {
        value: value
      }
      
      if (key && value) {
        $.ajax({
          url: hostname + '/store/' + key,
          method: 'put',
          data: JSON.stringify(data)
        }).done(function(data) {
          console.log(data);
        });
      }
    });
  });
})(jQuery);
