(function($) {
  var hostname = "http://bounty.0x80.ru"
  $(document).ready(function() {
    $('.save-key form').submit(function(e) {
      e.preventDefault();

      var log_window = $('.save-key .window');

      var key = $('.save-key input[name="key"]').val();
      var value = $('.save-key input[name="value"]').val();

      var data = {
        value: value
      }
     
      if (key && value) {
        var jsonData = JSON.stringify(data);
        var url = hostname + '/store/' + key;

        log(log_window, 'key: '+key);
        log(log_window, 'data: '+JSON.stringify(data));
        log(log_window, 'PUT request to: '+url);

        $.ajax({
          url: url,
          method: 'put',
          data: jsonData
        }).done(function(data) {
          log(log_window, 'Received: '+JSON.stringify(data));
        });
      }
    });
   
    $('.get-key form').submit(function(e) {
      e.preventDefault();

      var log_window = $('.get-key .window');

      var key = $('.get-key input[name="key"]').val();
      var default_val = $('.get-key input[name="value"]').val();

      if (key) {
        var url = hostname + '/store/' + key

        log(log_window, 'key: '+key);
        if (default_val) {
          log(log_window, 'default: '+default_val);
          url += '?default=' + default_val;
        }
        log(log_window, 'GET request to: '+url);

        $.ajax({
          url: url,
          method: 'get'
        }).done(function(data) {
          log(log_window, 'Received: '+JSON.stringify(data));
        });
      }
    });
  });

  var log = function(element, msg) {
    element.append('&gt; '+msg+'<br/>');
  }
})(jQuery);
