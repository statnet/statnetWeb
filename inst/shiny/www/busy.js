
//setInterval will check if shiny is busy every 100ms
//if shiny is busy, setTimeout will display div.busy after 1 second (1000ms)

setInterval(function(){
  if ($('html').attr('class')=='shiny-busy') {
    setTimeout(function() {
      if ($('html').attr('class')=='shiny-busy') {
        $('div.busy').show();
      }
    }, 1000);
  } else {
    $('div.busy').hide();
  }
}, 100);





Shiny.addCustomMessageHandler("errstate",
  function(message){
    if (message===0) {
      $('div.error').show();
    } else {
      $('div.error').hide();
    }
  }
);
