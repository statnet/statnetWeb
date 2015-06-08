setInterval(function(){
  if ($('html').attr('class')=='shiny-busy') {
    setTimeout(function() {
      if ($('html').attr('class')=='shiny-busy') {
        $('div.busy').show()
      }
    }, 1000)
  } else {
    $('div.busy').hide()
  }
}, 100)


//setInterval will check if shiny is busy every 100ms
//if shiny is busy, setTimeout will display div.busy after 1 second (1000ms)