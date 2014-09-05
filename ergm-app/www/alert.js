$(document).ready(function(){
  $(".helper-btn").click(function(){
    $(".helper-box").toggle(500);
  });
});

$(document).ready(function(){
  $(".mixmxtitle").click(function(){
    $(".mixmxbox").toggle(1000);
  });
});

$(document).ready(function(){
  $(".nodeleveltitle").click(function(){
    $(".nodelevelbox").toggle(1000);
  });
});

$(document).ready(function(){
  $(".graphleveltitle").click(function(){
    $(".graphlevelbox").toggle(1000);
  });
});
/*
window.onload = function(){ 
  //Get submit button
  var submitbutton = document.getElementById("tfq");
	//Add listener to submit button
	if(submitbutton.addEventListener){
		submitbutton.addEventListener("click", function() {
			if (submitbutton.value == 'Search for a term'){
				submitbutton.value = '';
			}
		});
	}
}*/