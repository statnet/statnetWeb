$(document).ready(function(){
  $(".helper-btn").click(function(){
    $(".helper-box").toggle(500);
  });
  
  $("#filetypehelper1").click(function(){
    $("#filetypebox1").toggle(500);
  });
  
  $("#filetypehelper2").click(function(){
    $("#filetypebox2").toggle(500);
  });
    
  $("#cughelper_dd").click(function(){
    $("#cughelperbox_dd").toggle(500);
  });
      
  $("#brghelper_dd").click(function(){
    $("#brghelperbox_dd").toggle(500);
  });
  
  $(".mixmxtitle").click(function(){
    $(".mixmxbox").toggle(1000);
  });
  
  $(".nodeleveltitle").click(function(){
    $(".nodelevelbox").toggle(1000);
  });

  $(".graphleveltitle").click(function(){
    $(".graphlevelbox").toggle(1000);
  });

  $("#termdocButton").click(function(){
    $(".docpopup").toggle(1000);
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