$(document).ready(function(){
  $(".helper-btn").click(function(){
    $(".helper-box").toggle(500);
  });
  
  $("#Robjhelp").click(function(){
    $("#Robjbox").toggle(500);
  });
  
  $("#filetypehelper1").click(function(){
    $("#filetypebox1").toggle(500);
  });
  
  $("#filetypehelper2").click(function(){
    $("#filetypebox2").toggle(500);
  });
  
  $("#filetypehelper3").click(function(){
    $("#filetypebox3").toggle(500);
  });
    
  $("#cughelper_dd").click(function(){
    $("#cughelperbox_dd").toggle(500);
  });
      
  $("#brghelper_dd").click(function(){
    $("#brghelperbox_dd").toggle(500);
  });
  
  $("#cughelper_gd").click(function(){
    $("#cughelperbox_gd").toggle(500);
  });
      
  $("#brghelper_gd").click(function(){
    $("#brghelperbox_gd").toggle(500);
  });
  
  $("#mixmxtitle").click(function(){
    $("#mixmxbox").toggle(1000);
    $("i",this).toggleClass("fa-angle-double-left fa-angle-double-down");
  });
  
  $("#nodeleveltitle").click(function(){
    $("#nodelevelbox").toggle(1000);
    $("i",this).toggleClass("fa-angle-double-left fa-angle-double-down");
  });

  $("#graphleveltitle").click(function(){
    $("#graphlevelbox").toggle(1000);
    $("i",this).toggleClass("fa-angle-double-left fa-angle-double-down");
  });

  $("#termdocButton").click(function(){
    $(".docpopup").toggle(500);
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