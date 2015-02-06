$(document).ready(function(){
  $(".helper-btn").click(function(){
    $(".helper-box").toggle(500);
  });

  $("#swciteButton").click(function(){
    $("#swcitation").toggle(500);
  });

  $("#sciteButton").click(function(){
    $("#scitation").toggle(500);
  });
  
  $("#linktitle1").click(function(){
    $("#linkbox1").toggle(200);
    $("i",this).toggleClass("fa-angle-double-left fa-angle-double-down");
  });
  
  $("#linktitle2").click(function(){
    $("#linkbox2").toggle(200);
    $("i",this).toggleClass("fa-angle-double-left fa-angle-double-down");
  });
  
  $("#linktitle3").click(function(){
    $("#linkbox3").toggle(200);
    $("i",this).toggleClass("fa-angle-double-left fa-angle-double-down");
  });
  
  $("#linktitle4").click(function(){
    $("#linkbox4").toggle(200);
    $("i",this).toggleClass("fa-angle-double-left fa-angle-double-down");
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
  
  $("#filetypehelper4").click(function(){
    $("#filetypebox4").toggle(500);
  });
  
  $("#filetypehelper5").click(function(){
    $("#filetypebox5").toggle(500);
  });
  
  $("#filetypehelper6").click(function(){
    $("#filetypebox6").toggle(500);
  });
    
  $("#ddhelper").click(function(){
    $("#ddhelperbox").toggle(500);
  });
      
  $("#gdhelper").click(function(){
    $("#gdhelperbox").toggle(500);
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
  
  $("#infhelper_gd").click(function(){
    $("#infhelperbox_gd").toggle(500);
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
  
  $(".chromewarning").click(function(){
    $(".chromewarningbox").toggle(500);
  });
  
  $("#termexpand").click(function(){
    $("i",this).toggleClass("fa-expand fa-compress");
    if($("#termdocbox").height()<100){
      $("#termdocbox").css({
      "max-height":"250px"
      });
    } else {
      $("#termdocbox").css({
      "max-height":"65px"
      });
    }
    
  });
  
  $("#terms").keyup(function(event){
    if(event.which == 13){
        $("#addtermButton").click();
    }
  });
  
  $("#mcmchelper").click(function(){
    $("#mcmchelpbox").toggle(500);
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