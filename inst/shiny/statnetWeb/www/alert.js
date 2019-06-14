$(document).ready(function(){
  $(".helper-btn").click(function(){
    $(".helper-box").toggle(500);
  });

  $("#aboutButton").click(function(){
    $("#aboutbox").show();
    $("#citebox").hide();
    $("#aboutButton").toggleClass("active", true);
    $("#citeButton").toggleClass("active", false);
  });

  $("#citeButton").click(function(){
    $("#citebox").show();
    $("#aboutbox").hide();
    $("#citeButton").toggleClass("active", true);
    $("#aboutButton").toggleClass("active", false);
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

  $("#symmdir").click(function(){
    $("#symmdir").toggleClass("active", true);
    $("#symmundir").toggleClass("active", false);
  });

  $("#symmundir").click(function(){
    $("#symmdir").toggleClass("active", false);
    $("#symmundir").toggleClass("active", true);
  });

  $("#countButton_dd").click(function(){
    $("#countButton_dd").toggleClass("active", true);
    $("#percButton_dd").toggleClass("active", false);
  });

  $("#percButton_dd").click(function(){
    $("#countButton_dd").toggleClass("active", false);
    $("#percButton_dd").toggleClass("active", true);
  });

  $("#countButton_gd").click(function(){
    $("#countButton_gd").toggleClass("active", true);
    $("#percButton_gd").toggleClass("active", false);
  });

  $("#percButton_gd").click(function(){
    $("#countButton_gd").toggleClass("active", false);
    $("#percButton_gd").toggleClass("active", true);
  });

  $("#matchingButton").click(function(){
    $("#matchingButton").toggleClass("active", true);
    $("#allButton").toggleClass("active", false);
  });

  $("#allButton").click(function(){
    $("#matchingButton").toggleClass("active", false);
    $("#allButton").toggleClass("active", true);
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

  $("#filetypehelper7").click(function(){
    $("#filetypebox7").toggle(500);
  });

  $("#filetypehelper8").click(function(){
    $("#filetypebox8").toggle(500);
  });

  $("#filetypehelper9").click(function(){
    $("#filetypebox9").toggle(500);
  });

  $("#filetypehelper10").click(function(){
    $("#filetypebox10").toggle(500);
  });

  $("#closewarning1").click(function(){
    $("#colorwarning1").hide();
    $("#closewarning1").hide();
  });

  //$("#samplenet").change(function(){
  //  $("#colorwarning1").show();
  //  $("#closewarning1").show();
  //});

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

  $("#cugtitle").click(function(){
    $("#cugbox").toggle(1000);
    $("i",this).toggleClass("fa-angle-double-left fa-angle-double-down");
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

  $("#termexpand").click(function(){
    $("i",this).toggleClass("fa-angle-double-down fa-angle-double-up");
    if($("#termdocbox").height()<100){
      $("#termdocbox").css({
      "max-height":"250px"
      });
    } else {
      $("#termdocbox").css({
      "max-height":"55px"
      });
    }

  });

  $("#termdocbox").click(function(){
    $("#termexpand i").toggleClass("fa-angle-double-down fa-angle-double-up");
    if($("#termdocbox").height()<100){
      $("#termdocbox").css({
      "max-height":"250px"
      });
    } else {
      $("#termdocbox").css({
      "max-height":"55px"
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

  $("#controldefault").click(function(){
    $("#mcmcopt1").toggleClass("gray");
  })
    $("#simcontroldefault").click(function(){
    $("#mcmcopt2").toggleClass("gray");
  })

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