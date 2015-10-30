$(document).ready(function(){

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

  $("#symmdir").click(function(){
    $("#symmdir").toggleClass("active", true);
    $("#symmundir").toggleClass("active", false);
  });

  $("#symmundir").click(function(){
    $("#symmdir").toggleClass("active", false);
    $("#symmundir").toggleClass("active", true);
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

});
