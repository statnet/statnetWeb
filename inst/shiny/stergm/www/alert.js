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

});
