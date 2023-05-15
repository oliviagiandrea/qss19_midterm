$(document).ready(function() {
  $('#group1').show();
  $('#group2').hide();

  $('#btn-group1').click(function() {
    $('#group1').show();
    $('#group2').hide();
    $('img').removeClass("show");
    $('img').addClass("hide");
    $('img.work').removeClass("hide");
    $('img.work').addClass("show");
  })

  $('#btn-group2').click(function() {
    $('#group2').show();
    $('#group1').hide();
    $('img').removeClass("show");
    $('img').addClass("hide");
    $('img.sex').removeClass("hide");
    $('img.sex').addClass("show");
  })

  $(".btn-graph").click(function() {
    $('img').removeClass("show");
    $('img').addClass("hide");
    var className = $(this).attr("class").split(" ").filter(function(c) {
      return c !== "btn-graph";
    });
    $("img." + className).removeClass("hide");
    $("img." + className).addClass("show");
  });
});


