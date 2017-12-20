$(document).ready(function() {

    var n = '';
    
    $(".strategy_box").on("mouseover", function(){
        n = this.id;
        Shiny.onInputChange("strategy_box_hover", n);
    });
    
    $(".strategy_box").on("mouseleave", function(){
        n = 'none';
        Shiny.onInputChange("strategy_box_hover", n);
    });

});