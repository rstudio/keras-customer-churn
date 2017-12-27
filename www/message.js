$(document).ready(function() {

    var box_id = '';
    
    $(".strategy_box").on("mouseover", function(){
        box_id = this.id;
        Shiny.onInputChange("strategy_box_hover", box_id);
    });
    
    $(".strategy_box").on("mouseleave", function(){
        box_id = 'none';
        Shiny.onInputChange("strategy_box_hover", box_id);
    });

});