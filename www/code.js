$( document ).ready(function() {
  $( ".navbar .container-fluid" ).append( '<img title="Idaho Fish and Game" src="idfglogo.png" align="right" height="80px" style = "right: 10px; top: 1px; padding-top:30px"/>' ).wrap('<a href="http://idfg.idaho.gov"/>');
    $( ".navbar .container-fluid .navbar-header" ).append( '<img title="Return to Wildlife Shiny Applications" src="r-shiny-logo.png" align="left" height="50px" style="position:absolute; top:50px; left:85px;")/>' ).wrap('<a href = "http://ifwisshiny.idfg.state.id.us:3838/EAR/IDFG_Wildlife_Shiny_Apps/" />');
});