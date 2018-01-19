// pull in desired CSS/SASS files
require( './styles/main.scss' );
var $ = jQuery = require( '../../node_modules/jquery/dist/jquery.js' );           // <--- remove if jQuery not needed
require( '../../node_modules/bootstrap-sass/assets/javascripts/bootstrap.js' );   // <--- remove if Bootstrap's JS not needed 

/**
 * Taken from https://stackoverflow.com/questions/901115/how-can-i-get-query-string-values-in-javascript
 */
function getQueryParamByName(name, url) {
    if (!url) url = window.location.href;
    name = name.replace(/[\[\]]/g, "\\$&");
    var regex = new RegExp("[?&]" + name + "(=([^&#]*)|&|#|$)"),
        results = regex.exec(url);
    if (!results) return "NotDefined";
    if (!results[2]) return '';
    return decodeURIComponent(results[2].replace(/\+/g, " "));
}

// inject bundled Elm app into div#main
var Elm = require( '../elm/FS' );
var app = Elm.FS.embed( document.getElementById( 'main' )
    , { operatingSystem: ""
      , url: ""
      , screenSize: {width: 0, height: 0}
      , applicationName: getQueryParamByName("name") 
    }
);
