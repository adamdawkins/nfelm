// pull in desired CSS/SASS files
require( './styles/main.scss' );

// inject bundled Elm app into div#main
var Elm = require( '../elm/Game' );
Elm.Football.embed( document.getElementById( 'main' ) );
