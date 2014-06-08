var ast = glsl.why( document.getElementById('per-fragment-lighting-fs').innerText, 0 );
var walked = glsl.generateShader( ast );
var output = '';

if( glsl.errors && glsl.errors.length ) {
    throw glsl.errors[0];
} else {
    for( var x = 0; x < ast.translation_unit.length; x++ ) {
        output += ast.translation_unit[ x ].toString();
    }
    console.log( output );
}
