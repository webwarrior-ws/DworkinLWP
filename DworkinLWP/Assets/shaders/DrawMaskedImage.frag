precision mediump float;

uniform sampler2D mask;
uniform vec2 screenDimensions;

void main() {
   vec2 p = gl_FragCoord.xy / screenDimensions;
   gl_FragColor = texture2D(mask, p);
}