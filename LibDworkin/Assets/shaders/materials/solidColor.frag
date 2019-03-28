precision mediump float;

varying vec2 pos;

uniform vec4 color;

void main() {
   gl_FragColor = vec4(color.rgb, 1.0);
}

