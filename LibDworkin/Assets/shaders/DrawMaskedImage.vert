precision mediump float;

attribute vec2 texCoords;

void main() {
	gl_Position = vec4(texCoords.xy, 0.0, 1.0);
}