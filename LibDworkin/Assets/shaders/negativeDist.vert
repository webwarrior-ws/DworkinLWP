precision mediump float;

attribute vec2 pos;

varying vec2 texCoords;

void main() {
	gl_Position = vec4(pos, 0.0, 1.0);
	texCoords = pos * 0.5 + 0.5;
}
