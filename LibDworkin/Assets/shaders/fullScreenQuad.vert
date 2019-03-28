precision mediump float;

attribute vec2 texCoords;

varying vec2 pos;

void main() {
	gl_Position = vec4(texCoords, 0.0, 1.0);
	pos = texCoords * 0.5 + 0.5;
}
