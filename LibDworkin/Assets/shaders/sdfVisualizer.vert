precision mediump float;

in vec2 texCoords;

varying out vec2 p;

void main() {
	gl_Position = vec4(texCoords, 0.0, 1.0);
	p = texCoords * 0.5 + 0.5;
}
