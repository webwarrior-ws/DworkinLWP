precision mediump float;

uniform vec2 screenDimensions;

attribute vec2 texCoords;

varying vec2 pos;
varying vec2 px1;
varying vec2 px2;
varying vec2 py1;
varying vec2 py2;

void main() {
	gl_Position = vec4(texCoords, 0.0, 1.0);
	pos = texCoords * 0.5 + 0.5;
	vec2 ps = vec2(1.0) / screenDimensions;
    px1 = pos - vec2(ps.x, 0.0);
    px2 = pos + vec2(ps.x, 0.0);
    py1 = pos - vec2(0.0, ps.y);
    py2 = pos + vec2(0.0, ps.y);
}

