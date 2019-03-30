precision mediump float;

varying vec2 closestPoint;
varying vec2 p;

uniform float h;

void main() {
	float d = distance(p, closestPoint);
	float t = d/h;
	gl_FragColor = vec4(t, 1.0, t-1.0, 1.0);
}
