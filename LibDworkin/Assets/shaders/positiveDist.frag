precision mediump float;

varying vec2 closestPoint;
varying vec2 p;

uniform float h;

void main() {
	float d = distance(p, closestPoint);
	gl_FragColor = vec4(clamp(d/h, 0.0, 1.0), 1.0, 1.0, 1.0);
}
