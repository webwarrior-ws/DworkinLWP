precision mediump float;

varying vec2 toClosestPoint;

void main() {
	float d = length(toClosestPoint);
	gl_FragColor = vec4(d, 1.0, d-1.0, 1.0);
}
