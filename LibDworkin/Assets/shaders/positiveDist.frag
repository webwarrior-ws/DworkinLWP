precision mediump float;

varying vec2 toClosestPoint;

void main() {
	float d = length(toClosestPoint);
	float cosAlpha = dot(vec2(1.0, 0.0), toClosestPoint) / d;
	gl_FragColor = vec4(d, clamp(0.0, 1.0, cosAlpha / 2.0 + 0.5), d-1.0, 1.0);
}
