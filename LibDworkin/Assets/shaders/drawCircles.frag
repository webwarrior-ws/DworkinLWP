precision mediump float;

varying vec2 p;
varying vec2 center;
varying float radius;
varying float innerRadius;

uniform float h;

void main() {
	float d = distance(p, center) - radius;
	if(innerRadius > 0.0) {
		d = abs(d) - (radius - innerRadius);
	};
	float t = d/h;
	gl_FragColor = vec4(t, 1.0, t-1.0, 1.0);
}