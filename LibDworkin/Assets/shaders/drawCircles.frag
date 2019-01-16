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
	gl_FragColor = vec4(clamp(d/h, 0.0, 1.0), 1.0, 0.0, 1.0);
}