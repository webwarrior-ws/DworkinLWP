precision mediump float;

varying float alpha;
varying float z;

uniform float h;

void main() {
	float d = z == 0.0 ? 0.0 : z / cos(alpha * h / z);
	gl_FragColor = vec4(clamp(d/h, 0.0, 1.0), 1.0, 0.0, 1.0);
}
