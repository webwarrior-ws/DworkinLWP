precision mediump float;

attribute vec3 pos;
attribute float inAlpha;

uniform vec2 scale;
uniform float h;

varying float alpha;
varying float z;

void main() {
	// map coordinates to [(-1, -1); (1, 1)] screen space
	vec2 newPos = (pos.xy * scale - 0.5) * 2.0;
	// invert y axis
	newPos.y = -newPos.y;
	gl_Position = vec4(newPos, pos.z / h, 1.0);
	z = pos.z;
	alpha = inAlpha;
}
