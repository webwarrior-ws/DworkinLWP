precision mediump float;

attribute vec2 pos;
attribute vec4 data;

uniform vec2 scale;

varying vec2 p;
varying vec2 center;
varying float radius;
varying float innerRadius;

void main() {
	// map coordinates to [(-1, -1); (1, 1)] screen space
	vec2 newPos = (pos * scale - 0.5) * 2.0;
	// invert y axis
	newPos.y = -newPos.y;
	gl_Position = vec4(newPos, 0.0, 1.0);
	p = pos;
	center = data.xy;
	radius = data.z;
	innerRadius = data.w;
}
