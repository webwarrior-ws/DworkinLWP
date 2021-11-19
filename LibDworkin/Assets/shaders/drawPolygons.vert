precision highp float;

attribute vec2 pos;

uniform vec2 scale;

void main() {
	// map coordinates to [(-1, -1); (1, 1)] screen space
	vec2 newPos = (pos * scale - 0.5) * 2.0;
	// invert y axis
	newPos.y = -newPos.y;
	gl_Position = vec4(newPos, 0.0, 1.0);
}
