precision mediump float;

attribute vec2 pos;
attribute vec4 inSegment;

uniform vec2 screenDimensions;
uniform vec2 scale;

varying vec2 closestPoint;
varying vec2 p;

float suqaredDistance(vec2 a, vec2 b) {
	vec2 diff = a - b;
	return dot(diff, diff);
}

void main() {
	// map coordinates to [(-1, -1); (1, 1)] screen space
	vec2 newPos = (pos * scale - 0.5) * 2.0;
	// invert y axis
	newPos.y = -newPos.y;
	gl_Position = vec4(newPos, 0.0, 1.0);
	p = pos;
	if(inSegment.xy == inSegment.zw || suqaredDistance(pos, inSegment.xy) < suqaredDistance(pos, inSegment.zw)) {
		closestPoint = inSegment.xy;
	}
	else {
		closestPoint = inSegment.zw;
	}
}
