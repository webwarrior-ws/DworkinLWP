precision mediump float;

attribute vec2 texCoords;

uniform vec2 screenDimensions;
uniform float scaleFactor;

varying vec2 pos;
varying vec2 pScaled;

void main() {
	gl_Position = vec4(texCoords, 0.0, 1.0);
	pos = texCoords * 0.5 + 0.5;
	vec2 scale = vec2(scaleFactor) / screenDimensions;
	pScaled = pos * scale * screenDimensions;
	float ratio = screenDimensions.x / screenDimensions.y;
	if(ratio < 1.0)
		pScaled.x *= ratio;
	else
		pScaled.y /= ratio;
}
