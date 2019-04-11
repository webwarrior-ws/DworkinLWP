precision mediump float;

attribute vec2 texCoords;

uniform vec2 lightPos; // !direction[0.0, 1.0]
uniform vec2 screenDimensions;

varying vec2 pos;
varying vec3 toLight;

void main() {
	gl_Position = vec4(texCoords, 0.0, 1.0);
	pos = texCoords * 0.5 + 0.5;
	float aspectRatio = screenDimensions.x / screenDimensions.y;
	vec2 correctedPos = ((pos - 0.5) * 2.0 * vec2(aspectRatio, 1.0)) / 2.0 + 0.5;
	toLight = vec3((lightPos + 1.0) * 0.5, 1.0) - vec3(correctedPos, 0.0);
}

