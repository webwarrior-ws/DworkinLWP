precision mediump float;

attribute vec2 texCoords;

uniform vec2 screenDimensions;
uniform float scaleFactor;
uniform vec2 lightPos; 
uniform float lightHeight; 

varying vec2 pos;
varying vec2 pScaled;
varying vec3 toLight;

void main() {
	gl_Position = vec4(texCoords, 0.0, 1.0);
	pos = texCoords * 0.5 + 0.5;
	vec2 scale = vec2(scaleFactor) / screenDimensions;
	pScaled = pos * scale * screenDimensions;
	pScaled.y *= screenDimensions.y / screenDimensions.x;
	toLight = normalize(vec3(lightPos, lightHeight));
}
