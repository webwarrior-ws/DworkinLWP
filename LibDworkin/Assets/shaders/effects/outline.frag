precision mediump float;

varying vec2 pos;

uniform vec2 screenDimensions;
uniform sampler2D sdf; 
uniform sampler2D foreground; 
uniform sampler2D background; 

const float threshold = 0.05;

uniform float outlineWidth;

float outline(vec2 rawPixel, float width) {
	float d = rawPixel.r > 0.0 ? rawPixel.r : rawPixel.g;
	float c = (d + 1.0 - width) / 2.0;
	return smoothstep(0.5-threshold*0.5, 0.5+threshold*0.5, c);
}

void main() {
	vec2 rawPixel = texture2D(sdf, pos).xy;
	vec3 fgColor = texture2D(foreground, pos).rgb;
	vec3 bgColor = texture2D(background, pos).rgb;

	float o = outline(rawPixel, outlineWidth);
	gl_FragColor = vec4(mix(fgColor, bgColor, o), 1.0);
}
