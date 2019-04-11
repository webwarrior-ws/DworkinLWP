
precision mediump float;

varying vec2 pos;

uniform sampler2D sdf;
uniform sampler2D background;
uniform sampler2D foreground;

uniform vec4 glowColor; // !color[1.0, 1.0, 1.0]
uniform float glowStrength; // !slider[0.05, 1.0, 1.0]
uniform int glowType; // !combobox[0, "Outer", "Inner"]

const float threshold = 0.05;

float mask(vec2 rawPixel) {
	float d = rawPixel.r > 0.0 ? rawPixel.r : -rawPixel.g;
    return smoothstep(-threshold, threshold, d);
}

float glow(float d) {
	return 1.0-clamp(abs(d)/glowStrength, 0.0, 1.0);
}

vec3 glow(float d, vec3 color, vec3 glowColor) {
	return mix(color, glowColor, glow(d));
}


void main() {
	vec3 rawPixel = texture2D(sdf, pos).rgb;
	vec3 fgColor = texture2D(foreground, pos).rgb;
	vec3 bgColor = texture2D(background, pos).rgb;
	float m = mask(rawPixel.rg);
    
	float d = rawPixel.r > 0.0 ? rawPixel.r : -rawPixel.g;
	vec3 col;
	if(glowType == 0)
		col = mix(fgColor, glow(d, bgColor, glowColor.rgb), m);
	else
		col = mix(glow(-d, fgColor, glowColor.rgb), bgColor, m);
   
	gl_FragColor = vec4(col, 1.0);
}
