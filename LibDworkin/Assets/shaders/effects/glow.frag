
precision mediump float;

varying vec2 pos;

uniform sampler2D sdf;
uniform sampler2D background;
uniform sampler2D foreground;

uniform vec4 glowColor; // !color[1.0, 1.0, 1.0]
uniform float glowStrength; // !slider[0.05, 1.0, 1.0]
uniform float bloomPower;
uniform int insideType;

const float threshold = 0.05;

float mask(vec2 rawPixel) {
	float d = rawPixel.r > 0.0 ? rawPixel.r : -rawPixel.g;
    return smoothstep(-threshold, threshold, d);
}

float glow(float d) {
	return 1.0-clamp(abs(d)/glowStrength, 0.0, 1.0);
}

float lightness(vec3 c) {
	float Xmax = max(c.r, max(c.g, c.b));
	float Xmin = min(c.r, min(c.g, c.b));
	return (Xmax + Xmin) / 2.0;
}


void main() {
	vec3 rawPixel = texture2D(sdf, pos).rgb;
	vec3 fgColor = texture2D(foreground, pos).rgb;
	vec3 bgColor = texture2D(background, pos).rgb;
	float m = mask(rawPixel.rg);
    
	float d = (rawPixel.r > 0.0 || insideType == 1) ? rawPixel.r : -rawPixel.g;
	vec3 fill = insideType == 2 ? fgColor : bgColor;
	
	float t = glow(d);
	vec3 col = mix(mix(fill, bgColor, m), glowColor.rgb, t);
	float brightness = 1.0 - lightness(glowColor.rgb) * 0.25;
	col += pow(t, bloomPower) * brightness;
	
	gl_FragColor = vec4(col, 1.0);
}
