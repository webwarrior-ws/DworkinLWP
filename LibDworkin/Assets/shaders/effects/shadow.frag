precision highp float;

varying vec2 pos;

uniform vec2 screenDimensions;
uniform sampler2D sdf; 
uniform sampler2D foreground; 
uniform sampler2D background; 

const float threshold = 0.05;

uniform vec2 shadowDir; 
uniform float shadowOffset; 
uniform float shadowSize; 
uniform float shadowSoftness; 
uniform float shadowStrength; 
uniform bool inner; 

float mask(vec2 rawPixel) {
    float d = rawPixel.r > 0.0 ? rawPixel.r : -rawPixel.g;
    return smoothstep(-threshold, threshold, d);
}

// http://c0de517e.blogspot.com/2014/04/smoothen-your-functions.html
float sbias(float x, float a) {
	return x/((1.0/a - 2.0)*(1.0 - x) + 1.0);
}

float sgain(float x, float a) {
	return x < 0.5 ? 
		sbias(2.0*x, a) * 0.5 : 
		1.0 - 0.5*sbias(2.0 - 2.0*x, a);
}

float shadow(vec2 p, sampler2D sdfTex, bool isInner) {
    vec4 rawPixel = texture2D(sdfTex, clamp(p, 0.0001, 0.9999));
	float d = rawPixel.r > 0.0 ? rawPixel.r : -rawPixel.g;
	if(isInner) d = -d;
    float s = sgain(d/shadowSize, shadowSoftness*0.5);
    return clamp(s, 1.0 - shadowStrength, 1.0);
}

void main() {
	vec2 rawPixel = texture2D(sdf, pos).xy;
	vec3 fgColor = texture2D(foreground, pos).rgb;
	vec3 bgColor = texture2D(background, pos).rgb;
   
	float m = mask(rawPixel);
	vec2 offset = shadowDir * shadowOffset / screenDimensions;
	float s = shadow(pos - offset, sdf, inner);
	if(inner)
		fgColor *= s;
	else
		bgColor *= s;
   
	gl_FragColor = vec4(mix(fgColor, bgColor, m), 1.0);
}