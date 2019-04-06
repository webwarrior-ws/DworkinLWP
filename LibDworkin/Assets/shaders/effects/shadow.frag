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

float shadow(vec2 p, sampler2D sdfTex, bool isInner) {
    vec4 rawPixel = texture2D(sdfTex, clamp(p, 0.001, 0.999));
	float d = rawPixel.r > 0.0 ? rawPixel.r : -rawPixel.g;
	if(isInner) d = -d;
    d = clamp(d / shadowSize, 0.0, 1.0);
    d = pow(d, shadowSize/shadowSoftness);
    return clamp(d, 1.0 - shadowStrength, 1.0);
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
	//gl_FragColor = vec4(vec3(rawPixel.r > 0.0 ? 0.0 : rawPixel.g) * 5.0, 1.0);
	//gl_FragColor = vec4(rawPixel.r > 0.0 ? vec3(rawPixel.r * 30.0, 0.0, 0.0) : vec3(0.0, 0.0, rawPixel.g * 8.0), 1.0);
}