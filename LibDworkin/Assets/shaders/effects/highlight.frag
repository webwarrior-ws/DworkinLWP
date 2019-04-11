precision mediump float;

varying vec2 pos;
varying vec3 toLight;

uniform sampler2D sdf;
uniform sampler2D background;
uniform sampler2D foreground;

uniform float lightIntensity; // !slider[0.0, 0.5, 1.0]

const float threshold = 0.05;

float mask(vec2 rawPixel) {
	float d = rawPixel.r > 0.0 ? rawPixel.r : -rawPixel.g;
    return smoothstep(-threshold, threshold, d);
}

float highlight() {
	float l = length(toLight);
	//vec3 normal = vec3(0.0, 0.0, 1.0);
	float z = toLight.z / l; //dot(toLight / l, normal)
	return clamp(z, 0.0, 1.0) / l;
}
/*
float blendOverlay(float base, float blend) {
	return base<0.5?(2.0*base*blend):(1.0-2.0*(1.0-base)*(1.0-blend));
}

vec3 blendOverlay(vec3 base, vec3 blend) {
	return vec3(blendOverlay(base.r,blend.r),blendOverlay(base.g,blend.g),blendOverlay(base.b,blend.b));
}

vec3 blendOverlay(vec3 base, vec3 blend, float opacity) {
	return (blendOverlay(base, blend) * opacity + base * (1.0 - opacity));
}
*/
vec3 blendOverlayFast(vec3 base, vec3 blend) {
	bvec3 ltHalf = lessThan(base, vec3(0.5));
	return (2.0*base*blend) * vec3(ltHalf) + (1.0-2.0*(1.0-base)*(1.0-blend)) * vec3(not(ltHalf));
}

vec3 blendOverlayFast(vec3 base, vec3 blend, float opacity) {
	return (blendOverlayFast(base, blend) * opacity + base * (1.0 - opacity));
}


void main() {
	vec3 rawPixel = texture2D(sdf, pos).rgb;

	float m = mask(rawPixel.rg);
	
	vec3 lighting = vec3(highlight());

	vec3 bgColor = texture2D(background, pos).rgb;
	vec3 fgColor = texture2D(foreground, pos).rgb;
	vec3 col = blendOverlayFast(mix(fgColor, bgColor, m), lighting, lightIntensity);
   
	gl_FragColor = vec4(col, 1.0);
}

