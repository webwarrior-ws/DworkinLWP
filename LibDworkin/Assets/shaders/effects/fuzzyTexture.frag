precision mediump float;

varying vec2 pos;
varying vec2 pScaled;

uniform sampler2D sdf;
uniform sampler2D background;
uniform sampler2D foreground;
uniform sampler2D noise;

uniform float falloff;
uniform float absorbThreshold;
uniform float fill;
uniform float interiorFade;


float fuzzy(float d) {
  float distort = d + (texture2D(noise, pScaled).r * 2.0 - 1.0) * falloff;
  float threshold = absorbThreshold + falloff*falloff/2.0;
  return smoothstep(-threshold, threshold, distort);
}

void main() {
	vec3 rawPixel = texture2D(sdf, pos).rgb;
	
	float innerDRange = fill * (falloff + absorbThreshold);
	float d = rawPixel.r > 0.0 ? rawPixel.r : max(-innerDRange, -rawPixel.g);
	d += (rawPixel.r > 0.0) ? 0.0 : (rawPixel.g * interiorFade);
	
	float m = fuzzy(d);
	
	vec3 fgColor = texture2D(foreground, pos).rgb;
	vec3 bgColor = texture2D(background, pos).rgb;
	vec3 col = mix(mix(fgColor, bgColor, m), bgColor, m);   

	gl_FragColor = vec4(col, 1.0);
}