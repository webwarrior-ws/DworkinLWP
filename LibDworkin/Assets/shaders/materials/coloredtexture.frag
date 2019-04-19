precision mediump float;

varying vec2 pos;
varying vec2 pScaled;

uniform vec4 color;
uniform sampler2D shadeMap;
uniform float flatShade;
uniform float contrast;

float getShade(sampler2D shadeTexture, vec2 pos) {
	vec3 texel = texture2D(shadeMap, pos).rgb;
	float v = max(texel.r, max(texel.g, texel.b));
	return (v - flatShade) * contrast;
}

vec3 applyShading(vec3 c, float shade) {
    return shade <= 0.0 ? vec3(1.0+shade) * c : c + vec3(shade);
}

void main() {
	float shade = getShade(shadeMap, pScaled);
	vec3 c = applyShading(color.rgb, shade);
	gl_FragColor = vec4(c, 1.0);
}

