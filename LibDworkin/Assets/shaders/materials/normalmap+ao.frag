precision mediump float;

varying vec2 pos;
varying vec2 pScaled;
varying vec3 toLight;

uniform vec4 color;
uniform sampler2D normalMap;

uniform float aoStrength;
uniform sampler2D aoMap;

vec3 texNormal(sampler2D normalmap, vec2 p) {
	vec3 rawPixel = texture2D(normalmap, p).rgb;
	vec2 normXY = (rawPixel.rg - vec2(0.5)) * 2.0;
	return vec3(normXY, rawPixel.b);
}

vec3 applyShading(vec3 c, float shade) {
    return shade <= 0.0 ? vec3(1.0+shade) * c : c + vec3(shade);
}

vec3 applyBump(vec3 c, vec3 normal, vec3 toLight) {
    float shade = clamp(dot(toLight, normal), 0.0, 1.0);
    float flatShade = dot(toLight, vec3(0.0, 0.0, 1.0));
    return applyShading(c, shade - flatShade);
}

void main() {
	vec3 normal = texNormal(normalMap, pScaled);
	float ao = 1.0;
	if(aoStrength > 0.0) {
		ao = mix(1.0, texture2D(aoMap, pScaled).r, aoStrength);
	}
	vec3 c = applyBump(color.rgb * ao, normal, toLight);
	gl_FragColor = vec4(c, 1.0);
}

