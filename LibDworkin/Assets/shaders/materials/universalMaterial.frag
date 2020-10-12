precision mediump float;

uniform vec3 toLight;
uniform vec4 color;
uniform vec4 specularColor;

uniform sampler2D reflectionMap;
uniform sampler2D aoMap;
uniform sampler2D normalMap;
uniform sampler2D roughnessMap;
uniform float roughnessValue;

varying vec2 pos;
varying vec2 pScaled;

float shade_normal(vec3 toLight, vec3 normal) {
    float flatShade = toLight.z; //dot(toLight, vec3(0.0, 0.0, 1.0));
    float shade = clamp(dot(toLight, normal), 0.0, 1.0);
    return shade - flatShade;
}

vec3 applyShading(vec3 c, float shade) {
    return shade <= 0.0 ? vec3(1.0+shade) * c : c + vec3(shade);
}

float specular_normal(vec3 n, vec2 pos) {
	vec3 e = vec3((pos-0.5)*2.0, 2.5);
	vec3 r = reflect(e, n);
 	vec3 sqrtTerm = vec3(r.x, r.y, r.z+1.0);
 	float m = 2.0 * sqrt(dot(sqrtTerm, sqrtTerm));
	vec2 vN = r.xy / m + 0.5;
	return texture2D(reflectionMap, vN).r;
}

void main() {
	float roughness = roughnessValue; 
	if(roughnessValue < 0.0) { roughness = texture2D(roughnessMap, pScaled).r; }
	vec3 n = texture2D(normalMap, pScaled).rgb;
	vec3 c = texture2D(aoMap, pScaled).r * color.rgb;
	vec3 normal = (n - vec3(0.5, 0.5, 0.0)) * vec3(2.0, 2.0, 1.0);
	float shade = shade_normal(toLight, normal);
	vec3 colShaded = applyShading(c, shade);
	float specular = (specular_normal(normal, pos) + 1.0) / 2.0;
  	gl_FragColor = vec4(mix(vec3(specular) * specularColor.rgb, colShaded, roughness), 1.0);
}