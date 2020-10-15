precision mediump float;
precision mediump sampler2D;

uniform sampler2D sdf;
uniform sampler2D background;
uniform sampler2D foreground;
uniform sampler2D reflectionMap;

uniform vec3 toLight;
uniform float contrast; 
uniform float maxDistance;
uniform int bevelProfile; // !combobox[0, "Linear", "Hyperbolic", "Parabola1", "Parabola2", "Exp"]
uniform int mode; // !combobox[1, "Inner", "Outer", "Ridge", "Trench"]

uniform float fgRoughness;
uniform float bgRoughness;
uniform vec4 fgSpecular;
uniform vec4 bgSpecular;

varying vec2 pos;
varying vec2 px1;
varying vec2 px2;
varying vec2 py1;
varying vec2 py2;

const float steepness = 2.4;
const float specularContrast = 0.25;
const float threshold = 0.05;

float mask(vec2 rawPixel) {
	float d = rawPixel.r > 0.0 ? rawPixel.r : -rawPixel.g;
    return smoothstep(-threshold, threshold, d);
}

float squared(float x) {
    return x*x;
}

float parabola(float x)
{
    return squared(4.0*x*(1.0-x));
}

float dInner(vec2 t) {
	return t.r == 0.0 ? t.g : -t.r;
}

float dist(vec2 tx, bool isInner) {
    return clamp((isInner ? dInner(tx) : tx.r), -maxDistance, maxDistance);
}

vec2 gradient(vec4 t) {
    return vec2(t.x-t.y, t.z-t.w);
}

vec3 normalAt(float d, vec2 grad) {
	float z = 1.0/steepness;   
	
	if(bevelProfile == 1)
		z = mix(z*0.5, z*1.5, 1.0/(steepness*d+0.001));
	else if(bevelProfile == 2)
		z = mix(z*0.75, z*3.5, squared(parabola(0.5*d)));
	else if(bevelProfile == 3)
		z = mix(z*0.75, z*3.5, 1.0-parabola(d));
	else if(bevelProfile == 4)
		z = mix(z*0.5, z*3.0, exp(-d)-0.367);
	
    return normalize(vec3(grad, z*sqrt(1.0-dot(grad, grad))));
}

float shade_normal(vec3 toLight, vec3 normal) {
    float flatShade = toLight.z; //dot(toLight, vec3(0.0, 0.0, 1.0));
    float shade = clamp(dot(toLight, normal), 0.0, 1.0);
    return shade - flatShade;
}

vec3 applyShading(vec3 c, float shade) {
    return shade <= 0.0 ? vec3(1.0+shade) * c : c + vec3(shade);
}

float specular_normal(vec3 n) {
	vec3 e = vec3(0.0, 0.0, 1.7);
	vec3 r = reflect(e, n);
 	vec3 sqrtTerm = vec3(r.xy, r.z + 1.0);
 	float m = 2.0 * sqrt(dot(sqrtTerm, sqrtTerm));
	vec2 vN = r.xy / m + 0.5;
	return texture2D(reflectionMap, vN).r;
}

vec3 getNormal(vec3 rawPixel, float mask) {
	vec2 tx1 = texture2D(sdf, px1).rg;
    vec2 tx2 = texture2D(sdf, px2).rg;
    vec2 ty1 = texture2D(sdf, py1).rg;
    vec2 ty2 = texture2D(sdf, py2).rg;
	
	vec3 inner = vec3(0.0, 0.0, 1.0), outer = vec3(0.0, 0.0, 1.0);
	// outer
	if(mode != 0 && mask > 0.0) {
		vec4 tOuter = clamp(vec4(tx1.r, tx2.r, ty1.r, ty2.r), -maxDistance, maxDistance);
		vec2 grad = gradient(tOuter);
		float d = dist(rawPixel.rg, false) / maxDistance;
		outer = normalAt(d, grad);
		if (mode != 3) { outer.xy = -outer.xy; }
	}
	// inner
	if(mode != 1 && mask < 1.0) {
		vec4 tInner = clamp(vec4(dInner(tx1), dInner(tx2), dInner(ty1), dInner(ty2)), -maxDistance, maxDistance);
		vec2 grad = gradient(tInner);
		float d = dist(rawPixel.rg, true) / maxDistance;
		inner = normalAt(d, grad);
		if (mode == 2) { inner.xy = -inner.xy; }
	}
	return mix(inner, outer, mask);
}


void main() {
	vec3 rawPixel = texture2D(sdf, pos).rgb;

	float m = mask(rawPixel.rg);
	vec3 fgColor = texture2D(foreground, pos).rgb;
	vec3 bgColor = texture2D(background, pos).rgb;
	
	vec3 c = mix(fgColor, bgColor, m);
	float d = rawPixel.r > 0.0 ? rawPixel.r : rawPixel.g;
	if(d > maxDistance) { 
		gl_FragColor = vec4(c, 1.0); 
		return; 
	}
	float m2 = smoothstep(-threshold, threshold, d-maxDistance);
	
	vec3 normal = getNormal(rawPixel, m);
	
	float roughness = mix(fgRoughness, bgRoughness, m);
	
	float shade = shade_normal(toLight, normal);
	vec3 colShaded = applyShading(c, shade * contrast);
	
	float specular = specular_normal(normal);
	float ambientSpecular = specular_normal(vec3(0.0, 0.0, 1.0));
	vec3 finalSpecular = vec3(specular - ambientSpecular) * specularContrast * contrast;
	vec3 specularColor = mix(fgSpecular, bgSpecular, m).rgb;
	
	vec3 col = mix(finalSpecular * specularColor + c, colShaded, roughness);
    col = mix(col, c, m2);

	gl_FragColor = vec4(col, 1.0);
}