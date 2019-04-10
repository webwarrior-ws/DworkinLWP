precision mediump float;

varying vec2 pos;
varying vec2 px1;
varying vec2 px2;
varying vec2 py1;
varying vec2 py2;
varying vec3 toLight;

uniform sampler2D sdf;
uniform sampler2D background;
uniform sampler2D foreground;

uniform float contrast; // !slider[1.0, 5.0, 10.0]
uniform float maxDistance; // !slider[0.0, 1.0, 1.0]
uniform int bevelProfile; // !combobox[0, "Linear", "Hyperbolic", "Parabola1", "Parabola2", "Exp"]
uniform int mode; // !combobox[1, "Inner", "Outer", "Ridge", "Trench"]


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

float dist(vec2 tx, bool isInner) {
    return clamp((isInner ? tx.g : tx.r), 0.0, maxDistance);
}

vec2 gradient(vec2 tx1, vec2 tx2, vec2 ty1, vec2 ty2, bool isInner) {
	float x1 = dist(tx1, isInner);
    float x2 = dist(tx2, isInner);
    float y1 = dist(ty1, isInner);
    float y2 = dist(ty2, isInner);
    return vec2(x1-x2, y1-y2);
}

vec3 normalAt(float d, vec2 grad) {
	float z = 1.0/contrast;   
    if(bevelProfile == 1)
        z = mix(z*0.5, z*1.5, 1.0/(contrast*d+0.001));
    else if(bevelProfile == 2)
        z = mix(z*0.75, z*3.5, squared(parabola(0.5*d)));
    else if(bevelProfile == 3)
        z = mix(z*0.75, z*3.5, 1.0-parabola(d));
    else if(bevelProfile == 4)
        z = mix(z*0.5, z*3.0, exp(-d)-0.367);

    return normalize(vec3(grad, z*sqrt(1.0-dot(grad, grad))));
}

// TODO: mix normals instead of mixing shades.
float bevel(vec3 toLight, vec3 normal, bool isInner, bool isDown) {
    if(isInner == isDown) {
        normal = vec3(-normal.xy, normal.z);
    }
    float flatShade = dot(toLight, vec3(0.0, 0.0, 1.0));
    float shade = clamp(dot(toLight, normal), 0.0, 1.0);
    return shade - flatShade;
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
	vec3 rawPixel = texture2D(sdf, pos).rgb;

	float m = mask(rawPixel.rg);
	vec3 fgColor = texture2D(foreground, pos).rgb;
	vec3 bgColor = texture2D(background, pos).rgb;

	

	vec2 tx1 = texture2D(sdf, px1).rg;
    vec2 tx2 = texture2D(sdf, px2).rg;
    vec2 ty1 = texture2D(sdf, py1).rg;
    vec2 ty2 = texture2D(sdf, py2).rg;

	vec3 c = mix(fgColor, bgColor, m);
	float shade;
	if(mode >= 2) { // both
		// outer
		vec2 grad = gradient(tx1, tx2, ty1, ty2, false);
		float d = dist(rawPixel.rg, false) / maxDistance;
		float outer = bevel(toLight, normalAt(d, grad), false, mode==3);
		// inner
		grad = gradient(tx1, tx2, ty1, ty2, true);
		d = dist(rawPixel.rg, true) / maxDistance;
		float inner = bevel(toLight, normalAt(d, grad), true, mode==2);

		shade = mix(inner, outer, m);
	}
	else {
		bool isInner = mode!=1;
		float d = dist(rawPixel.rg, isInner) / maxDistance;
		vec2 grad = gradient(tx1, tx2, ty1, ty2, isInner);
		shade = bevel(toLight, normalAt(d, grad), isInner, false);
	}
	vec3 col = applyShading(c, shade);
   
	gl_FragColor = vec4(col, 1.0);
}

