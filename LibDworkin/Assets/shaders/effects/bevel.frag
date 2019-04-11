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

float bevel(vec3 toLight, vec3 normal, bool isInner, bool isDown) {
	vec3 n = normal;
    if(isInner == isDown) {
        n.xy = -n.xy;
    }
    float flatShade = toLight.z; //dot(toLight, vec3(0.0, 0.0, 1.0));
    float shade = clamp(dot(toLight, n), 0.0, 1.0);
    return shade - flatShade;
}

vec3 applyShading(vec3 c, float shade) {
    return shade <= 0.0 ? vec3(1.0+shade) * c : c + vec3(shade);
}


void main() {
	vec3 rawPixel = texture2D(sdf, pos).rgb;

	//if(rawPixel.b > 0.0) discard;

	float m = mask(rawPixel.rg);
	vec3 fgColor = texture2D(foreground, pos).rgb;
	vec3 bgColor = texture2D(background, pos).rgb;

	vec2 tx1 = texture2D(sdf, px1).rg;
    vec2 tx2 = texture2D(sdf, px2).rg;
    vec2 ty1 = texture2D(sdf, py1).rg;
    vec2 ty2 = texture2D(sdf, py2).rg;

	vec3 c = mix(fgColor, bgColor, m);

	float inner=0.0, outer=0.0;
	// outer
	if(mode != 0 && m > 0.0) {
		vec4 tOuter = clamp(vec4(tx1.r, tx2.r, ty1.r, ty2.r), -maxDistance, maxDistance);
		vec2 grad = gradient(tOuter);
		float d = dist(rawPixel.rg, false) / maxDistance;
		outer = bevel(toLight, normalAt(d, grad), false, mode==3);
	}
	// inner
	if(mode != 1 && m < 1.0) {
		vec4 tInner = clamp(vec4(dInner(tx1), dInner(tx2), dInner(ty1), dInner(ty2)), -maxDistance, maxDistance);
		vec2 grad = gradient(tInner);
		float d = dist(rawPixel.rg, true) / maxDistance;
		inner = bevel(toLight, normalAt(d, grad), true, mode==2);
	}
	float shade = mix(inner, outer, m);

	vec3 col = applyShading(c, shade);
   
	gl_FragColor = vec4(col, 1.0);
}

