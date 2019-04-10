precision mediump float;

varying vec2 pos;

uniform sampler2D sdf;
uniform sampler2D background;
uniform sampler2D foreground;
uniform vec2 screenDimensions;

uniform float contrast; // !slider[1.0, 5.0, 10.0]
uniform float maxDistance; // !slider[0.0, 1.0, 1.0]
uniform vec2 lightPos; // !direction[0.5, 0.75]
uniform float lightHeight; // !slider[0.2, 1.0, 2.0]
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

float dist(vec2 p, sampler2D sdfTex, bool isInner) {
    vec4 texel = texture2D(sdfTex, p);
    return clamp((isInner ? texel.g : texel.r), 0.0, maxDistance);
}

vec3 normalAt(vec2 p, sampler2D sdfTex, vec2 screenDims, bool isInner) {
    vec2 ps = vec2(1.0) / screenDims;
    float x1 = dist(p - vec2(ps.x, 0.0), sdfTex, isInner);
    float x2 = dist(p + vec2(ps.x, 0.0), sdfTex, isInner);
    float y1 = dist(p - vec2(0.0, ps.y), sdfTex, isInner);
    float y2 = dist(p + vec2(0.0, ps.y), sdfTex, isInner);
    float d = (x1+x2+y1+y2)*0.25/maxDistance;
    vec2 grad = vec2(x1-x2, y1-y2);
    
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

// TODO: receive normal as argument to avoid duplicate calculations.
// Mix normals instead of mixing shades.
float bevel(vec2 p, sampler2D sdfTex, vec2 screenDims, bool isInner, bool isDown) {
    vec3 toLight = normalize(vec3(lightPos, lightHeight));
    vec3 normal = normalAt(p, sdfTex, screenDims, isInner);
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

	vec3 c = mix(fgColor, bgColor, m);
	float shade;
	if(mode >= 2) { // both
		// TODO: remove branching and leave only "else" branch (is faster)
		if((rawPixel.g == 0.0 && rawPixel.r == 1.0) || (rawPixel.g == 1.0 && rawPixel.r == 0.0)) {
			bool isDown = rawPixel.r == 0.0 ? mode==2 : mode==3;
			shade = bevel(pos, sdf, screenDimensions, rawPixel.r == 0.0, isDown);
		}
		else {
			float outer = bevel(pos, sdf, screenDimensions, false, mode==3);
			float inner = bevel(pos, sdf, screenDimensions, true, mode==2);
			shade = mix(inner, outer, m);
		}
	}
	else {
		shade = bevel(pos, sdf, screenDimensions, mode!=1, false);
	}
	vec3 col = applyShading(c, shade);
   
	gl_FragColor = vec4(col, 1.0);
}

