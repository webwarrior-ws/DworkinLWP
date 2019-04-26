precision mediump float;

varying vec2 pos;

uniform vec4 color;
uniform vec2 scale;

uniform vec2 lightPos; // !direction[0.5, 0.75]
uniform float lightHeight; // !slider[0.2, 0.5, 2.0]
uniform float bumpHeight; // !slider[0.0, 0.1, 0.25]
uniform float dim; // !slider[0.1, 0.3, 1.0]
uniform float featureScale; // !slider[1, 50, 100]
uniform float flatArea; // !slider[0.5, 1.0, 2.5]

const float threshold = 0.05;
float aspectRatio = scale.y / scale.x;

float mask(vec2 rawPixel) {
    float d = rawPixel.r > 0.0 ? rawPixel.r : -rawPixel.g;
    return smoothstep(-threshold, threshold, d);
}

float random (vec2 st) {
    return fract(sin(dot(st.xy,
                         vec2(12.9898,78.233)))*
        43758.5453123);
}

// Based on Morgan McGuire @morgan3d
// https://www.shadertoy.com/view/4dS3Wd
float noise (vec2 st) {
	vec2 i = floor(st);
	vec2 f = fract(st);

	// Four corners in 2D of a tile
	float a = random(i);
	float b = random(i + vec2(1.0, 0.0));
	float c = random(i + vec2(0.0, 1.0));
	float d = random(i + vec2(1.0, 1.0));

    vec2 u = f * f * (3.0 - 2.0 * f);

    return mix(a, b, u.x) +
            (c - a)* u.y * (1.0 - u.x) +
            (d - b) * u.x * u.y;
}

vec2 rot90(vec2 p) {
    return vec2(-p.y, p.x);
}

float fbm(vec2 x, float dimension, float lacunarity, int numOctaves) {
    float v = 0.0;
    float a = 0.5;
    vec2 shift = vec2(100.0, 100.0);
    // Rotate to reduce axial bias
    mat2 rot = mat2(cos(0.5), sin(0.5), -sin(0.5), cos(0.50));
    for (int i = 0; i < numOctaves; ++i) {
        v += a * noise(x);
        x = rot * x * lacunarity + shift;
        a /= (1.0 + dimension);
    }
    return v;
}

float stuccoHeight(vec2 p) {
    vec2 p2 = rot90(p);
    int detail = 5;
    float res =
        fbm(p2, dim, 2.0, detail)
        - fbm(p, dim, 2.0, detail-1) * flatArea;
    res = clamp(1.0-res, 0.0, 1.0);
    return res;
}

vec3 stuccoNormal(vec2 p, float height) {
    vec2 size = vec2(2.0, 0.0);
    float d = 0.025;
    float h = height/d;
    vec3 off = vec3(-1.0, 0.0, 1.0) * d;
    float s01 = stuccoHeight(p + off.xy) * h;
    float s21 = stuccoHeight(p + off.zy) * h;
    float s10 = stuccoHeight(p + off.yx) * h;
    float s12 = stuccoHeight(p + off.yz) * h;
    vec3 va = normalize(vec3(size.xy, s21-s01));
    vec3 vb = normalize(vec3(size.yx, s12-s10));
    return cross(va, vb);
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
	vec2 pScaled = pos * featureScale;
	pScaled.y /= aspectRatio;
	vec3 normal = stuccoNormal(pScaled, bumpHeight);
	vec3 toLight = normalize(vec3(lightPos, lightHeight));
	gl_FragColor = vec4(applyBump(color.rgb, normal, toLight), 1.0);
}

