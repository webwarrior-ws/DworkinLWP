precision mediump float;

varying vec2 pos;

uniform vec4 color;
uniform mat2 rotation; // !rotation[0, input:degrees]
uniform float ringsScale; // !slider[2.0, 2.7, 5.0]
uniform float ringsStretch; // !slider[0.0, 15.0, 50.0]
uniform float contrast; // !slider[0.0, 0.25, 0.5]

uniform sampler2D grainTexture; // !texture["../../textures/Wood_grain.png"]
uniform vec2 screenDimensions;

const int woodTextureOctaves = 6;

const int NOISE_GRADIENT_OCTAVES = 8;

// https://github.com/tobspr/GLSL-Color-Spaces/blob/master/ColorSpaces.inc.glsl
const float HCV_EPSILON = 1e-10;
const float HSL_EPSILON = 1e-10;

// Converts a value from linear RGB to HCV (Hue, Chroma, Value)
vec3 rgb_to_hcv(vec3 rgb)
{
    // Based on work by Sam Hocevar and Emil Persson
    vec4 P = (rgb.g < rgb.b) ? vec4(rgb.bg, -1.0, 2.0/3.0) : vec4(rgb.gb, 0.0, -1.0/3.0);
    vec4 Q = (rgb.r < P.x) ? vec4(P.xyw, rgb.r) : vec4(rgb.r, P.yzx);
    float C = Q.x - min(Q.w, Q.y);
    float H = abs((Q.w - Q.y) / (6.0 * C + HCV_EPSILON) + Q.z);
    return vec3(H, C, Q.x);
}
// Converts from linear rgb to HSL
vec3 rgb_to_hsl(vec3 rgb)
{
    vec3 HCV = rgb_to_hcv(rgb);
    float L = HCV.z - HCV.y * 0.5;
    float S = HCV.y / (1.0 - abs(L * 2.0 - 1.0) + HSL_EPSILON);
    return vec3(HCV.x, S, L);
}

vec3 saturate(vec3 v) { return clamp(v, 0.0, 1.0); }

// Converts from pure Hue to linear RGB
vec3 hue_to_rgb(float hue)
{
    float R = abs(hue * 6.0 - 3.0) - 1.0;
    float G = 2.0 - abs(hue * 6.0 - 2.0);
    float B = 2.0 - abs(hue * 6.0 - 4.0);
    return saturate(vec3(R,G,B));
}

// Converts from HSL to linear RGB
vec3 hsl_to_rgb(vec3 hsl)
{
    vec3 rgb = hue_to_rgb(hsl.x);
    float C = (1.0 - abs(2.0 * hsl.z - 1.0)) * hsl.y;
    return (rgb - 0.5) * C + hsl.z;
}

//	<https://www.shadertoy.com/view/4dS3Wd>
//	By Morgan McGuire @morgan3d, http://graphicscodex.com
//
float hash(float n) { return fract(sin(n) * 1e4); }

float noise(float x) {
	float i = floor(x);
	float f = fract(x);
	float u = f * f * (3.0 - 2.0 * f);
	return (mix(hash(i), hash(i + 1.0), u) - 0.5) * 2.0;
}

float valueNoise(float x, int numOctaves, float roughness) {
	float v = 0.0;
	float a = 0.5;
	float shift = 100.0;
	for (int i = 0; i < numOctaves; ++i) {
		v += a * noise(x);
		x = x * 2.0 + shift;
		a *= roughness;
	}
	return v;
}

vec3 noiseGradient(
		float t,
		vec3 baseColor,
		float hRange,
		float lRange,
		float sRange,
		float roughness,
		float contrast,
		float scale) {
	vec3 hsl = rgb_to_hsl(baseColor);
	float x = t * scale;
	hsl.x += valueNoise(x, NOISE_GRADIENT_OCTAVES, roughness) * hRange * 0.5;
	hsl.y += valueNoise(x + 2.0*scale, NOISE_GRADIENT_OCTAVES, roughness) * sRange * 0.5;
	hsl.z += valueNoise(x + 4.0*scale, NOISE_GRADIENT_OCTAVES, roughness) * lRange * 0.5;
	hsl = clamp(hsl, 0.0, 1.0);
	hsl.z = clamp(
		(hsl.z - 0.5) * (1.0 + contrast*2.0) + 0.5,
		0.0, 1.0);
	vec3 rgb = hsl_to_rgb(hsl);
	return rgb;
}

/**
 * Description : Array and textureless GLSL 2D simplex noise function.
 * Author : Ian McEwan, Ashima Arts.
 * Maintainer : ijm
 * Lastmod : 20110822 (ijm)
 * License : Copyright (C) 2011 Ashima Arts. All rights reserved.
 * Distributed under the MIT License. See LICENSE file.
 * https://github.com/ashima/webgl-noise
 *
 */

vec3 mod289(vec3 x) {
	return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec2 mod289(vec2 x) {
	return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec3 permute(vec3 x) {
	return mod289(((x * 34.0) + 1.0) * x);
}

float snoise(vec2 v) {
	const vec4 C = vec4(0.211324865405187, // (3.0-sqrt(3.0))/6.0
			0.366025403784439, // 0.5*(sqrt(3.0)-1.0)
			-0.577350269189626, // -1.0 + 2.0 * C.x
			0.024390243902439); // 1.0 / 41.0
	// First corner
	vec2 i = floor(v + dot(v, C.yy));
	vec2 x0 = v - i + dot(i, C.xx);

	// Other corners
	vec2 i1;
	//i1.x = step( x0.y, x0.x ); // x0.x > x0.y ? 1.0 : 0.0
	//i1.y = 1.0 - i1.x;
	i1 = (x0.x > x0.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0);
	// x0 = x0 - 0.0 + 0.0 * C.xx ;
	// x1 = x0 - i1 + 1.0 * C.xx ;
	// x2 = x0 - 1.0 + 2.0 * C.xx ;
	vec4 x12 = x0.xyxy + C.xxzz;
	x12.xy -= i1;

	// Permutations
	i = mod289(i); // Avoid truncation effects in permutation
	vec3 p = permute(permute(i.y + vec3(0.0, i1.y, 1.0))
			+ i.x + vec3(0.0, i1.x, 1.0));

	vec3 m = max(0.5 - vec3(dot(x0, x0), dot(x12.xy, x12.xy), dot(x12.zw, x12.zw)), 0.0);
	m = m * m;
	m = m * m;

	// Gradients: 41 points uniformly over a line, mapped onto a diamond.
	// The ring size 17*17 = 289 is close to a multiple of 41 (41*7 = 287)
	vec3 x = 2.0 * fract(p * C.www) - 1.0;
	vec3 h = abs(x) - 0.5;
	vec3 ox = floor(x + 0.5);
	vec3 a0 = x - ox;

	// Normalise gradients implicitly by scaling m
	// Approximation of: m *= inversesqrt( a0*a0 + h*h );
	m *= 1.79284291400159 - 0.85373472095314 * (a0 * a0 + h * h);

	// Compute final noise value at P
	vec3 g;
	g.x = a0.x * x0.x + h.x * x0.y;
	g.yz = a0.yz * x12.xz + h.yz * x12.yw;
	return 130.0 * dot(m, g);
}

float valueNoise(vec2 x, int numOctaves, float roughness) {
	float v = 0.0;
	float a = 0.5;
	vec2 shift = vec2(100.0);
	// Rotate to reduce axial bias
    mat2 rot = mat2(cos(0.5), sin(0.5), -sin(0.5), cos(0.50));
	for (int i = 0; i < numOctaves; ++i) {
		v += a * snoise(x);
		x = rot * x * 2.0 + shift;
		a *= roughness;
	}
	return v;
}

void main() {
	vec2 p = pos * ringsScale * rotation;
	p.y /= 1.0 + ringsStretch;
	p.x *= screenDimensions.x / screenDimensions.y;
	float rawNoise = valueNoise(p, woodTextureOctaves, 0.37) + 1.0;
	float t = sin(rawNoise * 3.14) * 0.5;
	vec3 base = noiseGradient(t, color.rgb, 0.01, 0.27, 0.05, 0.7, contrast, 50.0);
	float grainConstrast = mix(0.15, 0.35, contrast * 2.0);
	vec3 baseDarker = base*base * grainConstrast + base * (1.0 - grainConstrast);
	vec2 pf = pos * rotation * (screenDimensions / 256.0);
	float fibers = texture2D(grainTexture, pf).r;
	vec3 result = mix(baseDarker, base, step(fibers, 0.68));
	gl_FragColor = vec4(result, 1.0);
}
