precision highp float;

varying vec2 pos;

uniform float aspectRatio;


uniform vec4 color; // !color[0.8, 0.8, 0.8]
uniform float highlight; // !slider[0.0, 0.2, 0.4]
uniform float scale;
uniform vec2 offset; // !direction[1.0, 0.0]

uniform mat2 rotation; // !rotation[0, input:degrees]
uniform float stretch; // !slider[0.0, 0.0, 25.0]

uniform float strokeFreq; // !slider[200.0, 500.0, 1000.0]
uniform float strokeLength; // !slider[0.1, 0.5, 1.0]
uniform float strokeIntensity; // !slider[0.0, 0.35, 1.0]
uniform mat2 strokeRotation; // !rotation[0, input:degrees]


/**
 * This shader was written by Mikael Hvidtfeldt Christensen
 * (mail: mikael (at) hvidtfeldts.net) and comes from his own
 * creation: Fragmentarium - cross-platform application for exploring
 * pixel based graphics on the GPU (http://syntopia.github.io/Fragmentarium/).
 *
 * Licensed under Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License
 * (http://creativecommons.org/licenses/by-nc-sa/3.0/deed.en_US).
 */

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

float blendOverlay(float base, float blend) {
    return base<0.5?(2.0*base*blend):(1.0-2.0*(1.0-base)*(1.0-blend));
}

vec3 blendOverlay(vec3 base, vec3 blend) {
    return vec3(blendOverlay(base.r,blend.r),blendOverlay(base.g,blend.g),blendOverlay(base.b,blend.b));
}

vec3 blendOverlay(vec3 base, vec3 blend, float opacity) {
    return (blendOverlay(base, blend) * opacity + base * (1.0 - opacity));
}


vec3 metal(vec2 p) {
    vec3 highlightColor = min(vec3(1.0), color.rgb + vec3(highlight));
    float t = snoise((p + offset) * scale) * 0.5 + 0.5;
    return blendOverlay(color.rgb, highlightColor, t);
}

vec3 brushed_metal(vec2 p, mat2 noiseTransform,
        float strokeFreq, float strokeLength,
        float strokeIntensity, mat2 strokeRotation) {
    vec3 bg = metal(p * noiseTransform);
    vec2 pHf = p * mat2(aspectRatio, 0.0, 0.0, 1.0) * strokeRotation;
    float highFreq = snoise(vec2(pHf.x / strokeLength - 100.0, pHf.y * strokeFreq + 100.0)) * 0.5 + 0.5;
    float v = max(color.r, max(color.g, color.b));
    float a = (sqrt(abs(0.33-v)) + 0.2) * strokeIntensity; // needs improvement
    return blendOverlay(bg, vec3(highFreq), a);
}


mat2 transfrom = rotation * mat2(aspectRatio, 0.0, 0.0, 1.0/(stretch+1.0));

void main() {
	vec3 bgColor;
    if(strokeIntensity > 0.0)
        bgColor = brushed_metal(
            pos,
            transfrom,
            strokeFreq,
            strokeLength,
            strokeIntensity,
            strokeRotation);
    else
        bgColor = metal(pos * transfrom);

	gl_FragColor = vec4(bgColor, 1.0);
}

