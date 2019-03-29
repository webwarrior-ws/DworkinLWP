precision mediump float;

varying vec2 pos;

uniform sampler2D sdf;
uniform sampler2D background;
uniform sampler2D foreground;

const float threshold = 0.05;

float mask(vec2 rawPixel) {
	float d = rawPixel.r > 0.0 ? rawPixel.r : -rawPixel.g;
    return smoothstep(-threshold, threshold, d);
}


void main() {
   vec3 rawPixel = texture2D(sdf, pos).rgb;

   float m = mask(rawPixel.rg);
   vec3 col = mix(texture2D(foreground, pos).rgb, texture2D(background, pos).rgb, m);
   
   gl_FragColor = vec4(col, 1.0);
}
