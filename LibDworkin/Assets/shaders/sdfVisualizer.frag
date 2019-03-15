precision mediump float;

varying vec2 p;

uniform sampler2D sdf;

const float threshold = 0.05;

vec3 vis(float d) {
	vec3 col = vec3(1.0) - sign(d)*vec3(0.1,0.4,0.7);
	col *= 1.0 - exp(-2.0*abs(d));
	col *= 0.8 + 0.2*cos(32.0*d);
	return mix( col, vec3(1.0), 1.0-smoothstep(0.0,0.02,abs(d)) );
}

float mask(vec2 rawPixel) {
	float d = rawPixel.x > 0.0 ? rawPixel.r : -rawPixel.g;
    return smoothstep(-threshold, threshold, d);
}

float outline(vec2 rawPixel, float width) {
	float d = rawPixel.r > 0.0 ? rawPixel.r : rawPixel.g;
	float c = (d + 1.0 - width) / 2.0;
	return smoothstep(0.5-threshold*0.5, 0.5+threshold*0.5, c);
}

void main() {
   vec3 rawPixel = texture2D(sdf, p).rgb;

   // This might be viable optimization technique.
   // On first frame (or when background/foreground/shader changed), draw everything; then only update affected areas.
   // Saves about 25% time.
   //if(rawPixel.b > 0.0) discard;

   float d = rawPixel.r > 0.0 ? rawPixel.r : -rawPixel.g;

   vec3 col;
   col = vis(d);
   //col = vec3(mask(rawPixel.rg));
   //col = vec3(outline(rawPixel.rg, 0.05));

   gl_FragColor = vec4(col, 1.0);
}
