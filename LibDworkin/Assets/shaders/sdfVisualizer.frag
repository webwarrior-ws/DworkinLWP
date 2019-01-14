precision mediump float;

varying vec2 p;

uniform sampler2D sdf;

void main() {
   vec3 rawPixel = texture2D(sdf, p).xyz;

   float d = rawPixel.x > 0.0 ? rawPixel.x : -rawPixel.y;

   vec3 col = vec3(1.0) - sign(d)*vec3(0.1,0.4,0.7);
   col *= 1.0 - exp(-2.0*abs(d));
   col *= 0.8 + 0.2*cos(32.0*d);
   col = mix( col, vec3(1.0), 1.0-smoothstep(0.0,0.02,abs(d)) );
   /*
   float c = d + 1.0 / 2.0;
   float contrast = 10.0;
   float c1 = (c-.45) * contrast;
   float c2 = 1.-(c-.5) * contrast;
   float res = mix(c1, c2, (c-.5)*contrast);
   
   float threshold = 0.025;
   res = smoothstep(0.5-threshold, .5+threshold, c);
   col = vec3(res);
   */
   gl_FragColor = vec4(col, 1.0);
}
