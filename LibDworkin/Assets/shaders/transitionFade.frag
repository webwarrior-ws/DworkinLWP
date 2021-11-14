precision mediump float;
precision mediump sampler2D;

varying vec2 pos;

uniform sampler2D background;
uniform float t;


void main() {
   vec3 color = texture2D(background, pos).rgb;
   float alpha = t;
   gl_FragColor = vec4(color, alpha);
}

