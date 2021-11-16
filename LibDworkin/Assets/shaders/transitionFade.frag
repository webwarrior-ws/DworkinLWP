precision mediump float;
precision mediump sampler2D;

varying vec2 pos;

uniform sampler2D background;
uniform sampler2D oldImage;
uniform float t;


void main() {
   vec3 newColor = texture2D(background, pos).rgb;
   vec3 oldColor = texture2D(oldImage, pos).rgb;
   gl_FragColor = vec4(mix(oldColor, newColor, t), 1.0);
}

