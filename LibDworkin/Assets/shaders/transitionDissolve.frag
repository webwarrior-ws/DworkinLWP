precision mediump float;

varying vec2 pos;

uniform sampler2D background;
uniform sampler2D noise;
uniform float t;
uniform float ratio;


float lt(float a, float b) {
	return clamp(1.0 - smoothstep(0.0, 1.0, a-b + 0.5), 0.0, 1.0);
}

float gain(float x, float k) 
{
    float a = 0.5*pow(2.0*((x<0.5)?x:1.0-x), k);
    return (x<0.5)?a:1.0-a;
}


void main() {
	vec2 pScaled = pos;
	if(ratio < 1.0)
		pScaled.x *= ratio;
	else
		pScaled.y /= ratio;
   vec3 color = texture2D(background, pos).rgb;
   float noiseValue = texture2D(noise, pScaled).r;
   float alpha = gain(lt(noiseValue, t), 4.0);
   gl_FragColor = vec4(color, alpha);
}

