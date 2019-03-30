precision mediump float;

uniform float xScale;
uniform float h;
uniform sampler2D sdf;

varying vec2 pos;


void main() {
	vec4 rawPixel = texture2D(sdf, pos);
	
	if(rawPixel.b > 0.0) discard;

	gl_FragColor = rawPixel;
	if(rawPixel.r > 0.0) return;
	
	float d = h;
	for(float x=1.0; x<=h; x+=1.0) {
		vec2 shift = vec2(x*xScale, 0.0);
		float neighbourLeft = texture2D(sdf, pos - shift).r;
		float neighbourRight = texture2D(sdf, pos + shift).r;
        if(neighbourLeft > 0.0 || neighbourRight > 0.0) {
            d = x - max(neighbourLeft, neighbourRight)*h;
            break;
        }
	}

	gl_FragColor.g = d/h;
}

