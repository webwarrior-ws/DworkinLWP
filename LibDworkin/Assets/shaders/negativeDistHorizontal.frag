precision mediump float;

uniform float xScale;
uniform float h;
uniform sampler2D sdf;

varying vec2 pos;


float dx(vec2 neighbour) {
	float d = neighbour.r * h;
	float cosAlpha = (neighbour.g - 0.5) * 2.0;
	float dx = cosAlpha < 0.001 ? sign(cosAlpha) : d / cosAlpha;
	return d > 0.0 ? dx : 0.0;
}


void main() {
	vec4 rawPixel = texture2D(sdf, pos);
	
	if(rawPixel.b > 0.0) discard;

	gl_FragColor = rawPixel;
	if(rawPixel.r > 0.0) {
		gl_FragColor.g = 1.0;
		return;
	}
	
	float d = h;
	for(float x=1.0; x<=h; x+=1.0) {
		vec2 shift = vec2(x*xScale, 0.0);
		vec2 neighbourLeft = texture2D(sdf, pos - shift).rg;
		vec2 neighbourRight = texture2D(sdf, pos + shift).rg;
        if(neighbourLeft.r > 0.0 || neighbourRight.r > 0.0) {
            d = x - max(dx(neighbourLeft), dx(neighbourRight));
            break;
        }
	}

	gl_FragColor.g = d/h;
}

