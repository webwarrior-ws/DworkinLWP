precision highp float;
precision mediump sampler2D;

uniform float xScale;
uniform float h;
uniform sampler2D sdf;

varying vec2 pos;


float dx(vec3 neighbour) {
	return  neighbour.r * h;
}


void main() {
	vec4 rawPixel = texture2D(sdf, pos);
	
	if(rawPixel.b > 0.0) discard;

	gl_FragColor = rawPixel;
	if(rawPixel.r > 0.0) return;
	
	float d = h;
	for(float x=1.0; x<=h; x+=1.0) {
		vec2 shift = vec2(x*xScale, 0.0);
		vec3 neighbourLeft = texture2D(sdf, pos - shift).rgb;
		vec3 neighbourRight = texture2D(sdf, pos + shift).rgb;
        if(neighbourLeft.r > 0.0 || neighbourRight.r > 0.0) {
            d = x - max(dx(neighbourLeft), dx(neighbourRight));
            break;
        }
	}

	gl_FragColor.g = d/h;
}

