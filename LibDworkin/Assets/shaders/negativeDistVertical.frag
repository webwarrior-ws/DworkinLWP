precision mediump float;

uniform float yScale;
uniform float h;
uniform sampler2D sdf;

varying vec2 pos;

float len(float x, float y) {
	return sqrt(x*x + y*y);
}


void main() {
	vec4 rawPixel = texture2D(sdf, pos);
	
	if(rawPixel.b > 0.0) discard;

	gl_FragColor = rawPixel;
	if(rawPixel.r > 0.0) return;
	
	float d = rawPixel.g*h;
	for(float y=1.0; y<=h; y+=1.0) {
		if(d <= y) break;
		vec2 shift = vec2(0.0, y*yScale);
		vec2 neighbourUp = texture2D(sdf, pos - shift).rg * h;
		d = min(d, neighbourUp.r == 0.0 ? len(neighbourUp.g, y) : y-neighbourUp.r);
		vec2 neighbourDown = texture2D(sdf, pos + shift).rg * h;
		d = min(d, neighbourDown.r == 0.0 ? len(neighbourDown.g, y) : y-neighbourDown.r);
	}

	gl_FragColor.g = d/h;
}

