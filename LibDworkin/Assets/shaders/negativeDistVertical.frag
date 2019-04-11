precision mediump float;

uniform float yScale;
uniform float h;
uniform sampler2D sdf;

varying vec2 pos;

float squared(float x) {
	return x*x;
}

float l2(float y, vec2 neighbour) {
	float d = neighbour.r * h;
	return squared(y - d);
}

void main() {
	vec4 rawPixel = texture2D(sdf, pos);
	
	if(rawPixel.b > 0.0) discard;

	gl_FragColor = rawPixel;
	if(rawPixel.r > 0.0) return;
	
	// using squared distances to avoid sqrt call in loop
	float d2 = squared(rawPixel.g*h);
	for(float y=1.0; d2 > squared(y - 1.0); y+=1.0) {
		float y2 = y*y;
		vec2 shift = vec2(0.0, y*yScale);
		vec2 neighbourUp = texture2D(sdf, pos - shift).rg;
		d2 = min(d2, neighbourUp.r == 0.0 ? squared(neighbourUp.g * h) + y2 : l2(y, neighbourUp));
		vec2 neighbourDown = texture2D(sdf, pos + shift).rg;
		d2 = min(d2, neighbourDown.r == 0.0 ? squared(neighbourDown.g * h) + y2 : l2(y, neighbourDown));
	}

	gl_FragColor.g = sqrt(d2)/h;
}

