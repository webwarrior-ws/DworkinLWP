precision mediump float;

uniform float yScale;
uniform float h;
uniform sampler2D sdf;

varying vec2 pos;

float squared(float x) {
	return x*x;
}

float dy(vec2 neighbour) {
	float d = neighbour.r;
	float cosAlpha = (neighbour.g/h - 0.5) * 2.0;
	float sinAlpha = sqrt(1.0 - squared(cosAlpha));
	float dy = sinAlpha < 0.001 ? sign(sinAlpha) : d / sinAlpha;
	return dy;
}

void main() {
	vec4 rawPixel = texture2D(sdf, pos);
	
	if(rawPixel.b > 0.0) discard;

	gl_FragColor = rawPixel;
	if(rawPixel.r > 0.0) return;
	
	// using squared distances to avoid sqrt call in loop
	float d2 = squared(rawPixel.g*h);
	for(float y=1.0; y<=h; y+=1.0) {
		float y2 = y*y;
		if(d2 <= y2) break;
		vec2 shift = vec2(0.0, y*yScale);
		vec2 neighbourUp = texture2D(sdf, pos - shift).rg * h;
		d2 = min(d2, neighbourUp.r == 0.0 ? squared(neighbourUp.g) + y2 : squared(y-dy(neighbourUp)));
		vec2 neighbourDown = texture2D(sdf, pos + shift).rg * h;
		d2 = min(d2, neighbourDown.r == 0.0 ? squared(neighbourDown.g) + y2 : squared(y-dy(neighbourDown)));
	}

	gl_FragColor.g = sqrt(d2)/h;
}

