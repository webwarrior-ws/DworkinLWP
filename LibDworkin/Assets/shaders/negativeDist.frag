precision mediump float;

uniform vec2 screenDimensions;
uniform float h;
uniform bool horizontal;
uniform sampler2D sdf;

varying vec2 pos;

float len(float x, float y) {
	return sqrt(x*x + y*y);
}


void main() {
	vec4 rawPixel = texture2D(sdf, pos);
	
	if(rawPixel.b > 0.0) discard;

	if(rawPixel.r > 0.0) {
		gl_FragColor = rawPixel;
		return;
	}
	
	float d = h;
	if(horizontal) {
		float xScale = 1.0/screenDimensions.x;
		for(float x=1.0; x<h; x+=1.0) {
			vec2 shift = vec2(x*xScale, 0.0);
			float neighbourLeft = 0.0;
			float neighbourRight = 0.0;
			if((neighbourLeft = texture2D(sdf, pos - shift).r) > 0.0 || 
			   (neighbourRight = texture2D(sdf, pos + shift).r) > 0.0) {
				d = x - max(neighbourLeft, neighbourRight)*h;
				break;
			};
		};
	}
	else {
		d = rawPixel.g*h;
		float yScale = 1.0/screenDimensions.y;
		for(float y=1.0; y<h; y+=1.0) {
			vec2 shift = vec2(0.0, y*yScale);
			vec4 neighbourUp = texture2D(sdf, pos - shift);
			vec4 neighbourDown = texture2D(sdf, pos + shift);
			if(neighbourUp.r == 0.0) {
				d = min(d, len(neighbourUp.g*h, y));
			}
			else {
				d = min(d, max(0.0, y-neighbourUp.r*h));
			};
			if (neighbourDown.r == 0.0) {
				d = min(d, len(neighbourDown.g*h, y));
			}
			else {
				d = min(d, max(0.0, y-neighbourDown.r*h));
			}
		};
	};

	gl_FragColor = vec4(rawPixel.r, clamp(d/h, 0.0, 1.0), rawPixel.b, 1.0);
}
