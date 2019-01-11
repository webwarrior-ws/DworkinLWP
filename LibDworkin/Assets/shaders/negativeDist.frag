precision highp float;

uniform vec2 screenDimensions;
uniform vec2 scale;
uniform float h;
uniform bool horizontal;
uniform sampler2D sdf;


float len(float h, float v) {
	return sqrt(h*h + v*v);
}


void main() {
	vec2 pos = gl_FragCoord.xy;
	vec2 texCoords = pos / screenDimensions;
	vec4 rawPixel = texture2D(sdf, texCoords);
	
	if(rawPixel.r > 0.0) {
		gl_FragColor = rawPixel;
		return;
	}
	
	float maxDist = h * (screenDimensions.x * scale.x);
	float d = maxDist;
	if(horizontal) {
		for(float x=1.0; x<maxDist; x+=1.0) {
			vec2 leftPos = texCoords + vec2(-x/screenDimensions.x, 0.0);
			vec2 rightPos = texCoords + vec2(x/screenDimensions.x, 0.0);
			vec4 neighbourLeft = texture2D(sdf, leftPos);
			vec4 neighbourRight = texture2D(sdf, rightPos);
			if(neighbourLeft.r > 0.0 || neighbourRight.r > 0.0) {
				d = x - max(neighbourLeft.r, neighbourRight.r)*maxDist;
				break;
			};
		};
	}
	else {
		d = rawPixel.g*maxDist;
		for(float y=1.0; y<maxDist; y+=1.0) {
			vec2 shift = vec2(0.0, y/screenDimensions.y);
			vec4 neighbourUp = texture2D(sdf, texCoords - shift);
			vec4 neighbourDown = texture2D(sdf, texCoords + shift);
			bool exit = false;
			if(neighbourUp.r == 0.0) {
				d = min(d, len(neighbourUp.g*maxDist, y));
			}
			else {
				d = min(d, max(0.0, y-neighbourUp.r*maxDist));
				exit = true;
			};
			if (neighbourDown.r == 0.0) {
				d = min(d, len(neighbourDown.g*maxDist, y));
			}
			else {
				d = min(d, max(0.0, y-neighbourDown.r*maxDist));
				exit = true;
			}
			if(exit) break;
		};
	};

	gl_FragColor = vec4(rawPixel.r, clamp(d/maxDist, 0.0, 1.0), 0.0, 1.0);
}
