precision mediump float;

uniform vec2 screenDimensions;
uniform float h;
uniform bool horizontal;
uniform sampler2D sdf;


void main() {
	vec2 pos = gl_FragCoord.xy;
	pos.y = screenDimensions.y-pos.y;
	vec2 texCoords = gl_FragCoord.xy / screenDimensions;
	vec4 rawPixel = texture2D(sdf, texCoords);
	if(rawPixel.r > 0.0)
		discard;

	float d = h;
	if(horizontal) {
		for(float x=1.0; x<h; x+=1.0) {
			vec2 leftPos = texCoords + vec2(-x/screenDimensions.x, 0.0);
			vec2 rightPos = texCoords + vec2(x/screenDimensions.x, 0.0);
			vec4 neighbourLeft = texture2D(sdf, leftPos);
			vec4 neighbourRight = texture2D(sdf, rightPos);
			if(neighbourLeft.r > 0.0 || neighbourRight.r > 0.0) {
				d = x - max(neighbourLeft.r, neighbourRight.r)*h;
				break;
			};
		};
	}
	else {
		d = rawPixel.g*h;
		for(float y=1.0; y<h; y+=1.0) {
			vec2 shift = vec2(0.0, y/screenDimensions.y);
			vec4 neighbourUp = texture2D(sdf, texCoords - shift);
			vec4 neighbourDown = texture2D(sdf, texCoords + shift);
			if(neighbourUp.r == 0.0) {
				d = min(d, length(vec2(neighbourUp.g*h, y)));
			}
			else {
				d = min(d, max(0.0, y-neighbourUp.r*h));
			};
			if (neighbourDown.r == 0.0) {
				d = min(d, length(vec2(neighbourDown.g*h, y)));
			}
			else {
				d = min(d, max(0.0, y-neighbourDown.r*h));
			}
		};
	};

	gl_FragColor = vec4(rawPixel.r, clamp(d/h, 0.0, 1.0), 0.0, 1.0);
}
