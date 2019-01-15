precision mediump float;

varying vec4 segment;
varying vec2 p;

uniform float h;

void main() {
	vec2 a = segment.xy;
	vec2 b = segment.zw;
	float d;
	/*if(a == b) {
		d = distance(p, a);
	}
	else {*/
		// http://www.iquilezles.org/www/articles/distfunctions2d/distfunctions2d.htm
		vec2 pa = p-a, ba = b-a;
		float h1 = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
		d = length( pa - ba*h1 );
	//};
	gl_FragColor = vec4(clamp(d/h, 0.0, 1.0), 1.0, 0.0, 1.0);
}
