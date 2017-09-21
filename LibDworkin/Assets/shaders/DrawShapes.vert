attribute vec2 pos; 
uniform vec2 scale;

void main() { 
	gl_Position = vec4((pos * scale - 0.5) * 2.0, 0.0, 1.0); 
}