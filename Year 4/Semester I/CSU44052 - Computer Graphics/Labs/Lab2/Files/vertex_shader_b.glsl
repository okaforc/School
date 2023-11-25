#version 330

in vec4 vPosition; // the vertex in local coordinate system 
uniform vec4 vColor;
uniform float gScale;
uniform vec2 gPos;
out vec4 color;

void main () { 
	//gl_Position = vec4(1, 1, 1, 1);
	gl_Position = vec4(gScale * (vPosition.x + gPos.x), gScale * (vPosition.y + gPos.y), vPosition.z, 1.0);
	color = vColor;
}