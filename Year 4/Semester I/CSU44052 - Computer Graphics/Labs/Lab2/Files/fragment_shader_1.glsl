#version 330

in vec4 color;
out vec4 FragColor;

void main()
{
	//FragColor = vec4(1, 1, 1, 1);
	FragColor = color;
}