#include "main.h"


using namespace std;
using namespace glm;



GLuint VAO[3];
GLuint VBO[3];
GLuint EBO[3];
Shader shaders[3];
int vPosLocation, vColorLocation;

float up_amnt = 0, right_amnt = 0;
// mouse position in pixels. top left is 0.
vec2 mousePos;

// normalised mouse position in decimals. follows cartesian graph; middle is (0, 0), top left is (-1, 1), bottom right is (1, -1), etc.
vec2 normMousePos;

// WIDTH and HEIGHT of window.
const int WIDTH = 800;
const int HEIGHT = 600;

// vertices
vector<mat3> verts = {
		mat3(
			0,  1, 0,
			-.5, 0, 0,
			.5, 0, 0
		),
		mat3(
			-.5, 0, 0,
			-1, -1, 0,
			0, -1, 0
		),
		mat3(
			.5, 0, 0,
			0, -1, 0,
			1, -1, 0
		)
};

GLfloat colors[][12] =
{
	{
		1.0f, 0.0f, 0.0f, 1.0f,
		0.0f, 1.0f, 0.0f, 1.0f,
		0.0f, 0.0f, 1.0f, 1.0f,
	},
	{
		0.0, 1.0f, 0.0f, 1.0f,
		0.0f, 0.0f, 1.0f, 1.0f,
		1.0f, 0.0f, 0.0f, 1.0f
	},
	{
		0.0, 0.0f, 1.0f, 1.0f,
		1.0f, 0.0f, 0.0f, 1.0f,
		0.0f, 1.0f, 0.0f, 1.0f
	},
};

# pragma region shaders
// Vertex Shader (for convenience, it is defined in the main here, but we will be using text files for shaders in future)
// Note: Input to this shader is the vertex positions that we specified for the triangle. 
// Note: gl_Position is a special built-in variable that is supposed to contain the vertex position (in X, Y, Z, W)
// Since our triangle vertices were specified as vec3, we just set W to 1.0.
static const char* pVS;
// Fragment Shaders
static const char* pFS[3];
#pragma endregion

void printVec(vec2 v, bool newline = true) {
	if (newline) cout << "(" << v.x << ", " << v.y << ")" << endl;
	else cout << "(" << v.x << ", " << v.y << ")";
}

// Translate a regular OpenGL graphic (top left is (0, 0), bottom right = (width, height)) to Cartesian coordinates with a range of (-1, 1) on both axes.
// That is, normalise a point's axes.
// e.g., 
// WIDTH = 800
// HEIGHT = 600
// mousePos = vec2(400, 300)
// normalisePos(mousePos) = normMousePos = (0, 0)
// normalisePos(vec2(200, 700)) = (-0.5, -1.333)
vec2 normalisePos(vec2 v) {
	return vec2((v.x - WIDTH / 2) / WIDTH * 2, (v.y - HEIGHT / 2) / HEIGHT * -2);
}

#pragma region KBMHandlers
// Process the mouse moving with button input
void processMouseMovedP(int x, int y) {

}

// Process the mouse moving without button input
void processMouseMovedNP(int x, int y) {
	mousePos = vec2(x, y);
	normMousePos = normalisePos(mousePos);
}

// Process the mouse button being pressed
void processMousePressed(int button, int state, int x, int y) {
	if (state == GLUT_UP)
		cout << "button up" << endl;
	if (state == GLUT_DOWN) {
		cout << "button down: ";
		printVec(normMousePos);
	}
}

// Process regular keyboard characters
void processCharKeys(unsigned char key, int x, int y) {
	if (key == VK_ESCAPE) {
		cout << endl << endl << "Exiting..." << endl;
		exit(0);
	}
	cout << key;
}

// Process special keyboard keys (modifiers (CTRL, LSHIFT, etc.), function keys, arrow keys)
void processSpecKeys(int key, int x, int y) {
	switch (key)
	{
	case GLUT_KEY_UP:
		cout << "up ";
		up_amnt += 0.01;
		break;
	case GLUT_KEY_DOWN:
		//do something here
		cout << "down ";
		up_amnt -= 0.01;
		break;
	case GLUT_KEY_LEFT:
		//do something here
		cout << "left ";
		right_amnt -= 0.01;
		break;
	case GLUT_KEY_RIGHT:
		//do something here
		cout << "right ";
		right_amnt += 0.01;
		break;
	}
}
#pragma endregion

void display()
{
	glClear(GL_COLOR_BUFFER_BIT);
	// NB: Make the call to draw the geometry in the currently activated vertex buffer. This is where the GPU starts to work!
	// Need to call shader here


	int vcl1 = 0, vpl1 = 0, gsl1 = 0, gpl1 = 0;
	shaders[0] = Shader(pVS, pFS[0]);
	shaders[0].use();
	vcl1 = glGetUniformLocation(shaders[0].ID, "vColor");
	glUniform4fv(vcl1, 1, colors[0]);
	gsl1 = glGetUniformLocation(shaders[0].ID, "gScale");
	glUniform1f(gsl1, up_amnt);
	gpl1 = glGetUniformLocation(shaders[0].ID, "gPos");
	glUniform2f(gpl1, right_amnt, right_amnt);
	//vpl1 = glGetUniformLocation(shaders[0].ID, "vPosition");
	//glUniform4f(vpl1, 0, 400, 0, 1);
	//cout << "vposl: " << vpl1 << "\nvcoll: " << vcl1 << endl;
	glBindVertexArray(VAO[0]);
	//glDrawElements(GL_TRIANGLES, 3, GL_UNSIGNED_INT, 0);
	glDrawArrays(GL_TRIANGLES, 0, 3);

	int vcl2 = 0, vpl2 = 0, gsl2 = 0;
	shaders[1] = Shader(pVS, pFS[1]);
	shaders[1].use();
	vcl2 = glGetUniformLocation(shaders[1].ID, "vColor");
	glUniform4fv(vcl2, 1, colors[1]);
	gsl2 = glGetUniformLocation(shaders[0].ID, "gScale");
	glUniform1f(gsl2, up_amnt);
	/*vpl2 = glGetUniformLocation(shaders[1].ID, "vPosition");
	glUniform4f(vpl2, 1, 1, 1, 1);*/
	//cout << "vposl: " << vpl2 << "\nvcoll: " << vcl2 << endl;
	glBindVertexArray(VAO[1]);
	//glDrawElements(GL_TRIANGLES, 3, GL_UNSIGNED_INT, 0);
	glDrawArrays(GL_TRIANGLES, 0, 3);

	int vcl3 = 0, vpl3 = 0, gsl3 = 0;
	shaders[2] = Shader(pVS, pFS[2]);
	shaders[2].use();
	vcl3 = glGetUniformLocation(shaders[2].ID, "vColor");
	glUniform4fv(vcl3, 1, colors[2]);
	gsl2 = glGetUniformLocation(shaders[0].ID, "gScale");
	glUniform1f(gsl2, up_amnt);
	/*vpl3 = glGetUniformLocation(shaders[2].ID, "vPosition");
	glUniform4f(vpl3, 1, 1, 1, 1);*/
	//cout << "vposl: " << vpl3 << "\nvcoll: " << vcl3 << endl;
	glBindVertexArray(VAO[2]);
	//glDrawElements(GL_TRIANGLES, 3, GL_UNSIGNED_INT, 0);
	glDrawArrays(GL_TRIANGLES, 0, 3);


	glutPostRedisplay();
	glutSwapBuffers();
}


void init()
{
	// Create an index buffer for the 2 triangles
	GLuint indices[] =
	{
			0, 1, 2,
			1, 3, 4,
			2, 4, 5
	};

	// Allocate memory to vertex object arrays and index buffers
	glGenVertexArrays(3, VAO);
	glGenBuffers(3, EBO);
	
	for (int i = 0; i < 3; i++)
	{
		// Set up the shaders
		shaders[i] = Shader(pVS, pFS[i]);
		//shaderProgramID[i] = CompileShaders(pVS, pFS[i]);
		//shaderProgramID[i] = shaders[i].ID;
		// Put the vertices and colors into a vertex buffer object
		generateObjectBuffer(VBO[i], verts[i], colors[i]);
		// Link the current buffer to the shader
		linkCurrentBuffertoShader(VAO[i], VBO[i], shaders[i].ID);
		// Link the current index buffer to the vertex array object
		glBindVertexArray(VAO[i]);
		glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO[i]);
		glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(indices), indices, GL_STATIC_DRAW);
	}
}

int main(int argc, char** argv)
{
	string vs = readFile("../../Files/vertex_shader_b.glsl");
	pVS = vs.c_str();

	string fs1 = readFile("../../Files/fragment_shader_1.glsl");
	pFS[0] = fs1.c_str();

	string fs2 = readFile("../../Files/fragment_shader_1.glsl");
	pFS[1] = fs2.c_str();

	string fs3 = readFile("../../Files/fragment_shader_1.glsl");
	pFS[2] = fs3.c_str();

	// Set up the window
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);
	glutInitWindowSize(WIDTH, HEIGHT);
	glutCreateWindow("Lab 2");
	// Tell glut where the display function is
	glutDisplayFunc(display);

	// Keyboard and mouse handlers
	glutKeyboardFunc(processCharKeys);
	glutSpecialFunc(processSpecKeys);
	glutMouseFunc(processMousePressed);
	glutPassiveMotionFunc(processMouseMovedNP);

	// A call to glewInit() must be done after glut is initialized!
	GLenum res = glewInit();
	// Check for any errors
	if (res != GLEW_OK) {
		fprintf(stderr, "Error: '%s'\n", glewGetErrorString(res));
		return 1;
	}
	// Set up your objects and shaders
	init();
	// Begin infinite event loop
	glutMainLoop();
	return 0;
}

