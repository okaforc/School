#pragma warning(disable : 5208)

//// Windows includes (For Time, IO, etc.)
//#include <windows.h>
//#include <mmsystem.h>
//#include <iostream>
//#include <string>
//#include <stdio.h>
//#include <math.h>
//#include <vector> // STL dynamic memory.
//
//// OpenGL includes
//#include <GL/glew.h>
//#include <GL/freeglut.h>
//
//// Assimp includes
//#include <assimp/cimport.h> // scene importer
//#include <assimp/scene.h> // collects data
//#include <assimp/postprocess.h> // various extra operations
//
//// Project includes
//#include "maths_funcs.h"

#include "main.h"


/*----------------------------------------------------------------------------
MESH TO LOAD
----------------------------------------------------------------------------*/
// this mesh is a dae file format but you should be able to use any other format too, obj is typically what is used
// put the mesh in your project directory, or provide a filepath for it here
//#define MESH_NAME "monkeyhead_smooth.dae"
// https://free3d.com/3d-model/ricken-backer-4003---bass-888215.html
//#define MESH_NAME "untitled.obj"
//#define MESH_NAME "place.obj"
//#define MESH_NAME "cube.dae"
#define MESH_CUBOID "cuboid.dae"
#define MESH_CUBE "cube.dae"
#define MESH_SPHERE "sphere.dae"
#define MESH_MONKEY_HEAD "monkeyhead_smooth.dae"
#define MESH_GUITAR "rickenbacker 4003_High.obj"
#define MESH_BIN "rubbishBinCircular.dae"

#define TEXTURE_WOOD "bin_texture.jpg"
#define TEXTURE_PIC "pic.jpg"
#define TEXTURE_PIC2 "pic3.jpg"
/*----------------------------------------------------------------------------
----------------------------------------------------------------------------*/


#pragma region MESH LOADING
/*----------------------------------------------------------------------------
MESH LOADING FUNCTION
----------------------------------------------------------------------------*/

ModelData load_mesh(const char* file_name) {
	ModelData modelData;

	/* Use assimp to read the model file, forcing it to be read as    */
	/* triangles. The second flag (aiProcess_PreTransformVertices) is */
	/* relevant if there are multiple meshes in the model file that   */
	/* are offset from the origin. This is pre-transform them so      */
	/* they're in the right position.                                 */
	const aiScene* scene = aiImportFile(
		file_name,
		aiProcess_Triangulate | aiProcess_PreTransformVertices
	);

	if (!scene) {
		fprintf(stderr, "ERROR: reading mesh %s\n", file_name);
		return modelData;
	}

	printf("  %i materials\n", scene->mNumMaterials);
	printf("  %i meshes\n", scene->mNumMeshes);
	printf("  %i textures\n", scene->mNumTextures);

	for (unsigned int m_i = 0; m_i < scene->mNumMeshes; m_i++) {
		const aiMesh* mesh = scene->mMeshes[m_i];
		printf("    %i vertices in mesh\n", mesh->mNumVertices);
		modelData.mPointCount += mesh->mNumVertices;
		for (unsigned int v_i = 0; v_i < mesh->mNumVertices; v_i++) {
			if (mesh->HasPositions()) {
				const aiVector3D* vp = &(mesh->mVertices[v_i]);
				modelData.mVertices.push_back(vec3(vp->x, vp->y, vp->z));
			}
			if (mesh->HasNormals()) {
				const aiVector3D* vn = &(mesh->mNormals[v_i]);
				modelData.mNormals.push_back(vec3(vn->x, vn->y, vn->z));
			}
			if (mesh->HasTextureCoords(0)) {
				const aiVector3D* vt = &(mesh->mTextureCoords[0][v_i]);
				modelData.mTextureCoords.push_back(vec2(vt->x, vt->y));
			}
			if (mesh->HasTangentsAndBitangents()) {
				/* You can extract tangents and bitangents here              */
				/* Note that you might need to make Assimp generate this     */
				/* data for you. Take a look at the flags that aiImportFile  */
				/* can take.                                                 */
			}
		}
	}

	aiReleaseImport(scene);
	return modelData;
}

#pragma endregion MESH LOADING

// Shader Functions- click on + to expand
#pragma region SHADER_FUNCTIONS
char* readShaderSource(const char* shaderFile) {
	FILE* fp;
	fopen_s(&fp, shaderFile, "rb");

	if (fp == NULL) { return NULL; }

	fseek(fp, 0L, SEEK_END);
	long size = ftell(fp);

	fseek(fp, 0L, SEEK_SET);
	char* buf = new char[size + 1];
	fread(buf, 1, size, fp);
	buf[size] = '\0';

	fclose(fp);

	return buf;
}


static void AddShader(GLuint ShaderProgram, const char* pShaderText, GLenum ShaderType)
{
	// create a shader object
	GLuint ShaderObj = glCreateShader(ShaderType);

	if (ShaderObj == 0) {
		std::cerr << "Error creating shader..." << std::endl;
		std::cerr << "Press enter/return to exit..." << std::endl;
		std::cin.get();
		exit(1);
	}
	const char* pShaderSource = readShaderSource(pShaderText);

	// Bind the source code to the shader, this happens before compilation
	glShaderSource(ShaderObj, 1, (const GLchar**)&pShaderSource, NULL);
	// compile the shader and check for errors
	glCompileShader(ShaderObj);
	GLint success;
	// check for shader related errors using glGetShaderiv
	glGetShaderiv(ShaderObj, GL_COMPILE_STATUS, &success);
	if (!success) {
		GLchar InfoLog[1024] = { '\0' };
		glGetShaderInfoLog(ShaderObj, 1024, NULL, InfoLog);
		std::cerr << "Error compiling "
			<< (ShaderType == GL_VERTEX_SHADER ? "vertex" : "fragment")
			<< " shader program: " << InfoLog << std::endl;
		std::cerr << "Press enter/return to exit..." << std::endl;
		std::cin.get();
		exit(1);
	}
	// Attach the compiled shader object to the program object
	glAttachShader(ShaderProgram, ShaderObj);
}

GLuint CompileShaders()
{
	//Start the process of setting up our shaders by creating a program ID
	//Note: we will link all the shaders together into this ID
	shaderProgramID = glCreateProgram();
	if (shaderProgramID == 0) {
		std::cerr << "Error creating shader program..." << std::endl;
		std::cerr << "Press enter/return to exit..." << std::endl;
		std::cin.get();
		exit(1);
	}

	// Create two shader objects, one for the vertex, and one for the fragment shader
	AddShader(shaderProgramID, "simpleVertexShader.txt", GL_VERTEX_SHADER);
	AddShader(shaderProgramID, "simpleFragmentShader.txt", GL_FRAGMENT_SHADER);

	GLint Success = 0;
	GLchar ErrorLog[1024] = { '\0' };
	// After compiling all shader objects and attaching them to the program, we can finally link it
	glLinkProgram(shaderProgramID);
	// check for program related errors using glGetProgramiv
	glGetProgramiv(shaderProgramID, GL_LINK_STATUS, &Success);
	if (Success == 0) {
		glGetProgramInfoLog(shaderProgramID, sizeof(ErrorLog), NULL, ErrorLog);
		std::cerr << "Error linking shader program: " << ErrorLog << std::endl;
		std::cerr << "Press enter/return to exit..." << std::endl;
		std::cin.get();
		exit(1);
	}

	// program has been successfully linked but needs to be validated to check whether the program can execute given the current pipeline state
	glValidateProgram(shaderProgramID);
	// check for program related errors using glGetProgramiv
	glGetProgramiv(shaderProgramID, GL_VALIDATE_STATUS, &Success);
	if (!Success) {
		glGetProgramInfoLog(shaderProgramID, sizeof(ErrorLog), NULL, ErrorLog);
		std::cerr << "Invalid shader program: " << ErrorLog << std::endl;
		std::cerr << "Press enter/return to exit..." << std::endl;
		std::cin.get();
		exit(1);
	}
	// Finally, use the linked shader program
	// Note: this program will stay in effect for all draw calls until you replace it with another or explicitly disable its use
	glUseProgram(shaderProgramID);
	return shaderProgramID;
}
#pragma endregion SHADER_FUNCTIONS

// VBO Functions - click on + to expand
#pragma region VBO_FUNCTIONS
/// <summary>
/// Create and return new Model and generate an object buffer mesh for it.
/// </summary>
/// <param name="modelName">: The name of the new Model</param>
/// <param name="meshName">: The path to the the mesh you wish to use for the new Model</param>
/// <param name="textureName">: The path to the texture you wish to use for the new Model</param>
/// <param name="pind">: The index of this Model's parent. Defaults to -1 for "root" models.</param>
/// <returns>A pointer to the new Model</returns>
Model* generateObjectBufferMesh(const char modelName[], const char meshName[], const char textureName[], unsigned int pind = -1) {
	/*----------------------------------------------------------------------------
	LOAD MESH HERE AND COPY INTO BUFFERS
	----------------------------------------------------------------------------*/

	//Note: you may get an error "vector subscript out of range" if you are using this code for a mesh that doesnt have positions and normals
	//Might be an idea to do a check for that before generating and binding the buffer.

	Model *newModel = new Model(modelName, meshName, textureName, pind);
	cout << "going to load " << modelName << endl;
	ModelData newData = load_mesh(meshName);
	cout << "loaded " << modelName << " successfully" << endl;
	//models[0].data = load_mesh(meshName);
	GLuint nloc1 = glGetAttribLocation(shaderProgramID, "vertex_position");
	GLuint nloc2 = glGetAttribLocation(shaderProgramID, "vertex_normal");
	GLuint nloc3 = glGetAttribLocation(shaderProgramID, "vertex_texture");

	unsigned int vp_vbo;
	glGenBuffers(1, &vp_vbo);
	glBindBuffer(GL_ARRAY_BUFFER, vp_vbo);
	glBufferData(GL_ARRAY_BUFFER, newData.mPointCount * sizeof(vec3), &newData.mVertices[0], GL_STATIC_DRAW);

	unsigned int vn_vbo;
	glGenBuffers(1, &vn_vbo);
	glBindBuffer(GL_ARRAY_BUFFER, vn_vbo);
	glBufferData(GL_ARRAY_BUFFER, newData.mPointCount * sizeof(vec3), &newData.mNormals[0], GL_STATIC_DRAW);

	//	This is for texture coordinates which you don't currently need, so I have commented it out
	//	unsigned int vt_vbo = t_vbo_count;
	//	glGenBuffers (1, &vt_vbo);
	//	glBindBuffer (GL_ARRAY_BUFFER, vt_vbo);
	//	glBufferData (GL_ARRAY_BUFFER, monkey_head_data.mTextureCoords * sizeof (vec2), &monkey_head_data.mTextureCoords[0], GL_STATIC_DRAW);

	unsigned int vao = 0;
	glGenVertexArrays(1, &vao);
	glBindVertexArray(vao);

	glEnableVertexAttribArray(nloc1);
	glBindBuffer(GL_ARRAY_BUFFER, vp_vbo);
	glVertexAttribPointer(nloc1, 3, GL_FLOAT, GL_FALSE, 0, NULL);
	glEnableVertexAttribArray(nloc2);
	glBindBuffer(GL_ARRAY_BUFFER, vn_vbo);
	glVertexAttribPointer(nloc2, 3, GL_FLOAT, GL_FALSE, 0, NULL);


	unsigned int texture;
	glGenTextures(1, &texture);
	glBindTexture(GL_TEXTURE_2D, texture);
	// set the texture wrapping/filtering options (on the currently bound texture object)
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	// load and generate the texture
	int width, height, nrChannels;
	unsigned char* data = stbi_load(textureName, &width, &height, &nrChannels, 0);
	if (data)
	{
		glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, data);
		glGenerateMipmap(GL_TEXTURE_2D);
	}
	else
	{
		std::cout << "Failed to load texture" << std::endl;
	}
	stbi_image_free(data);

	glVertexAttribPointer(nloc3, 2, GL_FLOAT, GL_FALSE, 0, NULL);
	glEnableVertexAttribArray(nloc3);


	//	This is for texture coordinates which you don't currently need, so I have commented it out
	//	glEnableVertexAttribArray (nloc3);
	//	glBindBuffer (GL_ARRAY_BUFFER, vt_vbo);
	//	glVertexAttribPointer (nloc3, 2, GL_FLOAT, GL_FALSE, 0, NULL);

	




	p_vbos.push_back(vp_vbo);
	n_vbos.push_back(vn_vbo);
	//t_vbos.push_back(vt_vbo);
	vaos.push_back(vao);
	newModel->data = newData;
	newModel->position_loc = nloc1;
	newModel->normal_loc = nloc2;
	newModel->texture_loc = nloc3;
	newModel->position_vbo = vp_vbo;
	newModel->normal_vbo = vn_vbo;
	//newModel.texture_vbo = vt_vbo;
	newModel->vao = vao;
	newModel->texture = texture;
	cout << "created " << modelName << " mesh successfully" << endl;
	return newModel;
}


/// <summary>
/// Given a mesh name and a parent index, add a model to the scene.
/// The mesh name must be any existing .obj or .dae file to load into the <code>load_mesh</code> function.
/// </summary>
/// <param name="meshName">The path to the mesh you wish to load</param>
/// <param name="textureName">The path to the texture you wish to load into the mesh</param>
/// <param name="parent_num">The index of the parent in the hierarchy this model has</param>
void addModel(const char name[], const char meshName[], const char textureName[], unsigned int parent_num) {
	Model *m = generateObjectBufferMesh(name, meshName, textureName, parent_num);
	cout << m->name << ", " << m->parent_index << endl;
	models.push_back(m);
	if (parent_num != -1) {
		cout << "setting child-parent relationship for " << name << ", " << m->parent_index << endl;
		m->parent = models[m->parent_index]; // set the current model's parent to the model at its parent index
		models[m->parent_index]->children.push_back(m); // set the current model as the parent model's children
		cout << "set child-parent relationship for " << name << " successfully" << endl;
	}
}
#pragma endregion VBO_FUNCTIONS


void display() {

	// tell GL to only draw onto a pixel if the shape is closer to the viewer
	glEnable(GL_DEPTH_TEST); // enable depth-testing
	glDepthFunc(GL_LESS); // depth-testing interprets a smaller value as "closer"
	glClearColor(0.5f, 0.5f, 1.0f, 1.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glUseProgram(shaderProgramID);


	//Declare your uniform variables that will be used in your shader
	int matrix_location = glGetUniformLocation(shaderProgramID, "model");
	int view_mat_location = glGetUniformLocation(shaderProgramID, "view");
	int proj_mat_location = glGetUniformLocation(shaderProgramID, "proj");

	// Root of the Hierarchy
	mat4 view = identity_mat4();
	mat4 persp_proj = perspective(camera_fov, (float)width / (float)height, 0.1f, 1000.0f);
	glUniformMatrix4fv(proj_mat_location, 1, GL_FALSE, persp_proj.m);
	view = translate(view, vec3(translate_x, translate_y, translate_z));
	view = look_at(camera_pos, camera_pos + camera_target, camera_up);
	glUniformMatrix4fv(view_mat_location, 1, GL_FALSE, view.m);

	// Render each model
	for (int i = 0; i < models.size(); i++) {
		// Set up the matrix
		int matrix_location = glGetUniformLocation(shaderProgramID, "model");
		mat4 model = identity_mat4();
		model = translate(model, vec3(models[i]->translate.x, models[i]->translate.y, models[i]->translate.z)); // translations
		model = rotate_z_deg(rotate_y_deg(rotate_x_deg(model, models[i]->rotate.x), models[i]->rotate.y), models[i]->rotate.z); // rotations
		model = scale(model, vec3(models[i]->scale.x, models[i]->scale.y, models[i]->scale.z)); // scale
		if (models[i]->parent_index != -1) {
			// the first model cannot have a parent
			model = models[models[i]->parent_index]->matrix * model; // parent the current model's matrix. 
		}
		models[i]->matrix = model; // assign this new matrix to the model
		glUniformMatrix4fv(matrix_location, 1, GL_FALSE, model.m); // setup uniform matrix
		glBindVertexArray(models[i]->vao); // bind model's vao
		glBindTexture(GL_TEXTURE_2D, models[i]->texture); // bind model's texture
		glDrawArrays(GL_TRIANGLES, 0, models[i]->data.mPointCount); // draw this matrix
	}

	glutSwapBuffers();
}

float p = 0.0f;
void updateScene() {
	static DWORD last_time = 0;
	DWORD curr_time = timeGetTime();
	if (last_time == 0)
		last_time = curr_time;
	delta = (curr_time - last_time) * 0.001f;
	last_time = curr_time;
	
	// process camera movement
	processCameraMovement();

	// Rotate the model slowly around the y axis at 20 degrees per second
	//models[0].o.trans_z += 2.0f * delta;
	//models[1].o.rot_y = fmodf(m1_rot_y + (20.0f * delta), 360.0f);
	//models[2].o.rot_z = fmodf(m2_rot_z + (30.0f * delta), 360.0f);
	//m3_rot_x = rad2Deg(cos(p));

	// TODO: expand on these alternating cycles (walk cycle maybe)
	// TODO: textures
	p = fmodf(p + (delta * 5.0f), 360.0f);
	models[1]->translate.x += 2.0f * delta;
	models[3]->rotate.x = p * 10;
	models[5]->rotate.z = p * 10;
	models[6]->rotate.y = p * 40;
	//models[1].o.rot_x = rad2Deg(cos(p));
	//models[2].o.rot_x = rad2Deg(-cos(p));

	// Draw the next frame
	glutPostRedisplay();
}


void init()
{
	// Set up the shaders
	shaderProgramID = CompileShaders();
	// load mesh into a vertex buffer array
	addModel("base", MESH_CUBE, TEXTURE_PIC2, -1); // base, doesn't do anything
	addModel("arm_base_1", MESH_CUBOID, TEXTURE_PIC, 0); // moving horizontal base
	addModel("joint_1", MESH_SPHERE, TEXTURE_PIC, 1); // joint 1
	addModel("j1_arm_base", MESH_CUBOID, TEXTURE_PIC2, 2); // rotating vertical base
	addModel("joint_2", MESH_SPHERE, TEXTURE_PIC, 3); // joint 2
	addModel("j2_arm_base", MESH_CUBOID, TEXTURE_PIC, 4); // rotating angled base
	//addModel("j2_monkey", MESH_MONKEY_HEAD, TEXTURE_PIC, 5); // monke
	addModel("j2_bin", MESH_BIN, TEXTURE_WOOD, 5); // bin
	//addModel(MESH_GUITAR, 6);
	models[0]->translate.x = 9;
	models[1]->rotate.x = 90;
	models[3]->translate.y = 3;
	models[3]->rotate.x = -90;
	models[4]->translate.y = 3;
	models[5]->translate.y = 3;
	models[5]->rotate.z = 30;
	models[6]->translate.y = 4;
	cout << models[3]->getChild("joint_2")->parent->name << endl;
	//shaderProgramID = CompileShaders();

	//cout << "Press 'q' to switch to camera mode.\n" << 
	//	"Press 'w' to switch to translation mode.\n" << 
	//	"Press 'e' to switch to scale mode.\n" << 
	//	"Press 'r' to switch to rotation mode.\n\n"
}

// Process camera movement. This should run on every frame.
void processCameraMovement() {
	// If the player cannot fly, their movement should no longer move in the y-axis. However, simply disabling flying creates issues, as the y-axis is still
	// taken into account when normalising movement. To prevent this, we can ignore the y-axis of the camera target changing position before normalising.

	float t_cpos_y = camera_pos.v[1]; // y-pos of camera before updates
	if (FORWARD) {
		if (CAN_FLY) camera_pos += normalise(camera_target) * speed * delta;
		else camera_pos += normalise(vec3(camera_target.v[0], 0, camera_target.v[2])) * speed * delta; 
	}
	if (BACK) {
		if (CAN_FLY) camera_pos -= normalise(camera_target) * speed * delta;
		else camera_pos -= normalise(vec3(camera_target.v[0], 0, camera_target.v[2])) * speed * delta;
	}
	if (LEFT) {
		if (CAN_FLY) camera_pos -= normalise(cross(camera_target, camera_up)) * speed * delta;
		else {
			vec3 c = cross(camera_target, camera_up);
			camera_pos -= normalise(vec3(c.v[0], 0, c.v[2])) * speed * delta;
		}
	}
	if (RIGHT) {
		if (CAN_FLY) camera_pos += normalise(cross(camera_target, camera_up)) * speed * delta;
		else {
			vec3 c = cross(camera_target, camera_up);
			camera_pos += normalise(vec3(c.v[0], 0, c.v[2])) * speed * delta;
		}
	}
	if (UP) {
		camera_pos += vec3(0, speed * delta, 0);
	};
	if (DOWN) {
		camera_pos -= vec3(0, speed * delta, 0);
	};


	speed = SPRINT ? sprintSpeed : baseSpeed;
	if (!CAN_FLY) camera_pos.v[1] = t_cpos_y; // if can't fly, don't change y_pos
}


// Process the mouse moving without button input
void passiveMouseMoved(int x, int y) {
	look_y = wrap(look_y + ((x - width / static_cast<float>(2)) * cam_sensitivity_x), 0, 360);
	look_x = clamp(look_x + ((y - height / static_cast<float>(2)) * -cam_sensitivity_y), -89, 89);
	//look_z = sin(look_y * ONE_DEG_IN_RAD) * cos(look_x * ONE_DEG_IN_RAD);
	glutWarpPointer(width / 2, height / 2); // prevent mouse from leaving the centre of the screen

	camera_target = vec3(cos(deg2Rad(look_y)) * cos(deg2Rad(look_x)), sin(deg2Rad(look_x)), sin(deg2Rad(look_y)) * cos(deg2Rad(look_x)));
}


void specKeyPress(int key, int x, int y) {

	if (key == GLUT_KEY_SHIFT_L) {DOWN = true;}
}

void specKeyUp(int key, int x, int y) {

	if (key == GLUT_KEY_SHIFT_L) {DOWN = false;}
}

// Function ran on key press
void keypress(unsigned char key, int x, int y) {
	if (key == VK_ESCAPE) {
		cout << endl << endl << "Exiting..." << endl;
		exit(0);
	}

	if (key == 'w') FORWARD = true;
	if (key == 's') BACK = true;
	if (key == 'a') LEFT = true;
	if (key == 'd') RIGHT = true;
	if (key == ' ') UP = true;
	if (key == 'e') SPRINT = true;
	if (key == 'p') CAN_FLY = !CAN_FLY;
}

// Function ran on key release
void keyup(unsigned char key, int x, int y) {
	if (key == 'w') FORWARD = false;
	if (key == 's') BACK = false;
	if (key == 'a') LEFT = false;
	if (key == 'd') RIGHT = false;
	if (key == ' ') UP = false;
	if (key == 'e') SPRINT = false;
}


int main(int argc, char** argv) {

	// Set up the window
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);
	glutInitWindowSize(width, height);
	glutCreateWindow("Hello Triangle");

	// Tell glut where the display function is
	glutIgnoreKeyRepeat(true);
	glutDisplayFunc(display);
	glutIdleFunc(updateScene);
	glutKeyboardFunc(keypress);
	glutKeyboardUpFunc(keyup);
	glutSpecialFunc(specKeyPress);
	glutSpecialUpFunc(specKeyUp);
	glutPassiveMotionFunc(passiveMouseMoved);
	glutSetCursor(GLUT_CURSOR_NONE); // hide cursor

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
