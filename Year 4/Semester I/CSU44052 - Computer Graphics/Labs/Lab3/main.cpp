#pragma warning(disable : 5208)


#include "main.h"
using namespace std;
float p = 0.0f;
float tp = 0.0f;
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


vector<vec3> cubePositions = {
	vec3(0.0f,  2.0f,  0.0f),
	vec3(2.0f,  5.0f, -15.0f),
	//vec3(-1.5f, -2.2f, -2.5f),
	vec3(0),
	vec3(-3.8f, -2.0f, -12.3f),
	vec3(2.4f, -0.4f, -3.5f),
	vec3(-1.7f,  3.0f, -7.5f),
	vec3(1.3f, -2.0f, -2.5f),
	vec3(1.5f,  2.0f, -2.5f),
	vec3(1.5f,  0.2f, -1.5f),
	vec3(-1.3f,  1.0f, -1.5f)
};
vector<vec3> pointLightPositions = {
	vec3(1.3f, -2.0f, -2.5f),
	vec3(10.6f,  2.3f, -2.4f),
	vec3(1.3f,  5.1f, -1.1f),
	vec3(0.0f,  1.0f, -3.0f)
};


void display() {

	// tell GL to only draw onto a pixel if the shape is closer to the viewer
	glEnable(GL_DEPTH_TEST); // enable depth-testing
	glEnable(GL_BLEND); // enable depth-testing
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glDepthFunc(GL_LESS); // depth-testing interprets a smaller value as "closer"
	glClearColor(0.1f, 0.1f, 0.3f, 1.0f);
	//glClearColor(1, 1, 1, 1.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	mat4 view = camera.getViewMatrix();
	mat4 persp_proj = perspective(camera.FOV, (float)SM::width / (float)SM::height, 0.1f, 1000.0f);

	// Static meshes
	shaders["base"]->use();
#pragma region BASE_LIGHTS
	shaders["base"]->setVec3("viewPos", camera.pos);
	shaders["base"]->setFloat("material.shininess", 64.0f);
	shaders["base"]->setInt("material.diffuse", 0);
	shaders["base"]->setInt("material.specular", 1);

	shaders["base"]->setVec3("dirLight.direction", -0.2f, -1.0f, -0.3f);
	shaders["base"]->setVec3("dirLight.ambient", vec3(0.5f));
	shaders["base"]->setVec3("dirLight.diffuse", vec3(0.8f));
	shaders["base"]->setVec3("dirLight.specular", 0.5f, 0.5f, 0.5f);

	// point light 1
	shaders["base"]->setVec3("pointLights[0].position", pointLightPositions[0] * spread);
	shaders["base"]->setVec3("pointLights[0].ambient", vec3(0.05f));
	shaders["base"]->setVec3("pointLights[0].diffuse", 0.0f, 0.8f, 0.8f);
	shaders["base"]->setVec3("pointLights[0].specular", 1.0f, 1.0f, 1.0f);
	shaders["base"]->setFloat("pointLights[0].constant", 1.0f);
	shaders["base"]->setFloat("pointLights[0].linear", 0.9f);
	shaders["base"]->setFloat("pointLights[0].quadratic", 0.032f);
	// point light 2
	shaders["base"]->setVec3("pointLights[1].position", pointLightPositions[1] * spread);
	shaders["base"]->setVec3("pointLights[1].ambient", vec3(0.05f));
	shaders["base"]->setVec3("pointLights[1].diffuse", 0.0f, 0.8f, 0.8f);
	shaders["base"]->setVec3("pointLights[1].specular", 1.0f, 1.0f, 1.0f);
	shaders["base"]->setFloat("pointLights[1].constant", 1.0f);
	shaders["base"]->setFloat("pointLights[1].linear", 0.9f);
	shaders["base"]->setFloat("pointLights[1].quadratic", 0.032f);
	// point light 3
	shaders["base"]->setVec3("pointLights[2].position", pointLightPositions[2] * spread);
	shaders["base"]->setVec3("pointLights[2].ambient", vec3(0.05f));
	shaders["base"]->setVec3("pointLights[2].diffuse", 0.0f, 0.8f, 0.8f);
	shaders["base"]->setVec3("pointLights[2].specular", 1.0f, 1.0f, 1.0f);
	shaders["base"]->setFloat("pointLights[2].constant", 1.0f);
	shaders["base"]->setFloat("pointLights[2].linear", 0.9f);
	shaders["base"]->setFloat("pointLights[2].quadratic", 0.032f);
	// point light 4
	shaders["base"]->setVec3("pointLights[3].position", pointLightPositions[3] * spread);
	shaders["base"]->setVec3("pointLights[3].ambient", vec3(0.05f));
	shaders["base"]->setVec3("pointLights[3].diffuse", 0.0f, 0.8f, 0.8f);
	shaders["base"]->setVec3("pointLights[3].specular", 1.0f, 1.0f, 1.0f);
	shaders["base"]->setFloat("pointLights[3].constant", 1.0f);
	shaders["base"]->setFloat("pointLights[3].linear", 0.9f);
	shaders["base"]->setFloat("pointLights[3].quadratic", 0.032f);

	shaders["base"]->setVec3("spotLight.position", camera.pos);
	shaders["base"]->setVec3("spotLight.direction", camera.front);
	shaders["base"]->setVec3("spotLight.ambient", vec3(0.05f));
	shaders["base"]->setVec3("spotLight.diffuse", vec3(0.8f));
	shaders["base"]->setVec3("spotLight.specular", vec3(1.0f));
	shaders["base"]->setFloat("spotLight.constant", 1.0f);
	shaders["base"]->setFloat("spotLight.linear", 0.09f);
	shaders["base"]->setFloat("spotLight.quadratic", 0.032f);
	shaders["base"]->setFloat("spotLight.cutOff", cos(Util::deg2Rad(2.5f)));
	shaders["base"]->setVec3("spotLight.outerCutOff", cos(Util::deg2Rad(3.0f)));
#pragma endregion
	shaders["base"]->setMat4("view", view);
	shaders["base"]->setMat4("proj", persp_proj);
	// Transform each instance
	const unsigned int numInstances = 1000;
	mat4 models[numInstances];
	for (int i = 0; i < numInstances; i++) {
		mat4 model = identity_mat4();
		//model = translate(model, translations[i] + vec3(p * (fmodf(rand(), 0.03f)), 0, p / 2.0f));
		if (countdown <= 0.1f) {
			directions[i] = fmodf(rand(), 360.0f);
			if (i == numInstances - 1) countdown = countdown_max;
		}
		//cout << i << ": " << directions[i].v[0] << ", " << directions[i].v[1] << ", " << directions[i].v[2] << endl;
		//model = rotate_x_deg(model, directions[i].v[0]);
		//model = rotate_y_deg(model, directions[i].v[1]);
		//model = rotate_z_deg(model, directions[i].v[2]);
		vec3 htd = heading_to_direction(directions[i]);
		//model = rotate_x_deg(model, htd.v[0]*360.0f);
		//model = rotate_y_deg(model, htd.v[1]*360.0f);
		model = rotate_z_deg(model, htd.v[2]*360.0f);
		model = translate(model, translations[i]);
		translations[i] += normalise(htd) * 5 * SM::delta;
		models[i] = model;
	}
	//meshes[0]->render(numInstances, models);
	meshes[0]->render(models[0]);

	mat4 model = identity_mat4();
	meshes[1]->render(model);

	// Bonemeshes (skinned meshes)
	shaders["bonemesh"]->use();
#pragma region BMESH_LIGHTS
	shaders["bonemesh"]->setVec3("viewPos", camera.pos);
	shaders["bonemesh"]->setFloat("material.shininess", 64.0f);
	shaders["bonemesh"]->setInt("material.diffuse", 0);
	shaders["bonemesh"]->setInt("material.specular", 1);

	shaders["bonemesh"]->setVec3("dirLight.direction", -0.2f, -1.0f, -0.3f);
	shaders["bonemesh"]->setVec3("dirLight.ambient", vec3(0.5f));
	shaders["bonemesh"]->setVec3("dirLight.diffuse", vec3(0.8f));
	shaders["bonemesh"]->setVec3("dirLight.specular", vec3(1));

	// point light 1
	shaders["bonemesh"]->setVec3("pointLights[0].position", pointLightPositions[0] * spread);
	shaders["bonemesh"]->setVec3("pointLights[0].ambient", vec3(0.05f));
	shaders["bonemesh"]->setVec3("pointLights[0].diffuse", 0.0f, 0.8f, 0.8f);
	shaders["bonemesh"]->setVec3("pointLights[0].specular", 1.0f, 1.0f, 1.0f);
	shaders["bonemesh"]->setFloat("pointLights[0].constant", 1.0f);
	shaders["bonemesh"]->setFloat("pointLights[0].linear", 0.9f);
	shaders["bonemesh"]->setFloat("pointLights[0].quadratic", 0.032f);
	// point light 2
	shaders["bonemesh"]->setVec3("pointLights[1].position", pointLightPositions[1] * spread);
	shaders["bonemesh"]->setVec3("pointLights[1].ambient", vec3(0.05f));
	shaders["bonemesh"]->setVec3("pointLights[1].diffuse", 0.0f, 0.8f, 0.8f);
	shaders["bonemesh"]->setVec3("pointLights[1].specular", 1.0f, 1.0f, 1.0f);
	shaders["bonemesh"]->setFloat("pointLights[1].constant", 1.0f);
	shaders["bonemesh"]->setFloat("pointLights[1].linear", 0.9f);
	shaders["bonemesh"]->setFloat("pointLights[1].quadratic", 0.032f);
	// point light 3
	shaders["bonemesh"]->setVec3("pointLights[2].position", pointLightPositions[2] * spread);
	shaders["bonemesh"]->setVec3("pointLights[2].ambient", vec3(0.05f));
	shaders["bonemesh"]->setVec3("pointLights[2].diffuse", 0.0f, 0.8f, 0.8f);
	shaders["bonemesh"]->setVec3("pointLights[2].specular", 1.0f, 1.0f, 1.0f);
	shaders["bonemesh"]->setFloat("pointLights[2].constant", 1.0f);
	shaders["bonemesh"]->setFloat("pointLights[2].linear", 0.9f);
	shaders["bonemesh"]->setFloat("pointLights[2].quadratic", 0.032f);
	// point light 4
	shaders["bonemesh"]->setVec3("pointLights[3].position", pointLightPositions[3] * spread);
	shaders["bonemesh"]->setVec3("pointLights[3].ambient", vec3(0.05f));
	shaders["bonemesh"]->setVec3("pointLights[3].diffuse", 0.0f, 0.8f, 0.8f);
	shaders["bonemesh"]->setVec3("pointLights[3].specular", 1.0f, 1.0f, 1.0f);
	shaders["bonemesh"]->setFloat("pointLights[3].constant", 1.0f);
	shaders["bonemesh"]->setFloat("pointLights[3].linear", 0.9f);
	shaders["bonemesh"]->setFloat("pointLights[3].quadratic", 0.032f);

	shaders["bonemesh"]->setVec3("spotLight.position", camera.pos);
	shaders["bonemesh"]->setVec3("spotLight.direction", camera.front);
	shaders["bonemesh"]->setVec3("spotLight.ambient", vec3(0.05f));
	shaders["bonemesh"]->setVec3("spotLight.diffuse", vec3(0.8f));
	shaders["bonemesh"]->setVec3("spotLight.specular", vec3(1.0f));
	shaders["bonemesh"]->setFloat("spotLight.constant", 1.0f);
	shaders["bonemesh"]->setFloat("spotLight.linear", 0.09f);
	shaders["bonemesh"]->setFloat("spotLight.quadratic", 0.032f);
	shaders["bonemesh"]->setFloat("spotLight.cutOff", cos(Util::deg2Rad(12.5f)));
	shaders["bonemesh"]->setVec3("spotLight.outerCutOff", cos(Util::deg2Rad(13.0f)));
#pragma endregion 
	shaders["bonemesh"]->setMat4("view", view);
	shaders["bonemesh"]->setMat4("proj", persp_proj);

	
	float animTime = ((float)(timeGetTime() - startTime)) / 1000.0f;
	vector<aiMatrix4x4> trans;
	gmeshes[0]->getBoneTransforms(animTime, trans);
	for (int i = 0; i < trans.size(); i++) {
		glm::mat4 t = Util::aiToGLM(&trans[i]);
		shaders["bonemesh"]->setMat4GLM("glBones[" + to_string(i) + "]", t);
	}
	
	// instancing skinned meshes
	unsigned int nSInstances = 50;
	vector<mat4> pos;
	vector<mat4> roots;
	pos.resize(nSInstances);
	roots.resize(nSInstances);
	bool f = false;
	for (int i = 0; i < nSInstances; i++) {
		mat4 root = identity_mat4();
		root = scale(root, vec3(0.25f));
		if (countdown2 <= 0.1f) {
			dirs[i] = fmodf(rand(), 360.0f);
			if (i == nSInstances - 1) countdown2 = countdown_max2;
			if (!f) { cout << dirs[i] << endl; f = true; }
		}

		vec3 htd = heading_to_direction(dirs[i]); // convert rotation angle to vec3
		root = root * rotate_mat(Util::deg2Rad(90.0f), 1, 0, 0); // orient mesh
		root = root * rotate_mat(Util::deg2Rad(360.0f - dirs[i]), 0, 0, 1); // (try to) rotate mesh in facing direction
		translations2[i] -= normalise(htd) * 5 * SM::delta; // translate mesh at speed of 5 somethings/whatever
		root = translate(root, translations2[i]); // i don't know why the z-axis rotation and translation have to be negated, but it works

		roots[i] = root;
		pos[i] = identity_mat4(); // keep at origin and translate root
	}
	gmeshes[0]->render(nSInstances, pos.data(), roots.data());
	//gmeshes[0]->render(1, pos.data(), roots.data());

	mat4 model2 = identity_mat4();
	bmeshes[0]->render(model2);


	// Skybox
	glDepthFunc(GL_LEQUAL);
	shaders["skybox"]->use();
	//mat4 viewsb = mat4(mat3(camera.getViewMatrix()));
	//shaders["skybox"]->setMat4("view", viewsb);
	glm::mat4 viewsb = glm::mat4(glm::mat3(camera.getViewMatrixGLM()));
	shaders["skybox"]->setMat4GLM("view", viewsb);
	shaders["skybox"]->setMat4("proj", persp_proj);
	cubemap.render();
	glDepthFunc(GL_LESS);

	glutSwapBuffers();
}


void updateScene() {
	SM::updateDelta();
	//countdown = Help::wrap(countdown - SM::delta, 0.0f, 5.0f);
	countdown -= SM::delta;
	countdown2 -= SM::delta;

	camera.processMovement();
	p = fmodf(p + (SM::delta * 5.0f), 360.0f);
	tp += p;
	// Draw the next frame
	glutPostRedisplay();
}


void init()
{
	srand(time(nullptr));

	// random directions to move cubes in
	float offset = 1.0f;
	for (int z = -10; z < 10; z += 2) {
		for (int y = -10; y < 10; y += 2) {
			for (int x = -10; x < 10; x += 2) {
				vec3 translation;
				translation.v[0] = (float)x + offset * (rand() % spread);
				translation.v[1] = (float)y + offset * (rand() % spread);
				translation.v[2] = (float)z + offset * (rand() % spread);
				translations.push_back(translation);
				translations2.push_back(vec3(translation.v[0], 0, translation.v[2]));

				float d = fmodf(rand(), 360.0f);;
				directions.push_back(d);
			}
		}
	}
	dirs.resize(1000);

	// Create shaders to use
	Shader* s = new Shader("base", vert_main, frag_main);
	s->use();
	shaders[s->name] = s;

	Shader* s2 = new Shader("skybox", vert_sb, frag_sb);
	shaders[s2->name] = s2;
	s2->use();
	shaders["skybox"]->setInt("skybox", 0);

	Shader* s3 = new Shader("bonemesh", vert_bmesh, frag_bmesh);
	shaders[s3->name] = s3;
	s3->use();


	// Load meshes to be used
	Mesh* m = new Mesh(); // static mesh
	if (!m->loadMesh(MESH_TESTCUBE)) {
		cout << "\n\nfailed to load mesh :(\n";
	}
	meshes.push_back(m);

	Mesh* m2 = new Mesh(); // static mesh
	if (!m2->loadMesh(MESH_CONE)) {
		cout << "\n\nfailed to load mesh :(\n";
	}
	meshes.push_back(m2);
	
	BoneMesh* bm = new BoneMesh(); // bone mesh
	if (!bm->loadMesh(BMESH_ZOMBIE)) {
		cout << "\n\nfailed to load mesh :(\n";
	}
	bmeshes.push_back(bm);
	
	BoneMesh* bm2 = new BoneMesh(); // bone mesh
	if (!bm2->loadMesh(BMESH_GUY)) {
		cout << "\n\nfailed to load mesh :(\n";
	}
	gmeshes.push_back(bm2);
	
	// Load cubemap to be used
	cubemap = Cubemap(); // cubemap (skybox)
	cubemap.loadCubemap(cubemap_faces);

	startTime = timeGetTime();
}

// Process the mouse moving without button input
void passiveMouseMoved(int x, int y) {
	camera.processView(x, y);
}

void specKeyPressed(int key, int x, int y) {
	if (key == GLUT_KEY_SHIFT_L) { camera.DOWN = true; }
	if (key == GLUT_KEY_LEFT) { tx -= 1; }
	if (key == GLUT_KEY_RIGHT) { tx += 1; }
}

void specKeyReleased(int key, int x, int y) {
	if (key == GLUT_KEY_SHIFT_L) { camera.DOWN = false; }
}

// Function ran on key press
void keyPressed(unsigned char key, int x, int y) {
	if (key == VK_ESCAPE) {
		cout << endl << endl << "Exiting..." << endl;
		exit(0);
	}

	if (key == ' ') camera.UP = true;
	if (key == 'w' || key == 'W') camera.FORWARD = true;
	if (key == 's' || key == 'S') camera.BACK = true;
	if (key == 'a' || key == 'A') camera.LEFT = true;
	if (key == 'd' || key == 'D') camera.RIGHT = true;
	if (key == 'e' || key == 'E') camera.SPRINT = true;
	if (key == 'p' || key == 'P') camera.CAN_FLY = !camera.CAN_FLY;
	if (key == 'o' || key == 'O') camera.CAN_FALL = !camera.CAN_FALL;
}

// Function ran on key release
void keyReleased(unsigned char key, int x, int y) {
	if (key == ' ') camera.UP = false;
	if (key == 'w' || key == 'W') camera.FORWARD = false;
	if (key == 's' || key == 'S') camera.BACK = false;
	if (key == 'a' || key == 'A') camera.LEFT = false;
	if (key == 'd' || key == 'D') camera.RIGHT = false;
	if (key == 'e' || key == 'E') camera.SPRINT = false;
}


int main(int argc, char** argv) {

	// Set up the window
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);
	glutInitWindowSize(SM::width, SM::height);
	//glutInitWindowSize(glutGet(GLUT_SCREEN_WIDTH), glutGet(GLUT_SCREEN_HEIGHT));
	//glutCreateWindow("Hello Triangle");
	glutCreateWindow("Lab 3");

	//glutFullScreen();
	//SM::width = GLUT_SCREEN_WIDTH;
	//SM::height = GLUT_SCREEN_HEIGHT;

	// Tell glut where the display function is
	glutIgnoreKeyRepeat(true);
	glutDisplayFunc(display);
	glutIdleFunc(updateScene);
	glutKeyboardFunc(keyPressed);
	glutKeyboardUpFunc(keyReleased);
	glutSpecialFunc(specKeyPressed);
	glutSpecialUpFunc(specKeyReleased);
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

