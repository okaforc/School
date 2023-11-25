#pragma once

// Windows includes (For Time, IO, etc.)
#include <windows.h>
#include <mmsystem.h>
#include <iostream>
#include <string>
#include <stdio.h>
#include <math.h>
#include <vector> // STL dynamic memory.

// OpenGL includes
#include <GL/glew.h>
#include <GL/freeglut.h>

// Assimp includes
#include <assimp/cimport.h> // scene importer
#include <assimp/scene.h> // collects data
#include <assimp/postprocess.h> // various extra operations

// Project includes
#include "maths_funcs.h"
#include "model.h"

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

int width = 800;
int height = 600;


vector<Model> modelsb;
#pragma region SimpleTypes

//typedef struct
//{
//	size_t mPointCount = 0;
//	std::vector<vec3> mVertices;
//	std::vector<vec3> mNormals;
//	std::vector<vec2> mTextureCoords;
//} ModelData;
//
//typedef struct {
//	mat4 matrix; // model matrix
//	GLfloat trans_x = 5.0f;
//	GLfloat trans_y = 0.0f;
//	GLfloat trans_z = 0.0f;
//	GLfloat rot_x = 0.0f;
//	GLfloat rot_y = 0.0f;
//	GLfloat rot_z = 0.0f;
//	GLfloat scale_x = 1.0f;
//	GLfloat scale_y = 1.0f;
//	GLfloat scale_z = 1.0f;
//	unsigned int parent; // parent index in model
//} Object;
//
//typedef struct
//{
//	//string name;
//	ModelData data; // model data
//	Object o;
//	GLuint position_loc; // position attribute location
//	GLuint normal_loc; // normal attribute location
//	GLuint texture_loc; // texture attribute location
//	unsigned int position_vbo; // position vbo
//	unsigned int normal_vbo; // normal vbo
//	unsigned int texture_vbo; // texture vbo
//	unsigned int vao; // model vao
//} Model;


std::vector<unsigned int> p_vbos;
std::vector<unsigned int> n_vbos;
std::vector<unsigned int> t_vbos;
std::vector<unsigned int> vaos;
#pragma endregion SimpleTypes

using namespace std;
GLuint shaderProgramID;

//ModelData mesh_data;
//unsigned int mesh_vao = 0;

//GLuint loc1, loc2, loc3;

// list of models in scene
vector<Model*> models;


//GLfloat rotate_y = 0.0f;


//GLfloat max_rotate = 360.0f;
//GLfloat scale_x = 1.0f;
//GLfloat scale_y = 1.0f;
//GLfloat scale_z = 1.0f;
//GLfloat camera_x = 0.0f;
//GLfloat camera_y = 0.0f;
//GLfloat camera_z = 0.0f;

// Camera field of view
GLfloat camera_fov = 45.0f;

// Camera position
GLfloat translate_x = 0.0f;
GLfloat translate_y = 0.0f;
GLfloat translate_z = 3.0f;

// Camera rotation
GLfloat look_x = 0.0f; // pitch
GLfloat look_y = 0.0f; // yaw
GLfloat look_z = -1.0f; // roll

// Global up direction
vec3 up = vec3(0.0f, 1.0f, 0.0f);

// Camera position
vec3 camera_pos = vec3(translate_x, translate_y, translate_z);

// Camera target (where the camera is rotating)
vec3 camera_target = vec3(look_x, look_y, look_z);

// Camera direction (where the camera is looking)
vec3 camera_dir = normalise(camera_pos - camera_target);

// Camera front (where the camera is pointing)
vec3 camera_front = normalise(camera_dir);

// Camera right (x axis of camera
vec3 camera_right = normalise(cross(up, camera_dir));

// Camera up (y axis of camera
vec3 camera_up = cross(camera_dir, camera_right);

// Mouse position
vec2 mouse_pos = vec2(0, 0);

// Camera sensitivity (x axis)
GLfloat cam_sensitivity_x = 0.05f;

// Camera sensitivity (y axis)
GLfloat cam_sensitivity_y = 0.05f;

// Walking speed (movement speed)
GLfloat baseSpeed = 5.0f;

// Sprinting speed (movement speed)
GLfloat sprintSpeed = 25.0f;

// Camera speed (movement speed)
GLfloat speed = baseSpeed;

// View matrix
//mat4 view = identity_mat4();

// Perepective projection matrix
//mat4 persp_proj = identity_mat4();

// Model matrix
//mat4 model = identity_mat4();

// Keypress binds
bool DOWN = false, UP = false, RIGHT = false, LEFT = false, FORWARD = false, BACK = false, SPRINT = false;

// Can the player fly (move on the y-axis)?
bool CAN_FLY = true;

// time between frames
float delta = 0.0f;

// model transform
GLfloat m0_trans_x = 0.0f;
GLfloat m0_trans_y = 0.0f;
GLfloat m0_trans_z = 0.0f;
GLfloat m0_rot_x = 0.0f;
GLfloat m0_rot_y = 0.0f;
GLfloat m0_rot_z = 0.0f;

GLfloat m1_trans_x = 0.0f;
GLfloat m1_trans_y = 0.0f;
GLfloat m1_trans_z = 9.0f;
GLfloat m1_rot_x = 0.0f;
GLfloat m1_rot_y = 3.0f;
GLfloat m1_rot_z = 0.0f;

GLfloat m2_trans_x = 0.0f;
GLfloat m2_trans_y = 3.0f;
GLfloat m2_trans_z = 9.0f;
GLfloat m2_rot_x = 0.0f;
GLfloat m2_rot_y = 0.0f;
GLfloat m2_rot_z = 9.0f;
GLfloat m2_scale_x = 1.0f;
GLfloat m2_scale_y = 2.0f;
GLfloat m2_scale_z = 1.0f;

GLfloat m3_trans_x = 10.0f;
GLfloat m3_trans_y = -3.0f;
GLfloat m3_trans_z = 0.0f;
GLfloat m3_rot_x = 0.0f;
GLfloat m3_rot_y = 10.0f;
GLfloat m3_rot_z = 0.0f;

GLfloat m4_trans_x = -1.5f;
GLfloat m4_trans_y = -3.0f;
GLfloat m4_trans_z = 0.0f;
GLfloat m4_rot_x = 0.0f;
GLfloat m4_rot_y = 10.0f;
GLfloat m4_rot_z = 0.0f;

GLfloat m5_trans_x = 1.5f;
GLfloat m5_trans_y = -3.0f;
GLfloat m5_trans_z = 0.0f;
GLfloat m5_rot_x = 0.0f;
GLfloat m5_rot_y = 10.0f;
GLfloat m5_rot_z = 0.0f;

//// struct containing ids for keyboard movement.
//struct {
//	int rotation = 1;
//	int translation = 2;
//	int scale = 3;
//	int camera = 0;
//} MOVE_MODES;
//
//// actual keyboard movement setting.
//int KEYMODE = MOVE_MODES.camera;

// Wrap a value between min and max. If val is greater than max, it will wraparound to min and begin climbing from there, and vice versa.
float wrap(float val, float min, float max) {
	int range = max - min;
	if (val < min) val += range * ((min - val) / range + 1);
	return fmod(min + (val - min), range);
}

// Clamp a value between a minimum and maximum so that val cannot be greater than max or smaller than min.
float clamp(float val, float min, float max) {
	if (val < min) val = min;
	if (val > max) val = max;
	return val;
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
	return vec2((v.v[0] - width / 2) / width * 2, (v.v[1] - height / 2) / height * -2);
}

void printMatrix(mat4 mm) {
	std::cout << mm.m[0] << ", " << mm.m[1] << ", " << mm.m[2] << ", " << mm.m[3] << std::endl;
	std::cout << mm.m[4] << ", " << mm.m[5] << ", " << mm.m[6] << ", " << mm.m[7] << std::endl;
	std::cout << mm.m[8] << ", " << mm.m[9] << ", " << mm.m[10] << ", " << mm.m[11] << std::endl;
	std::cout << mm.m[12] << ", " << mm.m[13] << ", " << mm.m[14] << ", " << mm.m[15] << std::endl;
}

// Convert degrees to radians
float deg2Rad(float val) {
	return val * ONE_DEG_IN_RAD;
}

// Convert radians to degrees
float rad2Deg(float val) {
	return val * ONE_RAD_IN_DEG;
}

void processCameraMovement();



//void generateObjectBufferMesh(const char meshName[]) {
//	/*----------------------------------------------------------------------------
//	LOAD MESH HERE AND COPY INTO BUFFERS
//	----------------------------------------------------------------------------*/
//
//	//Note: you may get an error "vector subscript out of range" if you are using this code for a mesh that doesnt have positions and normals
//	//Might be an idea to do a check for that before generating and binding the buffer.
//
//	mesh_data = load_mesh(meshName);
//	unsigned int vp_vbo = 0;
//	loc1 = glGetAttribLocation(shaderProgramID, "vertex_position");
//	loc2 = glGetAttribLocation(shaderProgramID, "vertex_normal");
//	loc3 = glGetAttribLocation(shaderProgramID, "vertex_texture");
//
//	glGenBuffers(1, &vp_vbo);
//	glBindBuffer(GL_ARRAY_BUFFER, vp_vbo);
//	glBufferData(GL_ARRAY_BUFFER, mesh_data.mPointCount * sizeof(vec3), &mesh_data.mVertices[0], GL_STATIC_DRAW);
//	unsigned int vn_vbo = 0;
//	glGenBuffers(1, &vn_vbo);
//	glBindBuffer(GL_ARRAY_BUFFER, vn_vbo);
//	glBufferData(GL_ARRAY_BUFFER, mesh_data.mPointCount * sizeof(vec3), &mesh_data.mNormals[0], GL_STATIC_DRAW);
//
//	//	This is for texture coordinates which you don't currently need, so I have commented it out
//	//	unsigned int vt_vbo = 0;
//	//	glGenBuffers (1, &vt_vbo);
//	//	glBindBuffer (GL_ARRAY_BUFFER, vt_vbo);
//	//	glBufferData (GL_ARRAY_BUFFER, monkey_head_data.mTextureCoords * sizeof (vec2), &monkey_head_data.mTextureCoords[0], GL_STATIC_DRAW);
//
//	unsigned int vao = 0;
//	glBindVertexArray(vao);
//
//	glEnableVertexAttribArray(loc1);
//	glBindBuffer(GL_ARRAY_BUFFER, vp_vbo);
//	glVertexAttribPointer(loc1, 3, GL_FLOAT, GL_FALSE, 0, NULL);
//	glEnableVertexAttribArray(loc2);
//	glBindBuffer(GL_ARRAY_BUFFER, vn_vbo);
//	glVertexAttribPointer(loc2, 3, GL_FLOAT, GL_FALSE, 0, NULL);
//
//	//	This is for texture coordinates which you don't currently need, so I have commented it out
//	//	glEnableVertexAttribArray (loc3);
//	//	glBindBuffer (GL_ARRAY_BUFFER, vt_vbo);
//	//	glVertexAttribPointer (loc3, 2, GL_FLOAT, GL_FALSE, 0, NULL);
//}