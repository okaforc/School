#pragma once
#include "sm.h"

#define NOMINMAX
#include <limits>

// Windows includes (For Time, IO, etc.)
#include <windows.h>
#include <mmsystem.h>
#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <stdio.h>
#include <stddef.h>
#include <math.h>
#include <vector> // STL dynamic memory.

// OpenGL includes
#include <GL/glew.h>
#include <GL/freeglut.h>
#include <glm/glm.hpp>
#include <glm/gtx/string_cast.hpp>

// Assimp includes
#include <assimp/cimport.h> // scene importer
#include <assimp/scene.h> // collects data
#include <assimp/postprocess.h> // various extra operations

// Project includes
#include "maths_funcs.h"
//#include "model.h"
#include "mesh.h"
#include "bonemesh.h"
#include "cubemap.h"
#include "shader.h"
#include "util.h"
#include "camera.h"

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"


#pragma region MESH_NAMES
// Current working meshes
#define MESH_TEST "test.obj"
#define MESH_TESTCUBE "test_cube.obj"
#define MESH_CYLINDER "test_cylinder.obj"
#define MESH_SPIDER "spider.obj"
#define MESH_GROUND "test_ground.obj"
#define MESH_PLANK "wood_plank.obj"
#define MESH_CONE "cone.gltf"
#define MESH_O "o.gltf"

#define BMESH_ZOMBIE "zombie.gltf"
//#define BMESH_ZOMBIE "zombie.gltf"
#define BMESH_GUY "boblampclean.md5mesh"
//#define BMESH_GUY "boblampclean.md5anim"
//#define BMESH_GUY "zombie2.fbx"
//#define BMESH_ZOMBIE "test_cube.obj"
//#define BMESH_ZOMBIE "boblampclean.md5mesh"
//#define BMESH_ZOMBIE "test2.fbx"
//#define BMESH_LAMP "zombie.gltf"
#pragma endregion

std::map<std::string, Shader*> shaders;
const char* vert_main = "../../Shaders/main.vert";
const char* frag_main = "../../Shaders/main.frag";
const char* vert_sb = "../../Shaders/cubemap.vert";
const char* frag_sb = "../../Shaders/cubemap.frag";
const char* vert_bmesh = "../../Shaders/boneMesh.vert";
const char* frag_bmesh = "../../Shaders/boneMesh.frag";

std::vector<Mesh*> meshes;
std::vector<BoneMesh*> bmeshes;
std::vector<BoneMesh*> gmeshes;
Cubemap cubemap;
Camera camera = Camera();

std::vector<vec3> translations;
std::vector<float> directions;
std::vector<vec3> translations2;
std::vector<float> dirs;

std::vector<std::string> cubemap_faces = {
	"../../Models/skybox/right.jpg",
	"../../Models/skybox/left.jpg",
	"../../Models/skybox/top.jpg",
	"../../Models/skybox/bottom.jpg",
	"../../Models/skybox/front.jpg",
	"../../Models/skybox/back.jpg",
};

DWORD startTime;

int spread = 200;
float countdown = 1.0f;
float countdown_max = 2.0f;
float countdown2 = 1.0f;
float countdown_max2 = 2.0f;

float tx = 0, ty = 0, tz = 0;