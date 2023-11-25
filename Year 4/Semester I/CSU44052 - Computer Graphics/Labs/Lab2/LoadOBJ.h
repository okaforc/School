#pragma once

#include <GLM/vec3.hpp>
#include <GLM/vec2.hpp>
#include <GL/glew.h>
#include <GL/freeglut.h>
#include <iostream>
#include <vector>
#include <assimp/Importer.hpp>      // C++ importer interface
#include <assimp/scene.h>           // Output data structure
#include <assimp/postprocess.h>     // Post processing flags
class LoadOBJ
{
};
using namespace std;
using namespace glm;
static bool loadOBJ(const char* path, vector<vec3>& out_vertices, vector<vec2>& out_uvs, vector<vec3>& out_normals);
bool loadAssImp(const char* path, vector<unsigned short>& indices, vector<vec3>& vertices, vector<vec2>& uvs, vector<vec3>& normals);