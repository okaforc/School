#pragma once
#include <map>
#include <vector> // STL dynamic memory.
#include <string>
#include <fstream>
#include <iostream>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include "maths_funcs.h"

#include <GL/glew.h>
#include <GL/freeglut.h>
#include <GLM/mat4x4.hpp>

#include <assimp/scene.h> // collects data
#include <assimp/postprocess.h> // various extra operations

#define ARRAY_SIZE(a) (sizeof(a)/sizeof(a[0]))

class Util
{
public:
	static std::string readFile(const char* path);
	static float wrap(float val, float min, float max);
	static float clamp(float val, float min, float max);
	static float deg2Rad(float val);
	static float rad2Deg(float val);
	static void printVec3(vec3);

	static aiMatrix4x4 toAIM4(const mat4&);
	static glm::mat4 aiToGLM(aiMatrix4x4*);


	struct Tween {
		
	};
#define LINEAR 0
#define SINE 1
#define CUBIC 2

	static float lerp(float val, float min, float max, float delta);
};

