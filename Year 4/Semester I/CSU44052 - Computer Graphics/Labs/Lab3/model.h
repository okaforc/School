#pragma once
#include <GL/glew.h>
#include <GL/freeglut.h>
#include <stdio.h>
#include <math.h>
#include <vector>
#include <string>
#include "maths_funcs.h"
using namespace std;

typedef struct Translate {
	GLfloat x = 0.0f;
	GLfloat y = 0.0f;
	GLfloat z = 0.0f;
};
typedef struct Rotate {
	GLfloat x = 0.0f;
	GLfloat y = 0.0f;
	GLfloat z = 0.0f;
};
typedef struct Scale {
	GLfloat x = 1.0f;
	GLfloat y = 1.0f;
	GLfloat z = 1.0f;
};

typedef struct Transform {
	Translate translate;
	Rotate rotate;
	Scale scale;
};

typedef struct ModelData {
	size_t mPointCount = 0;
	vector<vec3> mVertices;
	vector<vec3> mNormals;
	vector<vec2> mTextureCoords;
};

class Model
{
public:
	//Transform transform;
	Translate translate; // position
	Rotate rotate; // rotation in degrees
	Scale scale; // scale
	mat4 matrix; // model matrix
	ModelData data; // model data
	unsigned int parent_index; // parent index in hierarchy. -1 if global root model.
	Model *parent; // parent model
	vector<Model*> children; // list of children models
	GLuint position_loc; // position attribute location
	GLuint normal_loc; // normal attribute location
	GLuint texture_loc; // texture attribute location
	unsigned int position_vbo; // position vbo
	unsigned int normal_vbo; // normal vbo
	unsigned int texture_vbo; // texture vbo
	unsigned int vao; // model vao
	unsigned int texture; // model texture
	string mesh_name; // name of mesh for model
	string texture_name; // name of texture for model
	string name;
	
	// functions
	Model();
	Model(string);
	Model(string, string, string, unsigned int);
	void setTranslate(float x, float y, float z);
	void setTranslate(Translate t);
	void addChild(Model*);
	Model* getChild(string);
};

