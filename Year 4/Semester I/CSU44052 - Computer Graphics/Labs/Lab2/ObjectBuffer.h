#pragma once
#include <GLM/vec3.hpp>
#include <GLM/vec2.hpp>
#include <GL/glew.h>
#include <GL/freeglut.h>
#include <iostream>
#include <vector>
#include <glm/mat3x3.hpp>
class ObjectBuffer
{
};
using namespace std;
using namespace glm;
// Macro for indexing vertex buffer
#define BUFFER_OFFSET(i) ((char *)NULL + (i))
void generateObjectBuffer(GLuint& VBO, GLfloat vertices[], GLfloat colors[]);
void generateObjectBuffer(GLuint& VBO, mat3 vertices, GLfloat colors[]);
void linkCurrentBuffertoShader(GLuint& VAO, GLuint& VBO, GLuint shaderProgramID);