#pragma once
#pragma warning(disable : 26495)
#include <string>
#include <map>
#include <stdio.h>
#include <stddef.h>
#include <math.h>
#include <vector> // STL dynamic memory.
#include <assert.h> // STL dynamic memory.

#include <assimp/cimport.h> // scene importer
#include <assimp/Importer.hpp>
#include <assimp/scene.h> // collects data
#include <assimp/postprocess.h> // various extra operations

#include <GL/glew.h>
#include <GL/freeglut.h>

#include "util.h"
#include "Texture.h"

#define AI_LOAD_FLAGS aiProcess_Triangulate | aiProcess_PreTransformVertices
#define MODELDIR(m) "../../Models/" + m.substr(0, m.find(".")) + "/"

class Mesh
{
public:
	struct MeshObject {
		MeshObject() {
			n_Indices = 0;
			baseVertex = 0;
			baseIndex = 0;
			materialIndex = 0;
		}
		unsigned int n_Indices;
		unsigned int baseVertex;
		unsigned int baseIndex;
		unsigned int materialIndex;
	};

	struct Material {
		vec3 ambientColour = vec3(0.0f);
		vec3 diffuseColour = vec3(0.0f);
		vec3 specularColour = vec3(0.0f);
	};

	Mesh() { ; }
	Mesh(std::string mesh_name) { loadMesh(mesh_name); }
	bool loadMesh(std::string mesh_name);
	bool initScene(const aiScene*, std::string);
	void initSingleMesh(const aiMesh*);
	bool initMaterials(const aiScene*, std::string);
	void loadColours(const aiMaterial*, int);
	void populateBuffers();
	void render();
	void render(unsigned int, const mat4*);
	void render(mat4);
	const Material& getMaterial();

#define POSITION_LOC 0 // p_vbo
#define NORMAL_LOC 1 // n_vbo
#define TEXTURE_LOC 2 // t_vbo
#define INSTANCE_LOC 3

	mat4 mat;
	unsigned int VAO; // mesh vao
	unsigned int p_VBO; // position vbo
	unsigned int n_VBO; // normal vbo
	unsigned int t_VBO; // texture vbo
	unsigned int EBO; // index (element) vbo (ebo)
	unsigned int IBO; // instance vbo (ibo)

	std::vector<vec3> m_Positions;
	std::vector<vec3> m_Normals;
	std::vector<vec2> m_TexCoords;
	std::vector<unsigned int> m_Indices;
	std::vector<MeshObject> m_Meshes;
	std::vector<Texture*> m_Textures;
	std::vector<Material> m_Materials;
};

