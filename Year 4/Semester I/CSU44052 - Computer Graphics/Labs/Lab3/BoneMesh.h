#pragma once
#pragma warning(disable : 26495)

#include <map>
#include <string>
#include <vector>
#include <assert.h>

#include <assimp/cimport.h> // scene importer
#include <assimp/Importer.hpp>
#include <assimp/scene.h> // collects data
#include <assimp/postprocess.h> // various extra operations

#include <GL/glew.h>
#include <GL/freeglut.h>

#include "util.h"
#include "texture.h"

#define MAX_NUM_BONES_PER_VERTEX 4
#define B_AI_LOAD_FLAGS aiProcess_Triangulate 
#define MODELDIR(m) "../../Models/" + m.substr(0, m.find(".")) + "/"


class BoneMesh
{
public:
	struct VertexBoneData
	{
		VertexBoneData() {}

		unsigned int BoneIDs[MAX_NUM_BONES_PER_VERTEX] = { 0 };
		float Weights[MAX_NUM_BONES_PER_VERTEX] = { 0.0f };

		void addBoneData(unsigned int BoneID, float Weight)
		{
			for (unsigned int i = 0; i < ARRAY_SIZE(BoneIDs); i++) {
				if (Weights[i] == 0.0) {
					BoneIDs[i] = BoneID;
					Weights[i] = Weight;
					//printf("bone %d weight %f index %i\n", BoneID, Weight, i);
					return;
				}
			}

			// should never get here - more bones than we have space for
			assert(0);
		}
	};

	struct BoneInfo {
		aiMatrix4x4 offsetMatrix;
		aiMatrix4x4 lastTransformation;

		BoneInfo(const aiMatrix4x4& offset) {
			offsetMatrix = offset;
			lastTransformation = Util::toAIM4(zero_mat4());
		}
	};

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
		Texture* diffTex = NULL;
		Texture* specExp = NULL;
	};

	bool loadMesh(std::string mesh_name);
	bool initScene(const aiScene*, std::string);
	void initSingleMesh(unsigned int, const aiMesh*);
	bool initMaterials(const aiScene*, std::string);
	void loadDiffuseTexture(const aiMaterial* pMaterial, std::string dir, unsigned int index);
	void loadSpecularTexture(const aiMaterial* pMaterial, std::string dir, unsigned int index);
	void loadColours(const aiMaterial*, int);
	void populateBuffers();
	void render(unsigned int, const mat4*, const mat4*);
	void render(unsigned int, const mat4*, const glm::mat4*);
	void render(mat4);
	const Material& getMaterial();
	void loadMeshBones(unsigned int, const aiMesh*);
	void loadSingleBone(unsigned int, const aiBone*);
	int getBoneID(const aiBone*);
	void getBoneTransforms(float, std::vector<aiMatrix4x4>&);
	void readNodeHierarchy(float, const aiNode*, const aiMatrix4x4&);
	const aiNodeAnim* findNodeAnim(const aiAnimation*, const std::string);
	void calcInterpolatedTranslation(aiVector3D&, float, const aiNodeAnim*);
	void calcInterpolatedScale(aiVector3D&, float, const aiNodeAnim*);
	void calcInterpolatedRotation(aiQuaternion&, float, const aiNodeAnim*);


#define POSITION_LOC 0 // p_vbo
#define NORMAL_LOC 1 // n_vbo
#define TEXTURE_LOC 2 // t_vbo
#define BONE_LOC 3 // bone vbo
#define BONE_WEIGHT_LOC 4 // bone weight location
#define INSTANCE_LOC 5
#define ROOT_LOC 9


	aiMatrix4x4 rootTransform = aiMatrix4x4(); // root matrix of mesh. used to transform entire mesh.
	aiMatrix4x4 globalInverseTrans; // inverse matrix
	unsigned int VAO; // mesh vao
	unsigned int p_VBO; // position vbo
	unsigned int n_VBO; // normal vbo
	unsigned int t_VBO; // texture vbo
	unsigned int EBO; // index (element) vbo (ebo)
	unsigned int IBO; // instance vbo (ibo)
	unsigned int BBO; // bone vbo
	unsigned int RBO; // root transform vbo

	std::vector<vec3> m_Positions;
	std::vector<vec3> m_Normals;
	std::vector<vec2> m_TexCoords;
	std::vector<unsigned int> m_Indices;

	std::vector<MeshObject> m_Meshes;
	//std::vector<Texture*> m_Textures;
	std::vector<Material> m_Materials;
	std::vector<VertexBoneData> m_Bones;
	std::vector<BoneInfo*> m_BoneInfo;

	std::map<std::string, unsigned int> boneToIndexMap;

	const aiScene* scene;
	Assimp::Importer importer;
};

