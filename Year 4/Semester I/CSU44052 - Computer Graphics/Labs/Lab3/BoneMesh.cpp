#include "BoneMesh.h"
using namespace std;

bool BoneMesh::loadMesh(std::string mesh_name)
{
	glGenVertexArrays(1, &VAO);
	glBindVertexArray(VAO);
	glGenBuffers(1, &p_VBO);
	glGenBuffers(1, &n_VBO);
	glGenBuffers(1, &t_VBO);
	glGenBuffers(1, &EBO);
	glGenBuffers(1, &IBO);
	glGenBuffers(1, &BBO);
	glGenBuffers(1, &RBO);

	std::string rpath = MODELDIR(mesh_name) + mesh_name;
	scene = importer.ReadFile(rpath.c_str(), B_AI_LOAD_FLAGS);

	bool valid_scene = false;

	if (!scene) {
		fprintf(stderr, "ERROR: reading mesh %s\n%s", rpath.c_str(), importer.GetErrorString());
		valid_scene = false;
	}
	else {
		globalInverseTrans = scene->mRootNode->mTransformation.Inverse(); // invert global transformation matrix
		valid_scene = initScene(scene, mesh_name);
	}

	glBindVertexArray(0); // avoid modifying VAO between loads
	return valid_scene;
}

bool BoneMesh::initScene(const aiScene* scene, std::string mesh_name)
{
	m_Meshes.resize(scene->mNumMeshes);
	m_Materials.resize(scene->mNumMaterials);

	unsigned int nvertices = 0;
	unsigned int nindices = 0;

	// Count all vertices and indices
	for (unsigned int i = 0; i < m_Meshes.size(); i++) {
		m_Meshes[i].materialIndex = scene->mMeshes[i]->mMaterialIndex; // get current material index
		m_Meshes[i].n_Indices = scene->mMeshes[i]->mNumFaces * 3; // there are 3 times as many indices as there are faces (since they're all triangles)
		m_Meshes[i].baseVertex = nvertices; // index of first vertex in the current mesh
		m_Meshes[i].baseIndex = nindices; // track number of indices

		// Move forward by the corresponding number of vertices/indices to find the base of the next vertex/index
		nvertices += scene->mMeshes[i]->mNumVertices;
		nindices += m_Meshes[i].n_Indices;
	}

	// Reallocate space for structure of arrays (SOA) values
	m_Positions.reserve(nvertices);
	m_Normals.reserve(nvertices);
	m_TexCoords.reserve(nvertices);
	m_Indices.reserve(nindices);
	m_Bones.resize(nvertices);

	// Initialise meshes
	for (unsigned int i = 0; i < m_Meshes.size(); i++) {
		const aiMesh* am = scene->mMeshes[i];
		initSingleMesh(i, am);
	}

	if (!initMaterials(scene, mesh_name)) {
		return false;
	}

	populateBuffers();
	return glGetError() == GL_NO_ERROR;
}

void BoneMesh::initSingleMesh(unsigned int mIndex, const aiMesh* amesh)
{
	// Populate the vertex attribute vectors
	for (unsigned int i = 0; i < amesh->mNumVertices; i++) {
		const aiVector3D& pPos = amesh->mVertices[i];
		const aiVector3D& pNormal = amesh->mNormals ? amesh->mNormals[i] : aiVector3D(0.0f, 1.0f, 0.0f);
		const aiVector3D& pTexCoord = amesh->HasTextureCoords(0) ? amesh->mTextureCoords[0][i] : aiVector3D(0.0f, 0.0f, 0.0f);
		//if (!amesh->mNormals) printf("no normal for index %u\n", mIndex);
		m_Positions.push_back(vec3(pPos.x, pPos.y, pPos.z));
		m_Normals.push_back(vec3(pNormal.x, pNormal.y, pNormal.z));
		m_TexCoords.push_back(vec2(pTexCoord.x, pTexCoord.y));
	}

	// Load all bones in mesh
	for (int i = 0; i < amesh->mNumBones; i++) {
		loadSingleBone(mIndex, amesh->mBones[i]);
	}

	// Populate the index buffer
	for (unsigned int i = 0; i < amesh->mNumFaces; i++) {
		const aiFace& Face = amesh->mFaces[i];
		//assert(Face.mNumIndices == 3);
		m_Indices.push_back(Face.mIndices[0]);
		m_Indices.push_back(Face.mIndices[1]);
		m_Indices.push_back(Face.mIndices[2]);
	}
}

bool BoneMesh::initMaterials(const aiScene* scene, std::string mesh_name)
{
	bool valid = true;
	std::string dir = MODELDIR(mesh_name);
	for (unsigned int i = 0; i < scene->mNumMaterials; i++) {
		const aiMaterial* pMaterial = scene->mMaterials[i];
		loadDiffuseTexture(pMaterial, dir, i);
		loadSpecularTexture(pMaterial, dir, i);
		loadColours(pMaterial, i);
	}
	return valid && glGetError() == GL_NO_ERROR;
}

void BoneMesh::loadDiffuseTexture(const aiMaterial* pMaterial, std::string dir, unsigned int index) {
	m_Materials[index].diffTex = NULL;

	if (pMaterial->GetTextureCount(aiTextureType_DIFFUSE) > 0) {
		aiString Path;

		if (pMaterial->GetTexture(aiTextureType_DIFFUSE, 0, &Path, NULL, NULL, NULL, NULL, NULL) == AI_SUCCESS) {
			const aiTexture* cTex = scene->GetEmbeddedTexture(Path.C_Str());
			if (cTex) {
				printf("BMesh: embedded texture type %s\n", cTex->achFormatHint);
				m_Materials[index].diffTex = new Texture(GL_TEXTURE_2D);
				unsigned int buffer = cTex->mWidth;
				m_Materials[index].diffTex->load(buffer, cTex->pcData);
			} else {
				std::string p(Path.data);
				std::cout << p << std::endl;
				if (p.substr(0, 2) == ".\\") {
					p = p.substr(2, p.size() - 2);
				}
				std::string fullPath = dir + p;
				m_Materials[index].diffTex = new Texture(fullPath, GL_TEXTURE_2D);
				if (!m_Materials[index].diffTex->load()) {
					printf("Error loading diffuse texture '%s'\n", fullPath.c_str());
				} else {
					printf("BMesh: Loaded diffuse texture '%s'\n", fullPath.c_str());
				}
			}
		}
	}
}

void BoneMesh::loadSpecularTexture(const aiMaterial* pMaterial, std::string dir, unsigned int index) {
	m_Materials[index].specExp = NULL;

	if (pMaterial->GetTextureCount(aiTextureType_SHININESS) > 0) {
		aiString Path;

		if (pMaterial->GetTexture(aiTextureType_SHININESS, 0, &Path, NULL, NULL, NULL, NULL, NULL) == AI_SUCCESS) {
			std::string p(Path.data);
			std::cout << p << std::endl;
			if (p.substr(0, 2) == ".\\") {
				p = p.substr(2, p.size() - 2);
			}

			std::string fullPath = dir + p;
			m_Materials[index].specExp = new Texture(fullPath, GL_TEXTURE_2D);

			if (!m_Materials[index].specExp->load()) {
				printf("Error loading specular texture '%s'\n", fullPath.c_str());
				//delete m_Textures[i];
				//m_Textures[i] = NULL;
				//valid = false;
			}
			else {
				printf("BMesh: Loaded specular texture '%s'\n", fullPath.c_str());
			}
		}
	}
}

void BoneMesh::loadColours(const aiMaterial* pMaterial, int ind)
{
	int shadingModel = 0;
	if (pMaterial->Get(AI_MATKEY_SHADING_MODEL, shadingModel) == AI_SUCCESS) {
		printf("Shading model %d\n", shadingModel);
	}

	aiColor3D ambientColour(0.0f, 0.0f, 0.0f);
	if (pMaterial->Get(AI_MATKEY_COLOR_AMBIENT, ambientColour) == AI_SUCCESS) {
		printf("Loaded ambient color [%f %f %f]\n", ambientColour.r, ambientColour.g, ambientColour.b);
		m_Materials[ind].ambientColour.v[0] = ambientColour.r;
		m_Materials[ind].ambientColour.v[1] = ambientColour.g;
		m_Materials[ind].ambientColour.v[2] = ambientColour.b;
	}
	else {
		m_Materials[ind].ambientColour = vec3(1.0f);
	}

	aiColor3D diffuseColour(0.0f, 0.0f, 0.0f);
	if (pMaterial->Get(AI_MATKEY_COLOR_DIFFUSE, diffuseColour) == AI_SUCCESS) {
		printf("Loaded diffuse color [%f %f %f]\n", diffuseColour.r, diffuseColour.g, diffuseColour.b);
		m_Materials[ind].diffuseColour.v[0] = diffuseColour.r;
		m_Materials[ind].diffuseColour.v[1] = diffuseColour.g;
		m_Materials[ind].diffuseColour.v[2] = diffuseColour.b;
	}

	aiColor3D specularColour(0.0f, 0.0f, 0.0f);
	if (pMaterial->Get(AI_MATKEY_COLOR_SPECULAR, specularColour) == AI_SUCCESS) {
		printf("Loaded specular color [%f %f %f]\n", specularColour.r, specularColour.g, specularColour.b);
		m_Materials[ind].specularColour.v[0] = specularColour.r;
		m_Materials[ind].specularColour.v[1] = specularColour.g;
		m_Materials[ind].specularColour.v[2] = specularColour.b;
	}
}

void BoneMesh::populateBuffers()
{
	glBindBuffer(GL_ARRAY_BUFFER, p_VBO);
	glBufferData(GL_ARRAY_BUFFER, sizeof(m_Positions[0]) * m_Positions.size(), &m_Positions[0], GL_STATIC_DRAW);
	glEnableVertexAttribArray(POSITION_LOC);
	glVertexAttribPointer(POSITION_LOC, 3, GL_FLOAT, GL_FALSE, 0, 0);

	glBindBuffer(GL_ARRAY_BUFFER, n_VBO);
	glBufferData(GL_ARRAY_BUFFER, sizeof(m_Normals[0]) * m_Normals.size(), &m_Normals[0], GL_STATIC_DRAW);
	glEnableVertexAttribArray(NORMAL_LOC);
	glVertexAttribPointer(NORMAL_LOC, 3, GL_FLOAT, GL_FALSE, 0, 0);

	glBindBuffer(GL_ARRAY_BUFFER, t_VBO);
	glBufferData(GL_ARRAY_BUFFER, sizeof(m_TexCoords[0]) * m_TexCoords.size(), &m_TexCoords[0], GL_STATIC_DRAW);
	glEnableVertexAttribArray(TEXTURE_LOC);
	glVertexAttribPointer(TEXTURE_LOC, 2, GL_FLOAT, GL_FALSE, 0, 0);

	glBindBuffer(GL_ARRAY_BUFFER, BBO);
	glBufferData(GL_ARRAY_BUFFER, sizeof(m_Bones[0]) * m_Bones.size(), &m_Bones[0], GL_STATIC_DRAW);
	glEnableVertexAttribArray(BONE_LOC);
	glVertexAttribIPointer(BONE_LOC, MAX_NUM_BONES_PER_VERTEX, GL_INT, sizeof(VertexBoneData), (const GLvoid*)0);

	glEnableVertexAttribArray(BONE_WEIGHT_LOC);
	glVertexAttribPointer(BONE_WEIGHT_LOC, MAX_NUM_BONES_PER_VERTEX, GL_FLOAT, GL_FALSE,
		sizeof(VertexBoneData), (const GLvoid*)(MAX_NUM_BONES_PER_VERTEX * sizeof(int32_t)));

	glBindBuffer(GL_ARRAY_BUFFER, IBO);
	for (unsigned int i = 0; i < 4; i++) {
		glEnableVertexAttribArray(INSTANCE_LOC + i);
		glVertexAttribPointer(INSTANCE_LOC + i, 4, GL_FLOAT, GL_FALSE, sizeof(mat4), (const void*)(i * sizeof(vec4)));
		glVertexAttribDivisor(INSTANCE_LOC + i, 1); // tell OpenGL this is an instanced vertex attribute.
	}

	glBindBuffer(GL_ARRAY_BUFFER, RBO);
	for (unsigned int i = 0; i < 4; i++) {
		glEnableVertexAttribArray(ROOT_LOC + i);
		glVertexAttribPointer(ROOT_LOC + i, 4, GL_FLOAT, GL_FALSE, sizeof(mat4), (const void*)(i * sizeof(vec4)));
		glVertexAttribDivisor(ROOT_LOC + i, 1); // tell OpenGL this is an instanced vertex attribute.
	}

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(m_Indices[0]) * m_Indices.size(), &m_Indices[0], GL_STATIC_DRAW);

}

void BoneMesh::render(unsigned int nInstances, const mat4* model_matrix, const mat4* bone_trans_matrix) // , const mat4* bone_trans_matrix
{
	glBindVertexArray(VAO);
	glBindBuffer(GL_ARRAY_BUFFER, IBO);
	glBufferData(GL_ARRAY_BUFFER, sizeof(mat4) * nInstances, &model_matrix[0], GL_DYNAMIC_DRAW);

	glBindBuffer(GL_ARRAY_BUFFER, RBO);
	glBufferData(GL_ARRAY_BUFFER, sizeof(mat4) * nInstances, &bone_trans_matrix[0], GL_DYNAMIC_DRAW);

	for (unsigned int i = 0; i < m_Meshes.size(); i++) {
		unsigned int mIndex = m_Meshes[i].materialIndex;
		assert(mIndex < m_Materials.size());

		if (m_Materials[mIndex].diffTex) m_Materials[mIndex].diffTex->bind(GL_TEXTURE0);
		if (m_Materials[mIndex].specExp) m_Materials[mIndex].specExp->bind(GL_TEXTURE1);
		glDrawElementsInstancedBaseVertex(
			GL_TRIANGLES,
			m_Meshes[i].n_Indices,
			GL_UNSIGNED_INT,
			(void*)(sizeof(unsigned int) * m_Meshes[i].baseIndex),
			nInstances,
			m_Meshes[i].baseVertex
		);
	}
	glBindVertexArray(0); // prevent VAO from being changed externally
}

void BoneMesh::render(unsigned int nInstances, const mat4* model_matrix, const glm::mat4* bone_trans_matrix) // , const mat4* bone_trans_matrix
{
	glBindVertexArray(VAO);
	glBindBuffer(GL_ARRAY_BUFFER, IBO);
	glBufferData(GL_ARRAY_BUFFER, sizeof(mat4) * nInstances, &model_matrix[0], GL_DYNAMIC_DRAW);

	glBindBuffer(GL_ARRAY_BUFFER, RBO);
	glBufferData(GL_ARRAY_BUFFER, sizeof(mat4) * nInstances, &bone_trans_matrix[0], GL_DYNAMIC_DRAW);

	for (unsigned int i = 0; i < m_Meshes.size(); i++) {
		unsigned int mIndex = m_Meshes[i].materialIndex;
		assert(mIndex < m_Materials.size());

		if (m_Materials[mIndex].diffTex) m_Materials[mIndex].diffTex->bind(GL_TEXTURE0);
		if (m_Materials[mIndex].specExp) m_Materials[mIndex].specExp->bind(GL_TEXTURE1);
		glDrawElementsInstancedBaseVertex(
			GL_TRIANGLES,
			m_Meshes[i].n_Indices,
			GL_UNSIGNED_INT,
			(void*)(sizeof(unsigned int) * m_Meshes[i].baseIndex),
			nInstances,
			m_Meshes[i].baseVertex
		);
	}
	glBindVertexArray(0); // prevent VAO from being changed externally
}

void BoneMesh::render(mat4 mm)
{
	mat4 id = identity_mat4();
	render(1, &id, &mm);
}

const BoneMesh::Material& BoneMesh::getMaterial()
{
	// TODO: insert return statement here
	return Material();
}

void BoneMesh::loadMeshBones(unsigned int mIndex, const aiMesh* aMesh)
{

}

void BoneMesh::loadSingleBone(unsigned int mIndex, const aiBone* pBone)
{
	int boneID = getBoneID(pBone);

	if (boneID == m_BoneInfo.size()) {
		BoneInfo* bInfo = new BoneInfo(pBone->mOffsetMatrix);
		m_BoneInfo.push_back(bInfo);
	}

	for (int i = 0; i < pBone->mNumWeights; i++) {
		const aiVertexWeight& vw = pBone->mWeights[i];
		unsigned int gvID = m_Meshes[mIndex].baseVertex + pBone->mWeights[i].mVertexId; // global vertex id
		//m_Bones[gvID]->addBoneData(boneID, vw.mWeight);
		VertexBoneData vbd = VertexBoneData();
		vbd.addBoneData(boneID, vw.mWeight);
		m_Bones[gvID] = vbd;
	}
}

int BoneMesh::getBoneID(const aiBone* pBone)
{
	int bIndex = 0;
	string bName = pBone->mName.C_Str();

	if (boneToIndexMap.find(bName) == boneToIndexMap.end()) {
		bIndex = boneToIndexMap.size();
		boneToIndexMap[bName] = bIndex;
		return bIndex;
	}
	else {
		return boneToIndexMap[bName];
	}
}

void BoneMesh::getBoneTransforms(float time, std::vector<aiMatrix4x4>& trans) {
	aiAnimation emptyAnim;
	float tps = 0.0f;
	float animTime = 0.0f;
	if (scene->HasAnimations()) {
		tps = scene->mAnimations[0]->mTicksPerSecond != 0 ?
			(float)scene->mAnimations[0]->mTicksPerSecond :
			25.0f
			; // ticks per second
		animTime = fmod(time * tps, (float)scene->mAnimations[0]->mDuration); // animation time in ticks
	}

	// use the root transform to base all bones off of
	//readNodeHierarchy(animTime, scene->mRootNode, rootTransform);
	readNodeHierarchy(animTime, scene->mRootNode, aiMatrix4x4());
	trans.resize(m_BoneInfo.size());

	for (int i = 0; i < m_BoneInfo.size(); i++) {
		trans[i] = m_BoneInfo[i]->lastTransformation;
	}
}

void BoneMesh::readNodeHierarchy(float atime, const aiNode* node, const aiMatrix4x4& parent) {
	string nodeName(node->mName.data);
	aiMatrix4x4 nodeTrans = node->mTransformation;

	if (scene->HasAnimations()) {
		const aiAnimation* anim = scene->mAnimations[0];
		const aiNodeAnim* animNode = findNodeAnim(anim, nodeName);

		// Interpolate translation, scaling, and rotation
		if (animNode) {
			// Interpolate translation
			aiVector3D trans;
			calcInterpolatedTranslation(trans, atime, animNode);
			aiMatrix4x4 transM;
			aiMatrix4x4::Translation(trans, transM);

			// Interpolate scaling
			aiVector3D scale;
			calcInterpolatedScale(scale, atime, animNode);
			aiMatrix4x4 scaleM;
			aiMatrix4x4::Scaling(scale, scaleM);

			// Interpolate rotation
			aiQuaternion rotation;
			calcInterpolatedRotation(rotation, atime, animNode);
			aiMatrix4x4 rotM;
			//aiMatrix4x4::Rotation(rotation.w, aiVector3D(rotation.x, rotation.y, rotation.z), rotM);
			rotM = aiMatrix4x4(rotation.GetMatrix());

			nodeTrans = transM * rotM * scaleM;
		}
	}

	aiMatrix4x4 globalTrans = parent * nodeTrans;
	if (boneToIndexMap.find(nodeName) != boneToIndexMap.end()) {
		unsigned int bIndex = boneToIndexMap[nodeName];
		m_BoneInfo[bIndex]->lastTransformation = globalInverseTrans * globalTrans * m_BoneInfo[bIndex]->offsetMatrix;
	}

	for (int i = 0; i < node->mNumChildren; i++) {
		readNodeHierarchy(atime, node->mChildren[i], globalTrans);
	}
}

const aiNodeAnim* BoneMesh::findNodeAnim(const aiAnimation* anim, const std::string nodeName) {
	for (int i = 0; i < anim->mNumChannels; i++) {
		const aiNodeAnim* tnode = anim->mChannels[i];
		if (string(tnode->mNodeName.data) == nodeName) return tnode;
	}
	return NULL;
}

void BoneMesh::calcInterpolatedTranslation(aiVector3D& out, float atime, const aiNodeAnim* node) {
	if (node->mNumPositionKeys <= 1) {
		out = node->mPositionKeys[0].mValue;
		return;
	}

	// lambda function. finds translation factor given an animation time in ticks and an aiNodeAnim. made because this function has only this one purpose
	auto findPosition = [atime, &node]() -> unsigned int {
		assert(node->mNumPositionKeys > 0);

		for (unsigned int i = 0; i < node->mNumPositionKeys - 1; i++) {
			float t = (float)node->mPositionKeys[i + 1].mTime;
			if (atime < t) {
				return i;
			}
		}
		return 0;
		};
	unsigned int positionIndex = findPosition();
	assert(positionIndex + 1 < node->mNumPositionKeys);

	float deltaTime = node->mPositionKeys[positionIndex + 1].mTime - node->mPositionKeys[positionIndex].mTime; // delta between two adjacent Position keyfraes
	float factor = (atime - node->mPositionKeys[positionIndex].mTime) / deltaTime; // intermediate Position factor
	//assert(factor >= 0.0f && factor <= 1.0f); // check for change in position
	const aiVector3D& start = node->mPositionKeys[positionIndex].mValue; // start position value
	const aiVector3D& end = node->mPositionKeys[positionIndex + 1].mValue; // end position value
	out = start + factor * (end - start); // interpolated position
}

void BoneMesh::calcInterpolatedScale(aiVector3D& out, float atime, const aiNodeAnim* node) {
	if (node->mNumScalingKeys <= 1) {
		out = node->mScalingKeys[0].mValue;
		return;
	}

	// lambda function. finds scaling factor given an animation time in ticks and an aiNodeAnim. made because this function has only this one purpose
	auto findScaling = [atime, &node]() -> unsigned int {
		assert(node->mNumScalingKeys > 0);

		for (unsigned int i = 0; i < node->mNumScalingKeys - 1; i++) {
			float t = (float)node->mScalingKeys[i + 1].mTime;
			if (atime < t) {
				return i;
			}
		}
		return 0;
		};
	unsigned int scalingIndex = findScaling();
	assert(scalingIndex + 1 < node->mNumScalingKeys);

	float deltaTime = node->mScalingKeys[scalingIndex + 1].mTime - node->mScalingKeys[scalingIndex].mTime; // delta between two adjacent scaling keyfraes
	float factor = (atime - node->mScalingKeys[scalingIndex].mTime) / deltaTime; // intermediate scaling factor
	//assert(factor >= 0.0f && factor <= 1.0f); // check for change in scale
	const aiVector3D& start = node->mScalingKeys[scalingIndex].mValue; // start scale value
	const aiVector3D& end = node->mScalingKeys[scalingIndex + 1].mValue; // end scale value
	out = start + factor * (end - start); // interpolated scale
}

void BoneMesh::calcInterpolatedRotation(aiQuaternion& out, float atime, const aiNodeAnim* node) {
	if (node->mNumRotationKeys <= 1) {
		out = node->mRotationKeys[0].mValue;
		return;
	}

	// lambda function. finds rotation factor given an animation time in ticks and an aiNodeAnim. made because this function has only this one purpose
	auto findRotation = [atime, &node]() -> unsigned int {
		assert(node->mNumRotationKeys > 0);

		for (unsigned int i = 0; i < node->mNumRotationKeys - 1; i++) {
			float t = (float)node->mRotationKeys[i + 1].mTime;
			if (atime < t) {
				return i;
			}
		}
		return 0;
		};
	unsigned int rotationIndex = findRotation();
	assert(rotationIndex + 1 < node->mNumRotationKeys);

	float deltaTime = node->mRotationKeys[rotationIndex + 1].mTime - node->mRotationKeys[rotationIndex].mTime; // delta between two adjacent rotation keyfraes
	float factor = (atime - node->mRotationKeys[rotationIndex].mTime) / deltaTime; // intermediate rotation factor
	//assert(factor >= 0.0f && factor <= 1.0f); // check for change in rotation
	const aiQuaternion& start = node->mRotationKeys[rotationIndex].mValue; // start rotation value
	const aiQuaternion& end = node->mRotationKeys[rotationIndex + 1].mValue; // end rotation value
	aiQuaternion::Interpolate(out, start, end, factor);
	out.Normalize(); // interpolated rotation
}
