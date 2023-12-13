#include "Mesh.h"

/// <summary>
/// Load a mesh with a given name.
/// </summary>
/// <param name="file_name">The full name of the model to load.</param>
/// <returns>A boolean. True if loading succeeds, false otherwise.</returns>
bool Mesh::loadMesh(std::string file_name) {
	glGenVertexArrays(1, &VAO);
	glBindVertexArray(VAO);
	glGenBuffers(1, &p_VBO);
	glGenBuffers(1, &n_VBO);
	glGenBuffers(1, &t_VBO);
	glGenBuffers(1, &EBO);
	glGenBuffers(1, &IBO);

	std::string rpath = MODELDIR(file_name) + file_name;
	const aiScene* scene = aiImportFile(
		rpath.c_str(),
		AI_LOAD_FLAGS
	);

	bool valid_scene = false;

	if (!scene) {
		fprintf(stderr, "ERROR: reading mesh %s\n%s", rpath.c_str(), aiGetErrorString());
		valid_scene = false;
	}
	else {
		valid_scene = initScene(scene, file_name);
	}

	glBindVertexArray(0); // avoid modifying VAO between loads
	return valid_scene;
}

bool Mesh::initScene(const aiScene* scene, std::string file_name) {
	m_Meshes.resize(scene->mNumMeshes);
	m_Textures.resize(scene->mNumMaterials);
	//m_Materials.resize(scene->mNumMaterials);

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

	// Initialise meshes
	for (unsigned int i = 0; i < m_Meshes.size(); i++) {
		const aiMesh* am = scene->mMeshes[i];
		initSingleMesh(am);
	}

	if (!initMaterials(scene, file_name)) {
		return false;
	}

	populateBuffers();
	return glGetError() == GL_NO_ERROR;
}

/// <summary>
/// Initialise a single mesh object and add its values (vertices, indices, positions, normals, and texture coordinates) to the parent mesh.
/// </summary>
/// <param name="amesh">The mesh to initialise.</param>
void Mesh::initSingleMesh(const aiMesh* amesh) {
	const aiVector3D Zero3D(0.0f, 0.0f, 0.0f);

	// Populate the vertex attribute vectors
	for (unsigned int i = 0; i < amesh->mNumVertices; i++) {
		const aiVector3D& pPos = amesh->mVertices[i];
		const aiVector3D& pNormal = amesh->mNormals[i];
		const aiVector3D& pTexCoord = amesh->HasTextureCoords(0) ? amesh->mTextureCoords[0][i] : Zero3D;

		m_Positions.push_back(vec3(pPos.x, pPos.y, pPos.z));
		m_Normals.push_back(vec3(pNormal.x, pNormal.y, pNormal.z));
		m_TexCoords.push_back(vec2(pTexCoord.x, pTexCoord.y));
	}

	// Populate the index buffer
	for (unsigned int i = 0; i < amesh->mNumFaces; i++) {
		const aiFace& Face = amesh->mFaces[i];
		assert(Face.mNumIndices == 3);
		m_Indices.push_back(Face.mIndices[0]);
		m_Indices.push_back(Face.mIndices[1]);
		m_Indices.push_back(Face.mIndices[2]);
	}
}

/// <summary>
/// Initialise the materials and textures used in the mesh.
/// </summary>
/// <param name="scene">The mesh to load.</param>
/// <param name="file_name">The name of the model file.</param>
/// <returns>A boolean. <code>true</code> if everything loaded correctly, false otherwise.</returns>
bool Mesh::initMaterials(const aiScene* scene, std::string file_name) {
	bool valid = true;
	std::string dir = MODELDIR(file_name);
	for (unsigned int i = 0; i < scene->mNumMaterials; i++) {
		const aiMaterial* pMaterial = scene->mMaterials[i];
		m_Textures[i] = NULL;

		if (pMaterial->GetTextureCount(aiTextureType_DIFFUSE) > 0) {
			aiString Path;

			if (pMaterial->GetTexture(aiTextureType_DIFFUSE, 0, &Path, NULL, NULL, NULL, NULL, NULL) == AI_SUCCESS) {
				const aiTexture* cTex = scene->GetEmbeddedTexture(Path.C_Str());
				if (cTex) {
					printf("BMesh: embedded diffuse texture type %s\n", cTex->achFormatHint);
					m_Textures[i] = new Texture(GL_TEXTURE_2D);
					unsigned int buffer = cTex->mWidth;
					m_Textures[i]->load(buffer, cTex->pcData);
				}
				else {
					std::string p(Path.data);
					std::cout << p << std::endl;
					if (p.substr(0, 2) == ".\\") {
						p = p.substr(2, p.size() - 2);
					}
					std::string fullPath = dir + p;
					m_Textures[i] = new Texture(fullPath, GL_TEXTURE_2D);
					if (!m_Textures[i]->load()) {
						printf("Error loading diffuse texture '%s'\n", fullPath.c_str());
					}
					else {
						printf("BMesh: Loaded diffuse texture '%s'\n", fullPath.c_str());
					}
				}
			}
		}

		//loadColours(pMaterial, i);

	}
	return valid;
}

void Mesh::loadColours(const aiMaterial* pMaterial, int index) {
	aiColor3D ambientColour(0.0f, 0.0f, 0.0f);
	if (pMaterial->Get(AI_MATKEY_COLOR_AMBIENT, ambientColour) == AI_SUCCESS) {
		m_Materials[index].ambientColour.v[0] = ambientColour.r;
		m_Materials[index].ambientColour.v[1] = ambientColour.g;
		m_Materials[index].ambientColour.v[2] = ambientColour.b;
	}
	aiColor3D diffuseColour(0.0f, 0.0f, 0.0f);
	if (pMaterial->Get(AI_MATKEY_COLOR_DIFFUSE, diffuseColour) == AI_SUCCESS) {
		m_Materials[index].diffuseColour.v[0] = diffuseColour.r;
		m_Materials[index].diffuseColour.v[1] = diffuseColour.g;
		m_Materials[index].diffuseColour.v[2] = diffuseColour.b;
	}
	aiColor3D specularColour(0.0f, 0.0f, 0.0f);
	if (pMaterial->Get(AI_MATKEY_COLOR_SPECULAR, specularColour) == AI_SUCCESS) {
		m_Materials[index].specularColour.v[0] = specularColour.r;
		m_Materials[index].specularColour.v[1] = specularColour.g;
		m_Materials[index].specularColour.v[2] = specularColour.b;
	}
}

/// <summary>
/// Bind and enable the VAO, VBOs, and EBO for usage.
/// </summary>
void Mesh::populateBuffers() {
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

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(m_Indices[0]) * m_Indices.size(), &m_Indices[0], GL_STATIC_DRAW);

	glBindBuffer(GL_ARRAY_BUFFER, IBO);
	for (unsigned int i = 0; i < 4; i++) {
		glEnableVertexAttribArray(INSTANCE_LOC + i);
		glVertexAttribPointer(INSTANCE_LOC + i, 4, GL_FLOAT, GL_FALSE, sizeof(mat4), (const void*)(i * sizeof(vec4)));
		glVertexAttribDivisor(INSTANCE_LOC + i, 1); // tell OpenGL this is an instanced vertex attribute.
	}
}

/// <summary>
/// Render the mesh by binding its VAO and drawing each index of every submesh.
/// </summary>
void Mesh::render() {
	glBindVertexArray(VAO);

	for (int i = 0; i < m_Meshes.size(); i++) {
		unsigned int mIndex = m_Meshes[i].materialIndex;
		assert(mIndex < m_Textures.size());

		if (m_Textures[mIndex]) m_Textures[mIndex]->bind(GL_TEXTURE0);

		// original code (OGLDEV) didn't access the indices for some reason, so i added it in
		//glDrawElementsBaseVertex(GL_TRIANGLES, m_Meshes[i].n_Indices, GL_UNSIGNED_INT, (void*)(&(m_Indices[m_Meshes[i].baseIndex])), m_Meshes[i].baseVertex);
		glDrawElementsBaseVertex(
			GL_TRIANGLES,
			m_Meshes[i].n_Indices,
			GL_UNSIGNED_INT,
			(void*)(sizeof(unsigned int) * m_Meshes[i].baseIndex),
			m_Meshes[i].baseVertex
		);
		//glDrawArrays(GL_TRIANGLES, m_Meshes[i].baseIndex, m_Meshes[i].n_Indices); // this works too somwhow
	}
	glBindVertexArray(0); // prevent VAO from being changed externally
}

/// <summary>
/// Render the mesh by binding its VAO and drawing each index of every submesh. This function supports instancing.
/// </summary>
/// <param name="nInstances">The number of instances you would like to draw.</param>
/// <param name="model_matrix">The matrices you would like to transform each instance with.</param>
void Mesh::render(unsigned int nInstances, const mat4* model_matrix) {
	glBindVertexArray(VAO);
	glBindBuffer(GL_ARRAY_BUFFER, IBO);
	glBufferData(GL_ARRAY_BUFFER, sizeof(mat4) * nInstances, &model_matrix[0], GL_DYNAMIC_DRAW);
	for (unsigned int i = 0; i < m_Meshes.size(); i++) {
		unsigned int mIndex = m_Meshes[i].materialIndex;
		assert(mIndex < m_Textures.size());

		if (m_Textures[mIndex]) m_Textures[mIndex]->bind(GL_TEXTURE0);
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

/// <summary>
/// Render the mesh by binding its VAO and drawing each index of every submesh. This is a special case that will render exactly one instance of your mesh.
/// </summary>
/// <param name="mat">The transform you would like to apply to your instance.</param>
void Mesh::render(mat4 mat) {
	render(1, &mat);
}

const Mesh::Material& Mesh::getMaterial() {
	for (int i = 0; i < m_Materials.size(); i++) {
		if (m_Materials[i].ambientColour != vec3(0.0f)) {
			return m_Materials[i];
		}
	}
}