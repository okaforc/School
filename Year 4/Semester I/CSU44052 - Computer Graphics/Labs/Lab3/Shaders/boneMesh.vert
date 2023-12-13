#version 330
layout (location = 0) in vec3 vertex_position;
layout (location = 1) in vec3 vertex_normal;
layout (location = 2) in vec2 vertex_texture;
layout (location = 3) in ivec4 vertex_bone;
layout (location = 4) in vec4 vertex_weight;
layout (location = 5) in mat4 instance_trans; // transform of mesh instance
layout (location = 9) in mat4 root_trans; // transform of root bone

out vec3 FragPos;
out vec3 Normal;
out vec2 TexCoords;
out vec4 Weights;
flat out ivec4 BoneID;

uniform mat4 view;
uniform mat4 proj;
uniform mat4 glBones[200];

void main()
{
    mat4 boneTransform = root_trans * glBones[vertex_bone[0]] * vertex_weight[0];
    boneTransform += root_trans * glBones[vertex_bone[1]] * vertex_weight[1];
    boneTransform += root_trans * glBones[vertex_bone[2]] * vertex_weight[2];
    boneTransform += root_trans * glBones[vertex_bone[3]] * vertex_weight[3];

    FragPos = vec3(instance_trans * vec4(vertex_position, 1.0));
    Normal = mat3(transpose(inverse(instance_trans))) * vertex_normal;
    TexCoords = vertex_texture;
    BoneID = vertex_bone;
    Weights = vertex_weight;
    gl_Position = proj * view * boneTransform * vec4(FragPos, 1.0);
}