#include "util.h"
//using namespace std;

std::string Util::readFile(const char* path) {
	std::ifstream file(path);
	if (!file.is_open()) {
		std::cout << "Failed to open file." << std::endl;
		return "";
	}

	std::string text, line;
	while (getline(file, line)) {
		text += line + "\n";
	}

	file.close();
	text.append("\0");
	return text;
}



// Wrap a value between min and max. If val is greater than max, it will wraparound to min and begin climbing from there, and vice versa.
float Util::wrap(float val, float min, float max) {
	int range = max - min;
	if (val < min) val += range * ((min - val) / range + 1);
	return fmod(min + (val - min), range);
}

// Clamp a value between a minimum and maximum so that val cannot be greater than max or smaller than min.
float Util::clamp(float val, float min, float max) {
	if (val < min) val = min;
	if (val > max) val = max;
	return val;
}

// Translate a regular OpenGL graphic (top left is (0, 0), bottom right = (width, height)) to Cartesian coordinates with a range of (-1, 1) on both axes.
// That is, normalise a point's axes.
// e.g., 
// WIDTH = 800
// HEIGHT = 600
// mousePos = vec2(400, 300)
// normalisePos(mousePos) = normMousePos = (0, 0)
// normalisePos(vec2(200, 700)) = (-0.5, -1.333)
vec2 normalisePos(vec2 v, float w, float h) {
	return vec2((v.v[0] - w / 2) / w * 2, (v.v[1] - h / 2) / h * -2);
}

void printMatrix(mat4 mm) {
	std::cout << mm.m[0] << ", " << mm.m[1] << ", " << mm.m[2] << ", " << mm.m[3] << std::endl;
	std::cout << mm.m[4] << ", " << mm.m[5] << ", " << mm.m[6] << ", " << mm.m[7] << std::endl;
	std::cout << mm.m[8] << ", " << mm.m[9] << ", " << mm.m[10] << ", " << mm.m[11] << std::endl;
	std::cout << mm.m[12] << ", " << mm.m[13] << ", " << mm.m[14] << ", " << mm.m[15] << std::endl;
}

// Convert degrees to radians
float Util::deg2Rad(float val) {
	return val * ONE_DEG_IN_RAD;
}

// Convert radians to degrees
float Util::rad2Deg(float val) {
	return val * ONE_RAD_IN_DEG;
}

/// <summary>
/// Linear interpolation. Interpolate a value from val to max at rate delta.
/// </summary>
/// <param name="val">The value to start interpolating from.</param>
/// <param name="min">The minmum value allowed for interpolation.</param>
/// <param name="max">The maximum value allowed for interpolation.</param>
/// <param name="delta">The rate of change of linearity.</param>
/// <returns>The value updated with the next interpolated value. For proper usage, this function should run using constantly updating values.</returns>
float Util::lerp(float val, float min, float max, float delta) {
	if (abs(val - min) < 1.5 || abs(val - max) < 1.5) return clamp(val + delta / 3, min, max);
	else return clamp(val + delta, min, max);
}

void Util::printVec3(vec3 v) {
	std::cout << v.v[0] << ", " << v.v[1] << ", " << v.v[2] << std::endl;
}

aiMatrix4x4 Util::toAIM4(const mat4& mat) {
	aiMatrix4x4* p = new aiMatrix4x4();
	p->a1 = mat.m[0];
	p->a2 = mat.m[4];
	p->a3 = mat.m[8];
	p->a4 = mat.m[12];

	p->b1 = mat.m[1];
	p->b2 = mat.m[5];
	p->b3 = mat.m[9];
	p->b4 = mat.m[13];

	p->c1 = mat.m[2];
	p->c2 = mat.m[6];
	p->c3 = mat.m[10];
	p->c4 = mat.m[14];

	p->d1 = mat.m[3];
	p->d2 = mat.m[7];
	p->d3 = mat.m[11];
	p->d4 = mat.m[15];
	return *p;
}

glm::mat4 Util::aiToGLM(aiMatrix4x4* from) {
	glm::mat4 to;
	to[0][0] = (GLfloat)from->a1; to[0][1] = (GLfloat)from->b1; to[0][2] = (GLfloat)from->c1; to[0][3] = (GLfloat)from->d1;
	to[1][0] = (GLfloat)from->a2; to[1][1] = (GLfloat)from->b2; to[1][2] = (GLfloat)from->c2; to[1][3] = (GLfloat)from->d2;
	to[2][0] = (GLfloat)from->a3; to[2][1] = (GLfloat)from->b3; to[2][2] = (GLfloat)from->c3; to[2][3] = (GLfloat)from->d3;
	to[3][0] = (GLfloat)from->a4; to[3][1] = (GLfloat)from->b4; to[3][2] = (GLfloat)from->c4; to[3][3] = (GLfloat)from->d4;
	return to;
};