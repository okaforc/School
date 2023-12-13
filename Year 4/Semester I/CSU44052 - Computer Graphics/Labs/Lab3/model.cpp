#include "model.h"
#include <iostream>
#include <stack>
using namespace std;

Model::Model() {
	mesh_name = "";
}

Model::Model(string mname) {
	mesh_name = mname;
}

Model::Model(string nm, string mname, string tname, unsigned int n) {
	name = nm;
	mesh_name = mname;
	texture_name = tname;
	parent_index = n;
}


void Model::setTranslate(float _x, float _y, float _z) {
	translate.x = _x;
	translate.y = _y;
	translate.z = _z;
}

void Model::setTranslate(vec3 v) {
	translate.x = v.v[0];
	translate.y = v.v[1];
	translate.z = v.v[2];
}

void Model::setRotate(vec3 v) {
	rotate.x = v.v[0];
	rotate.y = v.v[1];
	rotate.z = v.v[2];
}

void Model::setTranslate(Translate t) {
	this->translate = t;
}

vec3 Model::getTranslate() {
	return vec3(translate.x, translate.y, translate.z);
}

void Model::addChild(Model *m) {
	children.push_back(m);
}

Model* Model::getChild(string nm) {
	if (name == nm) return this;
	return getChildRecur(nm);
}

// DFS to find a child with the name nm;
Model* Model::getChildRecur(string nm) {
	map<string, bool> visited = map<string, bool>();
	stack<Model*> s = stack <Model*>();
	s.push(this);
	while (!s.empty()) {
		Model *n = s.top();
		if (n->name == nm) return n;
		s.pop();
		if (!visited[n->name]) {
			visited[n->name] = true;
		}

		for (int i = 0; i < n->children.size(); i++) {
			if (!visited[n->children[i]->name]) s.push(n->children[i]);
		}
	}
	return NULL;
}
