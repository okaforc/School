#include "model.h"

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
	this->translate.x = _x;
	this->translate.y = _y;
	this->translate.z = _z;
}

void Model::setTranslate(Translate t) {
	this->translate = t;
}

void Model::addChild(Model *m) {
	children.push_back(m);
}

Model* Model::getChild(string nm) {
	unsigned int n = children.size();

	for (int i = 0; i < n; i++)
	{
		if (children[i]->name == nm) {
			return children[i];
		}
	}
	return NULL;
}
