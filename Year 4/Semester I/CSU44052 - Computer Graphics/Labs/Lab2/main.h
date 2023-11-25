#pragma once
#include "Shader.h"
#include "ObjectBuffer.h"
#include <glm/mat3x3.hpp>
#include <glm/ext.hpp>
#include <stdlib.h>
#include <winuser.h>
#include <fstream>
#include <string>

void display();

vec2 posToRelative(vec2 v);

void processMouseMovedNP(int x, int y);

void processMouse(int a, int b, int c, int d);

void processNormalKeys(unsigned char key, int x, int y);

void processCharKeys(unsigned char key, int x, int y);

void init();


std::string readFile(const char* path) {
	ifstream file(path);
	if (!file.is_open()) {
		std::cerr << "Failed to open file." << endl;
		return "";
	}

	string text, line;
	while (getline(file, line)) {
		text += line + "\n";
	}

	file.close();
	text.append("\0");
	return text;
}
