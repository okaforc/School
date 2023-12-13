#pragma once

#include <string>
#include <vector>
#include <iostream>
#include <GL/glew.h>

class Texture
{
public:
	Texture(GLenum); // create from memory buffer
	Texture(const std::string, GLenum); // create from file
	Texture(const std::vector<std::string>, GLenum); // create from files
	bool load(); // load from file(s)
	bool load(unsigned int, void*); // load from memory buffer
	bool loadCubemap(std::vector<std::string> faces);
	void bind(GLenum textureUnit);
	void bind(GLenum textureType, GLenum textureUnit);

	std::string file_name;
	std::vector<std::string> cubemap_file_names;
	GLenum textureEnum;
	unsigned int texture;
	int _width, _height, _nrChannels, _bits_per_pixel;
};

