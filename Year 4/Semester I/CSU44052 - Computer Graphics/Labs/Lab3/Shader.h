#pragma once
#include <GL/glew.h>
#include <GL/freeglut.h>
#include <GLM/vec3.hpp>
#include <GLM/mat4x4.hpp>
#include <GLM/vec2.hpp>
#include <vector>
#include <string>
#include <iostream>
#include "util.h"

class Shader
{
public:
	GLuint ID = 0;
	std::string name;
	Shader() {}
	Shader(std::string shader_name, const char* vertex_shader_path, const char* fragment_shader_path) {
		name = shader_name;
		ID = CompileShaders(vertex_shader_path, fragment_shader_path);
	}

	void AddShader(GLuint ShaderProgram, const char* pShaderText, GLenum ShaderType);
	GLuint CompileShaders(const char* pVS, const char* pFS);


	// activate the shader
	// ------------------------------------------------------------------------
	void use()
	{
		glUseProgram(ID);
	}
	// utility uniform functions
	// ------------------------------------------------------------------------
	void setBool(const std::string& name, bool value) const
	{
		glUniform1i(glGetUniformLocation(ID, name.c_str()), (int)value);
	}
	// ------------------------------------------------------------------------
	void setInt(const std::string& name, int value) const
	{
		glUniform1i(glGetUniformLocation(ID, name.c_str()), value);
	}
	// ------------------------------------------------------------------------
	void setFloat(const std::string& name, float value) const
	{
		glUniform1f(glGetUniformLocation(ID, name.c_str()), value);
	}
	void setVec2(const std::string& name, vec2 value) const
	{
		//if (name == "light.direction") std::cout << value.v[0] << ", " << value.v[1] << ", " << value.v[2] << std::endl;
		glUniform2f(glGetUniformLocation(ID, name.c_str()), value.v[0], value.v[1]);
	}
	void setVec2(const std::string& name, float x, float y) const
	{
		//if (name == "light.direction") std::cout << value.v[0] << ", " << value.v[1] << ", " << value.v[2] << std::endl;
		glUniform2f(glGetUniformLocation(ID, name.c_str()), x, y);
	}
	void setVec3(const std::string& name, vec3 value) const
	{
		//if (name == "light.direction") std::cout << value.v[0] << ", " << value.v[1] << ", " << value.v[2] << std::endl;
		glUniform3f(glGetUniformLocation(ID, name.c_str()), value.v[0], value.v[1], value.v[2]);
	}
	void setVec3(const std::string& name, float x, float y, float z) const
	{
		//if (name == "light.direction") std::cout << value.v[0] << ", " << value.v[1] << ", " << value.v[2] << std::endl;
		glUniform3f(glGetUniformLocation(ID, name.c_str()), x, y, z);
	}
	void setMat4(const std::string& name, const mat4 &mat) const
	{
		glUniformMatrix4fv(glGetUniformLocation(ID, name.c_str()), 1, GL_FALSE, mat.m);
	}
	void setMat4GLM(const std::string& name, const glm::mat4& mat) const
	{
		glUniformMatrix4fv(glGetUniformLocation(ID, name.c_str()), 1, GL_FALSE, &mat[0][0]);
	}
	void setMat4AI(const std::string& name, const aiMatrix4x4& mat) const
	{
		glUniformMatrix4fv(glGetUniformLocation(ID, name.c_str()), 1, GL_TRUE, &mat.a1);
	}



private:
	// utility function for checking shader compilation/linking errors.
	// ------------------------------------------------------------------------
	void checkCompileErrors(unsigned int shader, std::string type)
	{
		int success;
		char infoLog[1024];
		if (type != "PROGRAM")
		{
			glGetShaderiv(shader, GL_COMPILE_STATUS, &success);
			if (!success)
			{
				glGetShaderInfoLog(shader, 1024, NULL, infoLog);
				std::cout << "ERROR::SHADER_COMPILATION_ERROR of type: " << type << "\n" << infoLog << "\n -- --------------------------------------------------- -- " << std::endl;
			}
		}
		else
		{
			glGetProgramiv(shader, GL_LINK_STATUS, &success);
			if (!success)
			{
				glGetProgramInfoLog(shader, 1024, NULL, infoLog);
				std::cout << "ERROR::PROGRAM_LINKING_ERROR of type: " << type << "\n" << infoLog << "\n -- --------------------------------------------------- -- " << std::endl;
			}
		}
	}
};



//void AddShader(GLuint ShaderProgram, const char* pShaderText, GLenum ShaderType);
//GLuint CompileShaders(static const char* pVS, static const char* pFS);
