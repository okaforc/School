#pragma once
#include <GLM/vec3.hpp>
#include <GLM/vec2.hpp>
#include <GL/glew.h>
#include <GL/freeglut.h>
#include <vector>
#include <iostream>

class Shader
{
public:
	GLuint ID = 0;
	Shader() {}
	Shader(static const char* pVS, static const char* pFS) {
		ID = CompileShaders(pVS, pFS);
	}

	void AddShader(GLuint ShaderProgram, const char* pShaderText, GLenum ShaderType)
	{
		// Create a shader object
		GLuint ShaderObj = glCreateShader(ShaderType);

		if (ShaderObj == 0)
		{
			fprintf(stderr, "Error creating shader type %d\n", ShaderType);
			exit(0);
		}
		// Bind the source code to the shader, this happens before compilation
		glShaderSource(ShaderObj, 1, (const GLchar**)&pShaderText, NULL);
		// Compile the shader and check for errors
		glCompileShader(ShaderObj);
		GLint success;
		// Check for shader related errors using glGetShaderiv
		glGetShaderiv(ShaderObj, GL_COMPILE_STATUS, &success);
		if (!success)
		{
			GLchar InfoLog[1024];
			glGetShaderInfoLog(ShaderObj, 1024, NULL, InfoLog);
			fprintf(stderr, "Error compiling shader type %d: '%s'\n", ShaderType, InfoLog);
			exit(1);
		}
		// Attach the compiled shader object to the program object
		glAttachShader(ShaderProgram, ShaderObj);
	}

	GLuint CompileShaders(static const char* pVS, static const char* pFS)
	{
		// Start the process of setting up our shaders by creating a program ID
		// Note: we will link all the shaders together into this ID
		GLuint shaderProgramID = glCreateProgram();
		if (shaderProgramID == 0)
		{
			fprintf(stderr, "Error creating shader program\n");
			exit(1);
		}

		// Create two shader objects, one for the vertex, and one for the fragment shader
		AddShader(shaderProgramID, pVS, GL_VERTEX_SHADER);
		AddShader(shaderProgramID, pFS, GL_FRAGMENT_SHADER);

		GLint Success = 0;
		GLchar ErrorLog[1024] = { 0 };


		// After compiling all shader objects and attaching them to the program, we can finally link it
		glLinkProgram(shaderProgramID);
		// Check for program related errors using glGetProgramiv
		glGetProgramiv(shaderProgramID, GL_LINK_STATUS, &Success);
		if (Success == 0)
		{
			glGetProgramInfoLog(shaderProgramID, sizeof(ErrorLog), NULL, ErrorLog);
			fprintf(stderr, "Error linking shader program: '%s'\n", ErrorLog);
			exit(1);
		}

		// Program has been successfully linked but needs to be validated to check whether the program can execute given the current pipeline state
		glValidateProgram(shaderProgramID);
		// Check for program related errors using glGetProgramiv
		glGetProgramiv(shaderProgramID, GL_VALIDATE_STATUS, &Success);
		if (!Success)
		{
			glGetProgramInfoLog(shaderProgramID, sizeof(ErrorLog), NULL, ErrorLog);
			fprintf(stderr, "Invalid shader program: '%s'\n", ErrorLog);
			exit(1);
		}
		// Finally, use the linked shader program
		// Note: this program will stay in effect for all draw calls until you replace it with another or explicitly disable its use
		//glUseProgram(shaderProgramID);
		return shaderProgramID;
	}

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
