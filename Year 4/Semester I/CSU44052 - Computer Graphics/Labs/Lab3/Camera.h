#pragma once
#include "maths_funcs.h"
#include "util.h"
#include "sm.h"
#include <GL/glew.h>
#include <GL/freeglut.h>
#include <glm/vec3.hpp>
#include <glm/gtc/matrix_transform.hpp>

class Camera
{
public:
	Camera() {
		pos = vec3(0.0f, 5.0f, -10.0f);
		front = vec3(0.0f, 0.0f, -1.0f);
		up = vec3(0, 1, 0);
	}

	/// <summary>
	/// Process and update the camera's view matrix. The mouse's x- and y-coordinates are used to dictate the direction the player is looking.
	/// </summary>
	/// <param name="x">The x direction the player is looking in.</param>
	/// <param name="y">The y direction the player is lookng in.</param>
	void processView(int, int);

	/// <summary>
	/// Process camera movement through the world space.
	/// <para>
	/// If the player cannot fly, their movement should no longer move in the y-axis. However, simply disabling flying creates issues, as the y-axis is still
	/// taken into account when normalising movement. To prevent this, we can ignore the y-axis of the camera target changing position before normalising.
	/// </para>
	/// </summary>
	void processMovement();

	/// <summary>
	/// Get the look_at position of the camera.
	/// </summary>
	/// <returns>The view matrix of the camera.</returns>
	mat4 getViewMatrix();

	/// <summary>
	/// Get the lookAt position of the camera using GLM primitives.
	/// </summary>
	/// <returns>The view matrix of the camera as a GLM::mat4</returns>
	glm::mat4 getViewMatrixGLM();


	// Variables
	vec3 pos; // Camera position
	vec3 front; // Camera direction
	vec3 up; // Camera up direction
	vec3 wUP = vec3(0.0f, 1.0f, 0.0f); // World up
	vec3 right; // Camera right direction
	float base_FOV = 60.0f; // Normal (minimum) field of view
	float max_FOV = 85.0f; // Maximum field of view
	float FOV = base_FOV; // Camera field of view
	float pitch; // x-axis rotation (vertical)
	float yaw; // y-axis rotation (horizontal)
	float roll; // z-axis rotation 
	float sensitivity = 0.15f; // Camera sensitivity 
	float baseSpeed = 5.0f; // Walking speed (movement speed)
	float sprintSpeed = 25.0f; // Sprinting speed (movement speed)
	float speed = baseSpeed; // Camera speed (movement speed)
	float falling_velocity = 0.0f; // Camera speed (movement speed)

	// Keyboard movement
	bool FORWARD = false; // z+
	bool BACK = false; // z-
	bool LEFT = false; // x-
	bool RIGHT = false; // x+
	bool UP = false; // y+
	bool DOWN = false; // y-
	bool CAN_FLY = true; // can the player fly (i.e., move on y-axis at will)?
	bool SPRINT = false; // is the player sprinting?
	bool CAN_FALL = false; // can the player fall (i.e., be affected by gravity)?
};

