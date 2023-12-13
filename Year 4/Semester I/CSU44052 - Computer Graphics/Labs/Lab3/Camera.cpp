#include "Camera.h"

/// <summary>
/// Process and update the camera's view matrix. The mouse's x- and y-coordinates are used to dictate the direction the player is looking.
/// </summary>
/// <param name="x">The x direction the player is looking in.</param>
/// <param name="y">The y direction the player is lookng in.</param>
void Camera::processView(int x, int y) {
	float xMid = SM::width / 2.0;
	float yMid = SM::height / 2.0;
	pitch = Util::clamp(pitch + ((y - yMid) * -sensitivity), -89.0f, 89.0f);
	yaw = Util::wrap(yaw + ((x - xMid) * sensitivity), 0.0f, 360.0f);
	glutWarpPointer(xMid, yMid); // prevent mouse from leaving the centre of the screen

	front = normalise(vec3(
		cos(Util::deg2Rad(yaw)) * cos(Util::deg2Rad(pitch)),
		sin(Util::deg2Rad(pitch)),
		sin(Util::deg2Rad(yaw)) * cos(Util::deg2Rad(pitch))
	));

	right = normalise(cross(front, wUP));
	up = normalise(cross(right, front));
}
/// <summary>
/// Process camera movement through the world space.
/// <para>
/// If the player cannot fly, their movement should no longer move in the y-axis. However, simply disabling flying creates issues, as the y-axis is still
/// taken into account when normalising movement. To prevent this, we can ignore the y-axis of the camera target changing position before normalising.
/// </para>
/// </summary>
void Camera::processMovement() {
	float t_cpos_y = pos.v[1]; // y-pos of camera before updates
	if (FORWARD) {
		if (CAN_FLY) pos += normalise(front) * speed * SM::delta;
		else pos += normalise(vec3(front.v[0], 0, front.v[2])) * speed * SM::delta;
	}
	if (BACK) {
		if (CAN_FLY) pos -= normalise(front) * speed * SM::delta;
		else pos -= normalise(vec3(front.v[0], 0, front.v[2])) * speed * SM::delta;
	}
	if (LEFT) {
		if (CAN_FLY) pos -= normalise(cross(front, up)) * speed * SM::delta;
		else {
			vec3 c = cross(front, up);
			pos -= normalise(vec3(c.v[0], 0, c.v[2])) * speed * SM::delta;
		}
	}
	if (RIGHT) {
		if (CAN_FLY) pos += normalise(cross(front, up)) * speed * SM::delta;
		else {
			vec3 c = cross(front, up);
			pos += normalise(vec3(c.v[0], 0, c.v[2])) * speed * SM::delta;
		}
	}
	if (UP) {
		pos += vec3(0, speed * SM::delta, 0);
	};
	if (DOWN) {
		pos -= vec3(0, speed * SM::delta, 0);
	};

	int lerp_speed = 180;
	speed = SPRINT ? sprintSpeed : baseSpeed;
	// if sprinting (and moving), change the FOV on start and when stopped.
	FOV = (SPRINT && (UP || DOWN || LEFT || RIGHT || FORWARD || BACK)) ? 
		Util::lerp(FOV, base_FOV, max_FOV, SM::delta * lerp_speed) : 
		Util::lerp(FOV, base_FOV, max_FOV, SM::delta * -lerp_speed)
	;
	if (!CAN_FLY && !CAN_FALL) pos.v[1] = t_cpos_y; // if can't fly or fall, don't change y_pos
	else if (!CAN_FLY && CAN_FALL) {
		falling_velocity = Util::lerp(falling_velocity, 0.0f, SM::terminal_velocity, SM::delta * SM::gravity);
		pos.v[1] -= falling_velocity;
	}
	else falling_velocity = 0.0f;
		//pos.v[1] -= SM::gravity * SM::delta; // if can fall but not fly
}

/// <summary>
/// Get the look_at position of the camera.
/// </summary>
/// <returns>The view matrix of the camera.</returns>
mat4 Camera::getViewMatrix() {
	return look_at(pos, pos + front, up);
}

/// <summary>
/// Get the lookAt position of the camera using GLM primitives.
/// </summary>
/// <returns>The view matrix of the camera as a GLM::mat4</returns>
glm::mat4 Camera::getViewMatrixGLM() {
	glm::vec3 tpos = glm::vec3(pos.v[0], pos.v[1], pos.v[2]);
	glm::vec3 tfront = glm::vec3(front.v[0], front.v[1], front.v[2]);
	glm::vec3 tup = glm::vec3(up.v[0], up.v[1], up.v[2]);
	return glm::lookAt(tpos, tpos + tfront, tup);
}