#include "SM.h"
#include <iostream>

int SM::width = 1024;
int SM::height = 600;

float SM::delta = 0.0f;
float SM::fps = 0;
int SM::frames = 0;

const float SM::gravity = 0.4f;
const float SM::terminal_velocity = 5.0f;

DWORD lastTime = timeGetTime();
void SM::updateDelta() {
	static DWORD last_time = 0;
	DWORD curr_time = timeGetTime();
	if (last_time == 0)
		last_time = curr_time;
	frames++;
	//std::cout << (curr_time - lastTime) << std::endl;
	if ((curr_time - lastTime)*0.001f >= 1.0f) {
		fps = 1000.0f / ((float)frames);
		//std::cout << frames << std::endl;
		frames = 0;
		lastTime = curr_time;
	}
	delta = (curr_time - last_time) * 0.001f;
	last_time = curr_time;
}