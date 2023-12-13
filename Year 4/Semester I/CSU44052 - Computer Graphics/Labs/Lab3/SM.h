#pragma once
#include <windows.h>

class SM
{
public:
	/// <summary>
	/// Update the global delta value on each frame.
	/// </summary>
	static void updateDelta();

	/// <summary>
	/// delta time
	/// </summary>
	static float delta; 
	
	/// <summary>
	/// frames per second
	/// </summary>
	static float fps; 

	static const float gravity;
	static const float terminal_velocity;

	static int width;
	static int height;
	static int frames;
private:
};

