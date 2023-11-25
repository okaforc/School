# Setting up a new Lab
These are instructions for setting up a new lab for CSU44052 Computer Graphics at Trinity College Dublin. All files have been used with Visual Studio 2022 (Visual Studio Build Tools 2019, in case that's useful) and work as intended as of 2023/09/28.

## 64-bit (x64)
To set up a new project, first create a new "OpenGLLabs" Project from the template. Then, 
- Copy the freeglut, glew, and glm folders in tcd_cg_libs.zip to the base directory of the current project (or just copy them from a previous project)
- In the project Property Pages ([Menu Bar] Project/[ProjectName] Properties), set "Configuration Properties -> Debugging -> Working Directory" to "$(OutDir)"
- In the menu just below the Menu Bar, set the project type to Release and make sure the value next to it is "x64". Build using Ctrl+B, then switch project type to Debug and enter Ctrl+B again (the order of Release and Debug don't matter; just make sure you do both).
- Copy the x64 .dll files found in the DLLs/x64 folder in the zip file to the Debug and Release folders found in the x64 folder in the base (solution) directory, or, alternatively, 
	- freeglut: "freeglut\bin\x64\freeglut.dll"
	- glew: "glew-1.10.0\bin\Release\x64\glew32.dll" (don't mind the file name)
- ur done :3

## 32-bit (x86, win32)
To set up a new project, first create a new "OpenGLLabs_x86" Project from the template. Then, 
- Copy the freeglut, glew, and glm folders in tcd_cg_libs.zip to the base directory of the current project (or just copy them from a previous project)
- In the project Property Pages ([Menu Bar] Project/[ProjectName] Properties), set "Configuration Properties -> Debugging -> Working Directory" to "$(OutDir)"
- In the menu just below the Menu Bar, set the project type to Release and make sure the value next to it is "x86". Build using Ctrl+B, then switch project type to Debug and enter Ctrl+B again (the order of Release and Debug don't matter; just make sure you do both).
- Copy the .dll files found in the DLLs/Win32 folder in the zip file to the Debug and Release folders found in the base (solution) directory, or, alternatively, 
	- freeglut: "freeglut\bin\freeglut.dll"
	- glew: "glew-1.10.0\bin\Release\Win32\glew32.dll"
- ur done :3_x86

## In short...
Debugging -> Working Directory: `$(OutDir)
C/C++ -> General -> Additional Include Directories: `$(SolutionDir)Assimp\include;$(SolutionDir)freeglut\include;$(SolutionDir)glew-1.10.0\include;$(SolutionDir)glm`
Linker -> General -> Additional Library Directories: `$(SolutionDir)Assimp\lib\x64;$(SolutionDir)freeglut\lib\x64;$(SolutionDir)glew-1.10.0\lib\Release\x64`
LInker -> Input: `$(CoreLibraryDependencies);%(AdditionalDependencies);freeglut.lib;glew32.lib;assimp-vc143-mt.lib`


## Adding an OpenGLLabs project template
This is usually found in "C:\Users\[user]\Documents\Visual Studio 2022\Templates\ProjectTemplates". 
	- 32-bit: Add the OpenGLLabs_x86.zip file to your Visual Studio 20XX project templates folder. 
	- 64-bit: Add the OpenGLLabs.zip file to your Visual Studio 20XX project templates folder. 
Restart Visual Studio if it's open.

# Copyright
no clue how this works do what you want with this information lol :3
written by iris onwa (strawberry :P)