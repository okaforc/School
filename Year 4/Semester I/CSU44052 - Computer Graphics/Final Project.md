## Specs
Your post-apocalyptic visualization of Dublin City can take any form you like, but the project must
have the following basic functionality:
1. 3-dimensional objects and views (note: fixed top-down orthographic view not allowed).
2. User interaction and camera-control
	1. user should be able to move around the scene using the keyboard and/or the mouse. At a minimum, implement moving forwards and backwards, turning left and turning right;
3. A Hierarchical animated Zombie/Robot etc. character or object relevant to the theme;
4. A crowd of moving Zombies/Robots etc.. characters or objects relevant to the theme;
	1. The crowd can be implemented in any way that you like. At a minimum, the crowd members should be translating around the scene, e.g., forwards and backwards. (A crowd simulation algorithm such as boids can be implemented as an Advanced Feature and having a hierarchical character for each crowd member is also an Advanced Feature).
5. Texture-mapping your scene and creatures using image files (e.g., jpg);
6. Implementation of the Phong Illumination model. Specular highlights must be visible;
	1. Multiple light sources (at least 2, can be point, directional, or spotlight)
	2. Multiple different material properties (at least 5 on 5 different objects – e.g., you could have several crowd characters with different material properties)
	3. Normal must be transformed correctly
	4. Shading must use a combination of ambient, diffuse, and specular lighting

***This required functionality is worth up to 70% of the project mark***

The final 30% will be given for advanced features, with approximately 15% per perfectly-executed feature – simple features will receive less, whereas elaborate or well-designed features will receive more.
Advanced Features can include the following, or indeed others that you think of:
- More complex boids/crowd animation
- Hierarchical creatures that move realistically in your crowd
- Simulated dynamics or physics of any sort
- Intelligent Characters
- Great models that you made yourself
- Height-mapped terrain
- Procedurally generated terrain or meshes
- More advanced texturing effects; multi-texturing, environment-mapping, bump-mapping, specular-mapping, etc.
- ??? – your own feature ideas

**Note**: The marking scheme provided shows the maximum marks that can be obtained for each section if completed perfectly. Merely attempting a section does not imply the full score indicated

## Idea
O'Connell Street
A crowd of zombies (red) tear through the streets, moving things in their path. They reach the Spire, and push against it for a few minutes, causing it to collapse and scatter an opposing group of zombies (blue). The two groups meet and fight. You, the character, can move through the environment, hiding in buildings and watching through windows and doors.
### Checklist
- [ ] Character movement
- [ ] At least 2 crowds of zombies
- [ ] Destruction of the Spire
- [ ] Entering buildings
- [ ] Toggleable flashlight

## Checklist
- [ ] 3-dimensional objects and views (note: fixed top-down orthographic view not allowed).
- [ ] User interaction and camera-control
	- [ ] user should be able to move around the scene using the keyboard and/or the mouse. At a minimum, implement moving forwards and backwards, turning left and turning right;
- [ ] A Hierarchical animated Zombie/Robot etc. character or object relevant to the theme;
- [ ] A crowd of moving Zombies/Robots etc.. characters or objects relevant to the theme;
	- [ ] The crowd can be implemented in any way that you like. At a minimum, the crowd members should be translating around the scene, e.g., forwards and backwards. (A crowd simulation algorithm such as boids can be implemented as an Advanced Feature and having a hierarchical character for each crowd member is also an Advanced Feature).
- [ ] Texture-mapping your scene and creatures using image files (e.g., jpg);
- [ ] Implementation of the Phong Illumination model. Specular highlights must be visible;
	- [ ] Multiple light sources (at least 2, can be point, directional, or spotlight)
	- [ ] Multiple different material properties (at least 5 on 5 different objects – e.g., you could have several crowd characters with different material properties)
	- [ ] Normal must be transformed correctly
	- [ ] Shading must use a combination of ambient, diffuse, and specular lighting









## To-do List
- [x] 23/11
	- [x] Load and render multiple distinct meshes
- [ ] 24/11: 
	- [ ] Animate multiple meshes
		- [ ] idea 1: animate everything on cycles multiplied by per-model factors. have several different cycle clocks running in the background, some slow, some fast, some sinusoidal, some bezier, etc.
		- [x] idea 2: animate individual submodels manually
	- [x] Add textures
	- [x] Add distinct textures
- [ ] 25/11
	- [ ] lights (phong)
	- [ ] recursive hierarchal search function for getChild
	- [x] move in real direction when CAN_FLY is false (i.e., move direction is not normalised for x-/z-axis if y-axis is locked)
- [ ] 26/11
	- [ ] Large base model in Blender

