phong illumination model
- ambient term (why)


### Vector Magnitude
#### *CG2, pg24*
$$||v|| = \sqrt{v_{1}^{2}+v_{2}^{2}+...+v_{n}^{2}}$$
e.g.,
$$\begin{Vmatrix}1\\3\\1\end{Vmatrix} = \sqrt{1^{2}+3^{2}+1^{2}} = \sqrt{11}$$

### Normalised Vector
#### *CG2, pg25*
To normalise a vector, divide it by its magnitude.
$$v' = \frac{v}{||v||} = \frac{1}{\sqrt{v_{1}^{2}+v_{2}^{2}+...+v_{n}^{2}}}v$$
### Dot Product
#### *CG2, pg27*
The dot product of 2 vectors gives a scalar. It returns angles. It's also the *length of the projection of $A$ onto $B$*. 
$$u \cdot v = u^{T}v = \begin{bmatrix}u_{1}&u_{2}&u_{3}\end{bmatrix} \begin{bmatrix}v_{1}\\ v_{2}\\ v_{3}\end{bmatrix} = u_{1}v_{1}+u_{2}v_{2}+u_{3}v_{3}$$
The dot product of $u \cdot u$ is the square of its magnitude. Therefore, magnitude can be written as the square root of the dot product of a vector against itself.
$$u \cdot u = u_{1}+u_{2}+u_{3} = ||u||^2$$
$$||u|| = \sqrt{u \cdot u}$$
The angle can be found with:
$$\theta = cos^{-1}\left[\frac{u \cdot v}{||u||\cdot||v||}\right]$$

### Cross Product
#### *CG2, pg35*
The cross product of two vectors gives a vector. It calculates direction. Graphically, the cross product returns a vector that is orthogonal to the plane formed by the two input vectors.
$$u \times v = \begin{bmatrix}u_{1}\\ u_{2}\\ u_{3}\end{bmatrix} \times \begin{bmatrix}v_{1}\\ v_{2}\\ v_{3}\end{bmatrix} = \begin{bmatrix}u_{2}v_{3} - u_{3}v_{2}\\ u_{3}v_{1} - u_{1}v_{3} \\ u_{1}v_{2} - u_{2}v_{1} \end{bmatrix} = w$$
The cross product is anti-commutative: $u \times v = -(v \times u$)
It is *not* associative: $u \times (v \times w) \neq (u \times v) \times w$

### Normals and Polygons
#### *CG2, pg41*
Given two vectors that connect at a vertex $v_{1}$ in a polygon $u_1$ and $u_2$ (see picture below):
$$u_{1}= \frac{v_{2}-v_{1}}{||v_{2}-v_{1}||}, \ u_{2}= \frac{v_{3}-v_{1}}{||v_{3}-v_{1}||}$$
The polygon (surface) normal is give by:
$$n = \frac{u_{2}\times u_1}{||u_{2}\times u_{1}||}$$
![[Pasted image 20231108214046.png|500]]

#### *CG9, pg9-10*
The vertex normal is the average of all normals of the polygons connected to this vertex. Alternatively, it is the result of normalising the sum of said normals, since dividing by a scalar does not change its direction.
![[Pasted image 20231108215656.png|500]]

### 

## Questions
**q1: Why is an ambient term used in the Phong Illumination Model?**
**Answer:** The Phong illumination model uses local illumination, which doesn't take into account reflected light from other sources. The ambient term is introduced to make for the potential low amount of light.
*CG4, pg20*


![[Pasted image 20231108175819.png|800]]
**answer: (c)** *CG4, pg36, CG3, pg37*
Microscopic geometry

![[Pasted image 20231108175828.png|800]]
**Answer: (c)** *CG3 pg42*
Bi-directional Reflectance Distribution Function

![[Pasted image 20231108175947.png|500]]
**Answer: (a)** *CG3, pg7*
Motion

![[Pasted image 20231108180313.png|800]]
**answer: (d)** *CG3, pg8*
Global illumination

![[Pasted image 20231108180329.png|800]]
**Answer: calculate average of normals** *CG9, pg10*
$$
\frac{
\begin{bmatrix}-0.7\\ 3.9\\ -0.2\end{bmatrix} +
\begin{bmatrix}-0.7\\ 3.9\\ 0.2\end{bmatrix} +
\begin{bmatrix}0.3\\ 4.1\\ 0\end{bmatrix} +
\begin{bmatrix}-1.7\\ 3.8\\ 0\end{bmatrix}
}{4} = 
\frac{
\begin{bmatrix}-2.8\\ 15.7\\ 0\end{bmatrix}
}{4} = 
\begin{bmatrix}-0.7\\ 3.925\\ 0\end{bmatrix}
$$

