# Ray Tracer in Haskell

  

This is my attempt at creating a ray tracer in Haskell for the function programming competition for INF1-A Introduction to Computation at University of Edinburgh. Since, I don't have any experience with ray tracing or even intermediate level computer graphics, this ray tracer was written following along with the excellent [Ray Tracing in One Weekend](https://raytracing.github.io/books/RayTracingInOneWeekend.html) book by Peter Shirley. 

### Description

The ray-tracer in it's current state generates a random scene of spheres using the current time as the seed. The scene has 3 fixed sphere to demonstrate the three implemented materials [Lambertian](https://en.wikipedia.org/wiki/Lambertian_reflectance), Metal and Dielectric; Around these 3 spheres, smaller spheres are generated randomly and are assigned random colors/materials. There are more lambertian spheres amongst the smaller spheres to reduce the complexity of the scene and time taken for rendering. 

The output file is in [ppm](http://netpbm.sourceforge.net/doc/ppm.html#:~:text=The%20PPM%20format%20is%20a%20lowest%20common%20denominator%20color%20image%20file%20format.&text=For%20example%2C%20"PPM%20using%20the,also%20called%20"portable%20pixmaps.") as it is the easiest file format to handle which i could find. However, you can use ffmpeg to convert it into a png quite easily using 
```
ffmpeg -i filename.ppm filename.png
```

### Running

For compiling
```
cabal build -O2
``` 
For running the executable
```
cabal run ray-tracer width samplesPerPixel raysPerSample
```
where width stands for width of image, samplesperPixel is number of samples the program takes for each pixel and raysPerPixel is the number of rays required to compute each sample.

### Todo

I had a couple more features that i wanted to implement before submitting this, however i ran out of time and had to scrap them. I hope eventually i'll be able to come back to this project and implement most if not all of these.

 - UV Mapping for custom png textures
 - Rendering for other 3d shapes like cubes, cylinders etc (very close to this but intersections between different shapes would require a rewrite of how i handle intersections)
 - Further Optimization. In it's current naive state with very basic optimisation using the [wiki page](https://wiki.haskell.org/Performance/GHC), the code is very slow and can run faster. 
 -  Custom Light Sources
 - Introducting modifiable density mediums to render fogs/clouds/smoke
 - Possibly look into rendering .obj files directly
 - Implement motion blurring and create ray-traced animations 

### Sources

Overview:
[_Ray Tracing in One Weekend_](https://raytracing.github.io/books/RayTracingInOneWeekend.html)
[_Ray Tracing: The Next Week_](https://raytracing.github.io/books/RayTracingTheNextWeek.html)

Material based rendering:
[Material Shading (Reflection, Refraction and Fresnel Surfaces)](https://www.scratchapixel.com/lessons/3d-basic-rendering/introduction-to-shading/reflection-refraction-fresnel)
[UV Mapping](https://conceptartempire.com/uv-mapping-unwrapping/)
[Dielectric Materials](http://viclw17.github.io/2018/08/05/raytracing-dielectric-materials/)

More Theoretical and Mathematical understanding: 
[Course Materials from Computer Graphics (CMU 15-462/662) at CMU ](http://15462.courses.cs.cmu.edu/fall2021/)
[Lecture notes and Course Materials from Computer Graphics (MIT 6-837) at MIT](https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-837-computer-graphics-fall-2012/lecture-notes/)

Bence Szilagyi for coding/math/general help throughout the process of writing this project.
The School of Informatics for DICE, as without them i would not be able to render any high resolution pictures. 

