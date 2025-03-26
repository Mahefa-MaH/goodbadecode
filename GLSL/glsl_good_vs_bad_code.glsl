// Good Code: Calculating the distance to the camera in a vertex shader.
#version 330 core
layout (location = 0) in vec3 aPos;
uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

out vec3 distToCamera;

void main() {
    vec4 worldPos = model * vec4(aPos, 1.0);
    vec4 viewPos = view * worldPos;
    distToCamera = vec3(viewPos.xyz); //only xyz as w is 1
    gl_Position = projection * viewPos;
}


// Bad Code:  Unnecessary calculations and inefficient use of variables.
#version 330 core
layout (location = 0) in vec3 aPos;
uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

out vec3 distToCamera;

void main() {
    vec4 worldPos = model * vec4(aPos, 1.0);
    vec4 viewPos = view * worldPos;
    vec4 temp = viewPos *2.0; //unnecessary operation
    vec4 temp2 = temp/2.0; //unnecessary operation
    distToCamera = vec3(temp2.xyz);
    gl_Position = projection * viewPos;
}
