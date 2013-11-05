attribute vec3 vertexPosition_modelspace;
uniform mat4 mvpMatrix;

void main() {
  vec4 pos = vec4(vertexPosition_modelspace, 1.0);
  vec4 clipSpace = mvpMatrix * pos;
  gl_Position = clipSpace / clipSpace.w;
}