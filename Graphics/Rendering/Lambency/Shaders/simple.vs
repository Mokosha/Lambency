attribute vec3 vertexPosition_modelspace;
attribute vec2 vertexTexCoord;

uniform mat4 mvpMatrix;

varying vec2 uv;

void main() {

  uv = vertexTexCoord;

  vec4 pos = vec4(vertexPosition_modelspace, 1.0);
  vec4 clipSpace = mvpMatrix * pos;
  gl_Position = clipSpace / clipSpace.w;
}