attribute vec3 position;
attribute vec2 texCoord;

uniform mat4 mvpMatrix;

varying vec2 uv;

void main() {

  uv = texCoord;

  vec4 pos = vec4(position, 1.0);
  vec4 clipSpace = mvpMatrix * pos;
  gl_Position = clipSpace / clipSpace.w;
}