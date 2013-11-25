attribute vec3 position;
attribute vec2 texCoord;

uniform mat4 mvpMatrix;

varying vec2 uv;

void main() {

  uv = texCoord;

  vec4 clipSpace = mvpMatrix * vec4(position, 1.0);
  clipSpace = clipSpace / clipSpace.w;
  gl_Position = clipSpace;
}