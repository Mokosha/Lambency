attribute vec3 position;
attribute vec2 texCoord;
attribute vec3 norm;

uniform mat4 mvpMatrix;
uniform mat4 m2wMatrix;

varying vec2 uv;
varying vec3 normal;

void main() {

  uv = texCoord;
  normal = normalize((m2wMatrix * vec4(norm, 0)).xyz);

  vec4 clipSpace = mvpMatrix * vec4(position, 1.0);
  gl_Position = clipSpace / clipSpace.w;
}