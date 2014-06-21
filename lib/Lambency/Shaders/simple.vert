attribute vec3 position;
attribute vec2 texCoord;

uniform mat4 mvpMatrix;
uniform mat2 texCoordMatrix;

varying vec2 uv;

void main() {
  uv = texCoordMatrix * texCoord;
  gl_Position = mvpMatrix * vec4(position, 1.0);
}