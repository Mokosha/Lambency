attribute vec3 position;
attribute vec2 texCoord;

uniform mat4 mvpMatrix;

varying vec2 uv;

void main() {
  uv = texCoord;
  gl_Position = mvpMatrix * vec4(position, 1.0);
}