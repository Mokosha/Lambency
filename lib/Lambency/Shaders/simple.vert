attribute vec3 position;
attribute vec2 texCoord;

uniform mat4 mvpMatrix;
uniform mat3 texCoordMatrix;

varying vec2 uv;

void main() {
  uv = texCoordMatrix * vec3(texCoord, 1.0);
  uv = uv / uv.z; // Perspective correction

  gl_Position = mvpMatrix * vec4(position, 1.0);
}