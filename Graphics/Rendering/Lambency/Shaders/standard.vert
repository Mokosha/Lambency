attribute vec3 position;
attribute vec3 normal;
attribute vec2 texCoord;

uniform mat4 mvpMatrix;
uniform mat4 m2wMatrix;

varying vec3 pos;
varying vec3 norm;
varying vec2 uv;

void main() {

  pos = (m2wMatrix * vec4(position, 1.0)).xyz;
  norm = normalize((m2wMatrix * vec4(normal, 0)).xyz);
  uv = texCoord;

  gl_Position = mvpMatrix * vec4(position, 1.0);
}