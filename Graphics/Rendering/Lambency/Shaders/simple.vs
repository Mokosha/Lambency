attribute vec3 vertexPosition_modelspace;

void main() {
  gl_Position.xyz = vertexPosition_modelspace + vec3(0.1, 0.0, 0.0);
  gl_Position.w = 1.0;
}