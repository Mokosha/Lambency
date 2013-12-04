attribute vec3 position;
uniform mat4 mvpMatrix;
void main() {
  vec4 clipSpace = mvpMatrix * vec4(position, 1.0);
  gl_Position = clipSpace / clipSpace.w;
}
