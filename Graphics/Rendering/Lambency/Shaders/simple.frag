varying vec2 uv;

uniform sampler2D diffuseTex;

void main() {
  gl_FragColor = texture2D(diffuseTex, uv);
}