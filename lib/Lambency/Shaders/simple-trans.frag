varying vec2 uv;

uniform sampler2D diffuseTex;
uniform float alpha;

void main() {
  gl_FragColor = texture2D(diffuseTex, uv);
  gl_FragColor.a = alpha;
}