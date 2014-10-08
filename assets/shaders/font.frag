varying vec2 uv;

uniform sampler2D maskTex;
uniform float alpha;
uniform vec3 color;

void main() {
  float al = alpha * texture2D(maskTex, uv).a;
  gl_FragColor = vec4(color, al);
}