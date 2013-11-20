varying vec2 uv;
varying vec3 normal;

uniform sampler2D diffuseTex;

uniform vec3 ambient;
uniform vec3 lightPos;
uniform vec3 lightDir;

void main() {
  float d = max(0.0, dot(-lightDir, normal));
  vec3 mod = clamp(vec3(d, d, d) + ambient, 0.0, 1.0);
  gl_FragColor = vec4(mod*(texture2D(diffuseTex, uv).xyz), 1);
}