varying vec2 uv;
varying vec3 normal;
varying vec3 pos;

uniform sampler2D diffuseTex;

uniform vec3 ambient;
uniform vec3 lightPos;
uniform vec3 lightDir;

void main() {

  float dist = distance(pos, lightPos);

  vec3 dir = pos-lightPos;
  dir = normalize(dir);

  float c = dot(lightDir, dir);
  // float fDif = 1.0-spotLight.fConeCosine;
  float fFactor = clamp(c, 0.0, 1.0);

  vec4 lightColor = clamp(c*10000.0, 0.0, 1.0)*vec4(1.0, 1.0, 1.0, 1.0)*fFactor/dist;

  float d = max(0.0, dot(-lightDir, normal));
  vec3 mod = clamp(vec3(d, d, d) + ambient, 0.0, 1.0);
  gl_FragColor = lightColor*vec4(mod*(texture2D(diffuseTex, uv).xyz), 1);
}