varying vec2 uv;
varying vec3 normal;
varying vec3 pos;

uniform sampler2D diffuseTex;

uniform vec3 ambient;
uniform vec3 lightPos;
uniform vec3 lightDir;

void main() {

  vec3 p2l = pos-lightPos;
  float dist = length(p2l);
  p2l = normalize(p2l);

  float cosToPt = dot(lightDir, p2l);
  // float fDif = 1.0-spotLight.fConeCosine;
  float spot = clamp(cosToPt, 0.0, 1.0);

  const vec3 color = vec3(1.0, 1.0, 1.0);
  vec3 lightColor = ambient + clamp(spot*10000.0, 0.0, 1.0)*color*(spot/(0.8*dist));

  float d = max(0.0, dot(-lightDir, normal));
  gl_FragColor = vec4(lightColor*d*(texture2D(diffuseTex, uv).xyz), 1.0);
}