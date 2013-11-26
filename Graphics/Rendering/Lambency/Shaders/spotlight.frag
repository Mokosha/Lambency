varying vec2 uv;
varying vec3 norm;
varying vec3 pos;

uniform sampler2D diffuseTex;

uniform vec3 ambient;
uniform vec3 lightPos;
uniform vec3 lightDir;

uniform sampler2D shadowMap;
uniform mat4 shadowVP;

void main() {

  vec4 lightPersp = shadowVP * vec4(pos, 1);
  lightPersp /= lightPersp.w;
  lightPersp = lightPersp * 0.5 + 0.5;
  float depth = texture2D(shadowMap, lightPersp.xy).z;
  float shadow = float(lightPersp.z > 0.001+depth);

  vec3 p2l = pos-lightPos;
  float dist = length(p2l);
  p2l = normalize(p2l);

  float cosToPt = dot(lightDir, p2l);
  float spot = clamp(cosToPt, 0.0, 1.0);

  const vec3 color = vec3(1.0, 1.0, 1.0);
  vec3 lightColor = ambient;
  if(spot > 0.0) {
    lightColor += color*(spot/(0.1*dist));
  }

  float d = max(0.0, dot(-lightDir, norm)) * (1.0 - 0.5*shadow);
  gl_FragColor = vec4(lightColor*d*(texture2D(diffuseTex, uv).xyz), 1.0);
}