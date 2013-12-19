varying vec2 uv;
varying vec3 norm;
varying vec3 pos;

uniform sampler2D diffuseTex;

uniform vec3 ambient;
uniform vec3 lightPos;
uniform vec3 lightDir;
uniform float lightCosCutoff;

uniform sampler2DShadow shadowMap;
uniform mat4 shadowVP;

float bias(float d) {
  return d - 0.0001;
}

void main() {

  vec4 lightPersp = shadowVP * vec4(pos, 1);
  lightPersp /= lightPersp.w;
  lightPersp = lightPersp * 0.5 + 0.5;
  lightPersp.z = bias(lightPersp.z);
  float shadow = float(shadow2D(shadowMap, lightPersp.xyz));

  vec3 p2l = pos-lightPos;
  float dist = length(p2l);
  p2l = normalize(p2l);

  float cosToPt = dot(lightDir, p2l);
  float spot = clamp(cosToPt, lightCosCutoff, 1.0);

  const vec3 color = vec3(1.0, 1.0, 1.0);
  vec3 lightColor = vec3(0, 0, 0);
  if(spot > 0.0) {
    lightColor += color*(spot/(0.1*dist));
  }

  // diffuse
  lightColor *= max(0.0, dot(-lightDir, norm)) * (1.0 - 0.5*shadow);

  // ambient
  lightColor += ambient;
  vec3 finalColor = lightColor*texture2D(diffuseTex, uv).xyz;

  gl_FragColor = vec4(finalColor, 1.0);
}
