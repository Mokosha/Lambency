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

float rnd(vec2 co)
{
  return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
}

// Fix values from their floating point representation
// to what they should be in the interval [0,255] based
// on a stochastic dithering pattern where the dithering
// is done based on the proximity to an integer value in
// the aforementioned interval.
float dither(float v, float r) {
  float val = v * 255.0;
  float ival = floor(val);
  float diff = val - ival;
  return (ival + float(r < diff)) / 255.0;
}

vec3 dither3(vec3 v, vec2 seed) {
  float r = rnd(seed);
  return vec3(dither(v.x, r), dither(v.y, r), dither(v.z, r));
}

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
  finalColor = dither3(finalColor, uv);

  gl_FragColor = vec4(finalColor, 1.0);
}
