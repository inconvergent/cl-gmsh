#version 420 core

precision highp float;

#define EPS 0.0001
#define MAX_DST 999999.0
#define PI 3.1415925359
#define LIGHT_SAMPLES 2
#define TWO_PI 6.2831852
#define NUM_TRIANGLES 1000

out vec4 frag;
in vec3 glPos;

uniform float time, sampleRate, volumeIntensity;
uniform mat4 viewMat, projMat;
uniform vec2 resolution;
uniform int full;
uniform vec3 cam, look, up;

struct HD { int m_id; int t_id; float t; };
struct Ray { vec3 origin; vec3 direction; vec4 colorIntensity; };
struct LightSample { vec3 pos; vec3 dir; vec3 rgb; float i; };
struct BastardTriangle { vec4 a; vec4 b; vec4 c; };

HD isect(vec3, vec3, vec3, vec3, vec3, HD, int, int);
vec3 volumeLight(int , vec3 , int,  vec3);

layout(std140, binding = 0) uniform GlobalPoly {
  BastardTriangle triangles[NUM_TRIANGLES];
};

uniform LightSample lightSamples[LIGHT_SAMPLES];

float rand(vec2 co) {
  return fract(sin(dot(co, vec2(12.9898, 78.233))) * 43758.5453);
}
vec3 rgb2hsv(vec3 c) {
    vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
    vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
    vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));

    float d = q.x - min(q.w, q.y);
    float e = 1.0e-10;
    return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
}
vec3 hsv2rgb(vec3 c) {
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

vec3 ws(vec4 w) { return w.xyz/w.w; }

vec3 modelToWorld(float x, float y, float z) {
  return ws(viewMat * vec4(x, y, z, 1));
}
// vec3 modelToWorld2(vec3 p) { return ws(viewMat * vec4(p, 1)); }
vec3 worldToModel(vec3 p) { return ws(inverse(viewMat) * vec4(p, 1)); }
vec3 screenToWorld(vec3 x){ return ws(inverse(projMat) * vec4(x, 1)); }

vec4 ndc(float z){
  return vec4(
    (gl_FragCoord.x / resolution.x - 0.5) * 2.0,
    (gl_FragCoord.y / resolution.y - 0.5) * 2.0,
    // (gl_FragCoord.z - 0.5) * 2.0,
    z, 1.0);
}

HD raytrace(vec3 ro, vec3 rd){
  int t_id = 0;
  int m_id = 0;

  HD hd = HD(-1, -1, MAX_DST);

  for (int i = 0; i < NUM_TRIANGLES; i += 2) {
    BastardTriangle curr = triangles[i];
    hd = isect(ro, rd, (vec3(curr.a.x, curr.a.y, curr.a.z)),
                        (vec3(curr.a.w, curr.b.x, curr.b.y)),
                        (vec3(curr.b.z, curr.b.w, curr.c.x)),
              hd, t_id, m_id);
    t_id += 1;

    curr = triangles[i+1];
    hd = isect(ro, rd, (vec3(curr.a.x, curr.a.y, curr.a.z)),
                        (vec3(curr.a.w, curr.b.x, curr.b.y)),
                        (vec3(curr.b.z, curr.b.w, curr.c.x)),
              hd, t_id, m_id);
    t_id += 1;
  }
  return hd;
}

void main() {
  vec4 coord = ndc(0);
  vec3 far = screenToWorld(ndc(1).xyz);
  vec3 ro = screenToWorld(coord.xyz);
  vec3 rd = normalize(far - coord.xyz);

  HD hd = raytrace(ro, rd);

  if (bool(full)) {
    vec3 res = vec3(0);
    float dst = 50;
    if (hd.m_id > -1) { dst = hd.t; }

      vec3 hit = ro + rd*dst;

      for (int q=0 ; q < LIGHT_SAMPLES ; q++) {
        res += volumeLight(q, ro,
            clamp(int(floor(dst * sampleRate)), 3, 60),
            hit);
      }
      frag.xyz = res;
      frag.w = 1;

  } else {
    frag.xyz = vec3(0);
    frag.w = 1;
    // if (hd.m_id > -1) {
      vec3 h = ro + hd.t * rd;
      frag.xyz = abs(worldToModel(h))/10;
    // }
  }

  #if 1
  #else
  #endif

  // vec3 hsv = rgb2hsv(frag.xyz);
  // hsv.y = clamp(hsv.y - 0.1*pow(hsv.z,2), 0, 1);
  // frag.xyz = hsv2rgb(hsv);
}

vec3 volumeLight(int light, vec3 origin, int numSamples, vec3 hit) {
  float r = time;

  vec3 res = vec3(0);
  for (int i=0 ; i < numSamples ; i++) {
    r = rand(gl_FragCoord.xy + vec2(i+r,r));
    vec3 s = mix(origin, hit, r);
    vec3 df = lightSamples[light].pos - s;
    float ldst = length(df);
    HD hd = raytrace(s, df / ldst);
    if (hd.m_id < 0 || hd.t > ldst) {
      res += exp(- pow(ldst / 0.2, 0.5));
    }

  }
  return lightSamples[light].rgb * res /
    float(numSamples) * distance(hit, origin);
}

HD isect(vec3 ro, vec3 rd,
         vec3 v0, vec3 v1, vec3 v2,
         HD hd, int t_id, int m_id) {

  vec3 s, q;
  float f, u, v, t;

  vec3 e1 = v1 - v0;
  vec3 e2 = v2 - v0;

  vec3 h = cross(rd, e2);
  float a = dot(e1, h);

  if ( (abs(a) > EPS) ) {
    f = 1.0 / a;
    s = ro - v0;
    u = f * dot(s, h);
    if ( !(u < 0.0 || u > 1.0) ) {
      q = cross(s, e1);
      v = f * dot(rd, q);
      if ( !(v < 0.0 || u + v > 1.0) ) {
        t = f * dot(e2, q);
        if (t > EPS && t < hd.t) {
          hd.t = t;
          hd.t_id = t_id;
          hd.m_id = m_id;
        }
      }
    }
  }
  return hd;
}

