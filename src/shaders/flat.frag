#version 430 core
// #extension GL_NV_uniform_buffer_std430_layout : enable
// layout(std140, binding = 1) uniform Norms { vec3 norms[NUM_TRIS]; };

#define EPS 0.0001
#define MAX_DST 9999.0
#define MAX_VOL_DST 200.0
#define PI 3.1415925359
#define TWO_PI 6.2831852
precision highp float;

struct HD { int t_id; float t; };
vec3 isect(vec3, vec3, vec3, vec3, vec3);
vec3 nrm(vec3, vec3);
HD raytrace(HD, vec3, vec3);
bool isectRayBox(vec3, vec3, vec3, vec3);

uniform float ts;
uniform mat4 pm, vm;
uniform vec2 resolution;
uniform vec3 vpn;
uniform samplerBuffer norms, mima, polyfx, lights, rnds, matpar;
uniform isamplerBuffer nodes, mats;

out vec4 FragColor;

vec3 nrm(vec3 d, vec3 n) {
  if (dot(d, n) > 0) { return n*-1.0; }
  return n;
}
vec3 ws(vec4 w) { return w.xyz/w.w; }
vec3 screenToWorld(vec3 x) { return ws( inverse(vm) * (inverse(pm) * vec4(x, 1))); }
vec4 ndc(float z){
  return vec4(
    (gl_FragCoord.xy/resolution.xy - 0.5) * 2.0, z, 1.0); //(gl_FragCoord.z - 0.5) * 2.0
}

// https://gamedev.stackexchange.com/questions/18436/most-efficient-aabb-vs-ray-collision-algorithms
// from: https://www.shadertoy.com/view/wtSyRd
bool isectRayBox(vec3 minpos, vec3 maxpos, vec3 o, vec3 idr) {
  // vec3 idr = 1.0 / ray_dir;
  const vec3 tbot = idr * (minpos - o);
  const vec3 ttop = idr * (maxpos - o);
  const vec3 tmin = min(ttop, tbot);
  const vec3 tmax = max(ttop, tbot);
  vec2 traverse = max(tmin.xx, tmin.yz);
  const float traverselow = max(traverse.x, traverse.y);
  traverse = min(tmax.xx, tmax.yz);
  const float traversehi = min(traverse.x, traverse.y);
  // return vec3(float(traversehi > max(traverselow, 0.0)), traversehi, traverselow);
  return traversehi > max(traverselow, 0.0);
}

// from: https://www.shadertoy.com/view/MlGcDz
vec3 isectPoly(vec3 ro, vec3 rd, vec3 v2v0, vec3 v1v0, vec3 v0) {
  vec3 rov0 = ro - v0;
  vec3  n = cross( v1v0, v2v0 );
  vec3  q = cross( rov0, rd );
  float d = 1.0/dot( rd, n );
  float u = d*dot( -q, v2v0 );
  float v = d*dot(  q, v1v0 );
  float t = d*dot( -n, rov0 );
  if( u<0.0 || v<0.0 || (u+v)>1.0 ) t = -1.0;
  return vec3( t, u, v );
}

HD checkPolygons(HD hd, int n, int i, vec3 o, vec3 rd) {
  for (int p = i; p < i+(3*n); p+=3) {
    const int pp = p;
    const vec3 hit = isectPoly(o, rd,
        texelFetch(polyfx, pp).rgb,    // e2
        texelFetch(polyfx, pp+1).rgb,  // e1
        texelFetch(polyfx, pp+2).rgb); // v0
    #if 0
    const int tst = int(hit.x < hd.t && hit.x>0.0);
    const int tst1 = (1-tst);
    // hd.t = tst*hit.x + tst1*hd.t;
    hd.t_id = tst*p + tst1*hd.t_id;
    #else
    if ( hit.x < hd.t && hit.x>0.0 ) {
      hd.t = hit.x;
      hd.t_id = p/3;
    }
    #endif
  }
  return hd;
}

HD raytrace(HD hd, vec3 o, vec3 rd) {
  const vec3 ird = 1.0/rd;
  int i = 0;
  while (i>=0) {
    ivec4 node = texelFetch(nodes, i);
    const int c2 = 2 * i;
    const bool hitbox = isectRayBox(texelFetch(mima, c2).rgb, // min xyz / max xyz
                                    texelFetch(mima, c2+1).rgb, o, ird);

    const int isboxhit = int(hitbox);
    hd = checkPolygons(hd, node.r, node.g/3, o, rd);
    const int isleaf = int(node.r>0);
    i = (    isboxhit  * (node.g*(1 - isleaf) + isleaf*node.w) +
          (1-isboxhit) *  node.w ) / 4;
  }
  return hd;
}

void main() { ////////////////////////////////////////////////////////////////
  const vec4 ndc_coord = ndc(0);
  const vec3 world_far = screenToWorld(ndc(1).xyz);
  const vec3 cam = screenToWorld(ndc_coord.xyz);
  const vec3 rd = world_far - cam;

  const HD hd = raytrace(HD(-1, MAX_DST), cam, rd);
  if (hd.t_id > -1) { // HIT
    const int mi = texelFetch(mats, hd.t_id).x;
    vec3 c = texelFetch(matpar, texelFetch(mats, hd.t_id).y).xyz;

    // texelFetch(mats, hd.t_id)
    // const vec3 n = nrm(rd, texelFetch(norms, hd.t_id).xyz);
    // const float v = abs(dot(vpn, n));
    const float v = 1.0;
    FragColor = vec4(v*c,0);
    // FragColor = vec4(c, 0);

    #if 1
    const vec3 h = cam + rd*max(min(hd.t, MAX_VOL_DST), 0);
    // FragColor.xyz += 1*int(m>0);
    // FragColor.xyz = vec3(1.0);
    #endif
  } else { // MISS
    FragColor = vec4(0, 0, 0, 1); // FragColor = vec4(abs(coord.xy), 0, 0);
  }

} ///////////////////////////////////////////////////////////////////////////

