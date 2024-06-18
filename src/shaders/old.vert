#version 420 core

precision highp float;

in vec2 c2d;
out vec3 glPos;
// uniform mat4 viewMat;
// uniform mat4 projMat;
//
// res 1000

void main() {
  float mx = max(resolution.x, resolution.y);
  vec2 coord = gl_FragCoord.xy / mx + (1-resolution/mx)*0.5 ;

  glPos = vec3(c2d,0);
  gl_Position = vec4(c2d, 0,1.0);
}

