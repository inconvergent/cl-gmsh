#version 420 core

precision highp float;

in vec2 c2d;

void main() {
  gl_Position = vec4(c2d, -1.0, 1.0);
}

