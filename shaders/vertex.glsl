#version 330 core

in vec2 pos;
out vec2 uv;

void main(void) {
  gl_Position = vec4(pos.xy, 0.0, 1.0);
  uv = pos;
}
