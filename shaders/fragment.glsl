#version 330

uniform float iTime;
uniform vec2 iMousePos;

in vec2 uv;
out vec4 fragColor;

void main(void) {
        fragColor = vec4(float(iMousePos.x / 500.0), uv.y, abs(sin(iTime)), 1.0);
}
