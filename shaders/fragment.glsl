#version 330

uniform float iTime;

in vec2 uv;
out vec4 fragColor;

void main(void) {
        fragColor = vec4(uv.x, uv.y, abs(sin(iTime)), 1.0);
}
