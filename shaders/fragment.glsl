#version 330

uniform float iTime;
uniform vec2 iMousePos;
uniform vec2 iResolution;

in vec2 uv;
out vec4 fragColor;

void main(void) {
        fragColor.xy = gl_FragCoord.xy / iResolution;
}
