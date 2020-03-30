#version 330

uniform float iTime;
uniform float iDeltaTime;
uniform vec2 iMousePos;
uniform vec2 iResolution;

uniform sampler2D texture0;

in vec2 uv;
out vec4 fragColor;

void main(void) {
     vec2 uv = uv*0.5+0.5;
     fragColor.rgba = texture(texture0, vec2(uv.x, -uv.y));
}