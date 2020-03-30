#version 330

uniform float iTime;
uniform float iDeltaTime;
uniform vec2 iMousePos;
uniform vec2 iResolution;
uniform float isBuffer;

uniform sampler2D texture0;
uniform sampler2D buffer0;

in vec2 uv;
out vec4 fragColor;

void main(void) {
        if (isBuffer == 0.0) {
                fragColor = texture(texture0, gl_FragCoord.xy / iResolution);
        } else {
                fragColor = texture(buffer0, gl_FragCoord.xy / iResolution);
        }
}
