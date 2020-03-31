#version 330

uniform float iTime;
uniform float iDeltaTime;
uniform vec2 iMousePos;
uniform vec2 iResolution;
uniform int bufferId;

uniform sampler2D texture0;
uniform sampler2D buffer0;
uniform sampler2D buffer1;

in vec2 uv;
out vec4 fragColor;

void main(void) {
        if (bufferId == 0) {
                fragColor = texture(texture0, gl_FragCoord.xy / iResolution);
        } else if (bufferId == 1) {
                fragColor = texture(buffer1, gl_FragCoord.xy / iResolution);
        } else {
                fragColor = mix(texture(buffer1, gl_FragCoord.xy / iResolution) / 2.0,
                                texture(buffer0, gl_FragCoord.xy / iResolution) / 2.0,
                                sin(iTime)*0.5+0.5);
        }
}
