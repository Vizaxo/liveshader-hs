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
                fragColor = texture(buffer1, gl_FragCoord.xy / iResolution);
        } else if (bufferId == 1) {
                vec4 c = texture(buffer1, gl_FragCoord.xy / iResolution);
                fragColor = vec4(100.0,0,0,0);
        } else {
                fragColor = mix(texture(buffer0, gl_FragCoord.xy / iResolution),
                                texture(buffer1, gl_FragCoord.xy / iResolution) / 10.0,
                                0);
        }
}
