${INDENT}/* Copy ${NAME} */
${INDENT}movw  r0,#:lower16:__${NAME}_start
${INDENT}movt  r0,#:upper16:__${NAME}_start
${INDENT}movw  r1,#:lower16:__${NAME}_words
${INDENT}movw  r2,#:lower16:__${NAME}_load
${INDENT}movt  r2,#:upper16:__${NAME}_load
${INDENT}cbz  r1,1f
0:${INDENT}ldr  r4,[r2],#4
${INDENT}str  r4,[r0],#4
${INDENT}subs  r1,r1,#1
${INDENT}bne  0b

1:
