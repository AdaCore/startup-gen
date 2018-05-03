${INDENT}/* Clear ${NAME} */
${INDENT}movw  r0,#:lower16:__${NAME}_start
${INDENT}movt  r0,#:upper16:__${NAME}_start
${INDENT}movw  r1,#:lower16:__${NAME}_words
${INDENT}movw  r2,#0
${INDENT}cbz  r1,1f
0:${INDENT}str  r2,[r0],#4
${INDENT}subs  r1,r1,#1
${INDENT}bne  0b
1:
