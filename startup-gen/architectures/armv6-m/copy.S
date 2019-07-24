${INDENT}/* Copy .${NAME} */
${INDENT}ldr	r0,=__${NAME}_start
${INDENT}ldr	r1,=__${NAME}_words
${INDENT}ldr	r2,=__${NAME}_load
${INDENT}cmp	r1,#0
${INDENT}beq	1f
0:${INDENT}ldr	r4,[r2]
${INDENT}adds	r2, #4
${INDENT}str	r4,[r0]
${INDENT}adds	r0, #4
${INDENT}subs	r1,r1,#1
${INDENT}bne	0b
1:
