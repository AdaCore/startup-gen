${INDENT}/* Clear .${NAME} */
${INDENT}ldr	r0,=__${NAME}_start
${INDENT}ldr	r1,=__${NAME}_words
${INDENT}ldr	r2,=0
${INDENT}cmp	r1,#0
${INDENT}beq	1f
0:${INDENT}str	r2,[r0]
${INDENT}adds	r0,#4
${INDENT}subs	r1,r1,#1
${INDENT}bne	0b
1:

