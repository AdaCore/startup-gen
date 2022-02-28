#include <stdint.h>

#define AIRCR (*(volatile uint32_t*)0xe000ed0cUL)
#define SYSRESETREQ    (1<<2)
#define VECTKEY        (0x05fa0000UL)
#define VECTKEY_MASK   (0x0000ffffUL)

void print(const char* str)
{
  int len = 0;
  asm volatile (
    " mov r0, %[reason]  \n"
    " mov r1, %[arg]  \n"
    " bkpt %[swi] \n"
    :
    : [reason] "r" (0x04), [arg] "r" (str), [swi] "i" (0xAB)
    : "r0", "r1", "r2", "r3", "ip", "lr", "memory", "cc"
  );
}

void _exit(int val) {
    while (1) {
        AIRCR = (AIRCR & VECTKEY_MASK) | VECTKEY | SYSRESETREQ;
    }
}

void before_main(void) __attribute__((constructor));
void after_main(void) __attribute__((destructor));

void before_main(void)
{
    print("before_main()\n");
}

void after_main(void)
{
    print("after_main()\n");
}

int main(int argc, char **argv) {
    print("main()\n");
    return -1;
}
