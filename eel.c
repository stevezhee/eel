#include <stdio.h>
void puti(int x);
void putu(unsigned int x);
void putf(float x);
void putb(int x);

void puti(int x) { printf("%d\n", x); }
void putb(int x) { if(x == 0) {printf("false\n"); } else {printf("true\n"); } }
void putu(unsigned int x) { printf("%u\n", x); }
void putf(float x) { printf("%f\n", x); }
