#include <stdio.h>
void puti(int x);
void putu(unsigned int x);
void putf(float x);

void puti(int x) { printf("%d\n", x); }
void putu(unsigned int x) { printf("%u\n", x); }
void putf(float x) { printf("%f\n", x); }
