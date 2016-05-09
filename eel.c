#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <SDL.h>

void puti(int x) { printf("%d\n", x); }
void putd(double x) { printf("%f\n", x); }
void putl(unsigned long int x) { printf("%lu\n", x); }
void putb(int x) { if(x == 0) {printf("false\n"); } else {printf("true\n"); } }
void putu(unsigned int x) { printf("%u\n", x); }
void putf(float x) { printf("%f\n", x); }
void putp(void *x) { printf("%p\n", x); }
void putcstr(char *s) { printf("%s\n", s); }
void putcchar(char c) { printf("%c\n", c); }

static SDL_Window *win = NULL;
static SDL_Renderer *renderer = NULL;
static SDL_Surface *screen = NULL;

void cleanup_sdl(int i)
{
  /* if(screen) SDL_FreeSurface(screen); */
  /* if(renderer) SDL_DestroyRenderer(renderer); */
  /* if(win) SDL_DestroyWindow(win); */
  /* SDL_Quit(); */
  exit(i);
}

void exitor(bool x)
{
  if(x) return;

  fprintf(stderr, "SDL failure:%s\n", SDL_GetError());
  cleanup_sdl(-1);
}

int poll_sdl()
{
  SDL_Event e;
  if (!SDL_PollEvent(&e)) return 0;
  if (e.type == SDL_QUIT) return SDL_QUIT;
  if (e.type == SDL_KEYDOWN) return e.key.keysym.sym;
  return 0;
}

#if SDL_BYTEORDER == SDL_BIG_ENDIAN
#define rmask 0xff000000
#define gmask 0x00ff0000
#define bmask 0x0000ff00
#define amask 0x000000ff
#else
#define rmask 0x000000ff
#define gmask 0x0000ff00
#define bmask 0x00ff0000
#define amask 0xff000000
#endif

SDL_Texture *load_rgba(char *fn, int w, int h)
{
  SDL_Surface *srfc;
  SDL_Texture *tex;
  unsigned int *pixels;
  int sz = w*h;
  SDL_RWops *file;
  unsigned int px;

  if(sz == 0) return NULL;
  exitor(pixels = malloc(sz*4));
  exitor(file = SDL_RWFromFile(fn, "rb"));
  exitor(sz == SDL_RWread(file, pixels, 4, sz));
  exitor(!SDL_RWclose(file));

  exitor(srfc = SDL_CreateRGBSurfaceFrom(pixels, w, h, 32, w*4, rmask,gmask,bmask,amask));
  exitor(tex = SDL_CreateTextureFromSurface(renderer, srfc));
  SDL_FreeSurface(srfc);
  free(pixels);
	 
  return tex;
}

void clear_sdl()
{
SDL_RenderClear(renderer);
}

void present_sdl()
{
SDL_RenderPresent(renderer);
}

void blit(SDL_Texture *tex, int w, int h, int x, int y, double angle)
{
  SDL_Rect rect;
  rect.x = x;
  rect.y = y;
  rect.w = w;
  rect.h = h;
  
  SDL_RenderCopyEx(renderer, tex, NULL, &rect, angle, NULL, SDL_FLIP_NONE);
}

void init_sdl(int winX, int winY, int winW, int winH)
{
  exitor(!SDL_Init(SDL_INIT_VIDEO));
  exitor(win = SDL_CreateWindow("Hello World", winX, winY, winW, winH, 0));
  exitor(screen = SDL_GetWindowSurface(win));
  exitor(renderer = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED));
  exitor(!SDL_SetRenderDrawColor(renderer, 0xFF, 0xFF, 0xFF, 0xFF));
}

void set_color(int r, int g, int b, int a)
{
  SDL_SetRenderDrawColor( renderer, r, g, b, a );
}

int eel_main(char argc, char *argv[]);

#ifdef __cplusplus
extern "C"
#endif
int main(int argc, char *argv[])
{
  return eel_main(argc, argv);
}
