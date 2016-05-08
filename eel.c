#include <stdio.h>
#include <stdbool.h>
#include <SDL.h>

void puti(int x) { printf("%d\n", x); }
void putb(int x) { if(x == 0) {printf("false\n"); } else {printf("true\n"); } }
void putu(unsigned int x) { printf("%u\n", x); }
void putf(float x) { printf("%f\n", x); }

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

SDL_Texture *load_bmp(char *fn, int *w, int *h)
{
  SDL_Surface *srfc;
  SDL_Texture *tex;
  exitor(srfc = SDL_LoadBMP(fn));
  exitor(tex = SDL_CreateTextureFromSurface(renderer, srfc));
  *w = srfc->w;
  *h = srfc->h;
  SDL_FreeSurface(srfc);
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

void blit(SDL_Texture *tex, int x, int y, int w, int h)
{
  SDL_Rect rect;
  rect.x = x;
  rect.y = y;
  rect.w = w;
  rect.h = h;
  
  SDL_RenderCopy(renderer, tex, NULL, &rect);
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
