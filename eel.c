#include <stdio.h>
#include <stdbool.h>
#include <SDL.h>
void puti(int x);
void putu(unsigned int x);
void putf(float x);
void putb(int x);

void puti(int x) { printf("%d\n", x); }
void putb(int x) { if(x == 0) {printf("false\n"); } else {printf("true\n"); } }
void putu(unsigned int x) { printf("%u\n", x); }
void putf(float x) { printf("%f\n", x); }

static SDL_Window *win = NULL;
static SDL_Renderer *renderer = NULL;
static SDL_Surface *screen = NULL;

void cleanup_sdl(int i);
void exitor(bool x);
void init_sdl(int winX, int winY, int winW, int winH);

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

void init_sdl(int winX, int winY, int winW, int winH)
{
  exitor(!SDL_Init(SDL_INIT_VIDEO));
  exitor(win = SDL_CreateWindow("Hello World", winX, winY, winW, winH, 0));
  exitor(screen = SDL_GetWindowSurface(win));
  exitor(renderer = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED));
  exitor(!SDL_SetRenderDrawColor(renderer, 0xFF, 0xFF, 0xFF, 0xFF));

/*     bitmapSurface = SDL_LoadBMP("ship.bmp"); */
/*     if(!bitmapSurface) */
/*       { */
/*  	printf("%s\n", SDL_GetError()); */
/*  	goto cleanup; */
/*       } */
/*     /\* bitmapSurface = SDL_ConvertSurface(tempSurface, screenSurface->format, 0); *\/ */
/*     /\*  *\/ */
/*     /\* if(!bitmapSurface) *\/ */
/*     /\*   { *\/ */
/*     /\* 	printf("%s\n", SDL_GetError()); *\/ */
/*     /\* 	goto cleanup; *\/ */
/*     /\*   } *\/ */
/*     bitmapTex = SDL_CreateTextureFromSurface(renderer, bitmapSurface); */
/*     SDL_Rect rect; */
/*     rect.x = 10; */
/*     rect.y = 20; */
/*     rect.w = bitmapSurface->w; */
/*     rect.h = bitmapSurface->h; */
/*     if(!bitmapTex) */
/*       { */
/*     	printf("%s\n", SDL_GetError()); */
/*     	goto cleanup; */
/*       } */
/*        SDL_FreeSurface(bitmapSurface); */

/*        int dx; */
/*        int dy; */

/*                         { */
/*                             case SDLK_UP: */
/*  			      dx = 0; */
/*  			      dy = -1; */
/*                             break; */

/*                             case SDLK_DOWN: */
/*  			      dx = 0; */
/*  			      dy = 1; */
/*                             break; */

/*                             case SDLK_LEFT: */
/*  			      dx = -1; */
/*  			      dy = 0; */
/*                             break; */

/*                             case SDLK_RIGHT: */
/*  			      dx = 1; */
/*  			      dy = 0; */
/*                             break; */

/*                             default: */
/*  			      dx = 0; */
/*  			      dy = 0; */
/*                             break; */
/*                         }		} */

/*  	/\* SDL_BlitSurface(bitmapSurface, NULL, screen, NULL); *\/ */
/*  	/\* SDL_UpdateWindowSurface(win); *\/ */
/*  	rect.x += dx; */
/*  	rect.y += dy; */

/*  	rect.x = rect.x > (width - rect.w) ? (width - rect.w) : rect.x; */
/*  	rect.x = rect.x < 0 ? 0 : rect.x; */
/*  	rect.y = rect.y > (height - rect.h) ? (height - rect.h) : rect.y; */
/*  	rect.y = rect.y < 0 ? 0 : rect.y; */

/*         SDL_RenderClear(renderer); */
/*         SDL_RenderCopy(renderer, bitmapTex, NULL, &rect); */
/*         SDL_RenderPresent(renderer); */
/*     } */
}
