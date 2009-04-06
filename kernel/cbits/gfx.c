#include "vbe.h"
#include "gfx.h"

#define MIN(x,y) (((x)<=(y))?(x):(y))
#define MAX(x,y) (((x)>(y))?(x):(y))

#define FAR(p) ((((p)>>16)<<4)|((p)&0xffff))

static struct vbe_modeinfoblock *vbe_mib;
static struct vbe_controlinfoblock *vbe_cib;

static unsigned long fg; /* current foreground pixel value */
typedef struct { int xmin,ymin,xmax,ymax; } Clip;
static Clip clip,screenclip;
static struct { int x,y,visible; } cursor;

static struct {
  int width,height;
  unsigned bpp; /* bytes per pixel */
} screen = {0,0,0};

void init_gfx(struct vbe_modeinfoblock *mib,vbe_controlinfoblock_t *cib)
{
  vbe_mib=mib;
  vbe_cib=cib;
  screen.bpp=(vbe_mib->bits_per_pixel+7)/8; /* bytes per pixel */
  clip.xmax=screen.width=vbe_mib->x_resolution;
  clip.ymax=screen.height=vbe_mib->y_resolution;
  screenclip=clip;
  set_cursor(screen.width/2,screen.height/2);
  /*show_cursor();*/
}

void *gfx_framebuffer(void) { return (void *)vbe_mib->phys_base_ptr; }

unsigned gfx_width (void) { return screen.width; }
unsigned gfx_height(void) { return screen.height; }
unsigned gfx_bpp(void)    { return screen.bpp; }

unsigned gfx_mask_size(PixelField p) {
  return vbe_mib->pixel_layout[p].mask_size;
}

unsigned gfx_field_position(PixelField p)
{
  return vbe_mib->pixel_layout[p].field_position;
}

unsigned vbe_version(void)     { return vbe_cib->vbeversion; }
char *vbe_oemstring(void)      { return (char *)FAR(vbe_cib->oemstringptr); }
char *vbe_oemvendorname(void)  { return (char *)FAR(vbe_cib->oemvendornameptr); }
char *vbe_oemproductname(void) { return (char *)FAR(vbe_cib->oemproductnameptr); }


void set_clip(int l,int t,int r,int b)
{
  clip.xmin=MAX(l,0);
  clip.ymin=MAX(t,0);
  clip.xmax=MIN(r,screen.width);
  clip.ymax=MIN(b,screen.height);
}

static unsigned pixelvalue(Color red,Color green,Color blue)
{
  if(vbe_mib) {
    unsigned fg;
    pixel_layout_t *p=vbe_mib->pixel_layout;
    fg=(red>>(16-p[Red].mask_size))<<p[Red].field_position;
    fg|=(green>>(16-p[Green].mask_size))<<p[Green].field_position;
    fg|=(blue>>(16-p[Blue].mask_size))<<p[Blue].field_position;
    return fg;
  }
  else return 0;
}

void set_color(Color red,Color green,Color blue)
{
  fg=pixelvalue(red,green,blue);
}

static void setpixelclip(int x,int y,Clip *c)
{
  int p;

  if(x<c->xmin || y<c->ymin || x>=c->xmax || y>=c->ymax) return;
  p=y*screen.width+x;
  switch(screen.bpp) {
  case 1:
    ((db *)(vbe_mib->phys_base_ptr))[p]=fg;
    break;
  case 2:
    ((dw *)(vbe_mib->phys_base_ptr))[p]=fg;
    break;
  case 3:
    {
      db *pp=(db *)(vbe_mib->phys_base_ptr+p*3);
      pp[0]=fg;
      pp[1]=fg>>8;
      pp[2]=fg>>16;
    }
    break;
  case 4:
    ((dd *)(vbe_mib->phys_base_ptr))[p]=fg;
    break;
  }
}

static void setpixel(int x,int y)
{
  setpixelclip(x,y,&clip);
}

void set_pixel(int x,int y)
{
  hide_cursor();
  setpixel(x,y);
  show_cursor();
}


unsigned get_pixel(int x,int y,unsigned undef)
{
  unsigned p;

  if(x<0 || y<0 || x>=screen.width || y>=screen.height) return undef;
  p=y*screen.width+x;
  switch(screen.bpp) {
  case 1:
    return ((db *)(vbe_mib->phys_base_ptr))[p];
  case 2:
    return ((dw *)(vbe_mib->phys_base_ptr))[p];
  case 3:
    {
      db *pp=(db *)(vbe_mib->phys_base_ptr+p*3);
      return pp[0]+(pp[1]<<8)+(pp[2]<<16);
    }
  case 4:
    return ((dd *)(vbe_mib->phys_base_ptr))[p];
  }
  return undef;
}

void fill_rectangle(int x0,int y0,int w,int h)
{
  int x,y;
  hide_cursor();
  /* TODO: compute intersection with clip rectangle */
  for(y=y0;y<y0+h;y++)
    for(x=x0;x<x0+w;x++)
      setpixel(x,y);
  show_cursor();
}

void draw_line(int x1,int y1,int x2,int y2)
{
  hide_cursor();
  if(x1==x2) {
    int y0,h;
    if(y1<=y2) y0=y1,h=y2-y1+1; else y0=y2,h=y1-y2+1;
    fill_rectangle(x1,y0,1,h);
  }
  else if(y1==y2) {
    int x0,w;
    if(x1<=x2) x0=x1,w=x2-x1+1; else x0=x2,w=x1-x2+1;
    fill_rectangle(x0,y1,w,1);
  }
  else {
    int dx,dy,sx,sy;
    dx=x2-x1,sx=1; if(dx<0) dx=-dx,sx=-1;
    dy=y2-y1,sy=1; if(dy<0) dy=-dy,sy=-1;
    if(dx>dy) {
      int x,y,d,n;
      for(d=0,n=dx,x=x1,y=y1;n>=0;x+=sx,n--) {
	setpixel(x,y);
	d+=dy;
	if(d>=dx) y+=sy,d-=dx;
      }
    }
    else {
      int x,y,d,n;
      for(d=0,n=dy,x=x1,y=y1;n>=0;y+=sy,n--) {
	setpixel(x,y);
	d+=dx;
	if(d>=dy) x+=sx,d-=dy;
      }
    }
  }
  show_cursor();
}

#define CURSIZE 11

static void swap_cursor(void)
{
  static unsigned curfg,h[CURSIZE],v[CURSIZE];
  unsigned i,savedfg;

  if(curfg==0) {
    /* first time */
    curfg=pixelvalue(0xffff,0,0); /* red cursor */
    for(i=0;i<CURSIZE;i++) h[i]=v[i]=curfg;
  }

  savedfg=fg;
  for(i=0;i<CURSIZE;i++) {
    int d=i-CURSIZE/2;
    fg=h[i];
    h[i]=get_pixel(cursor.x+d,cursor.y,curfg);
    setpixelclip(cursor.x+d,cursor.y,&screenclip);
    if(d!=0) {
      fg=v[i];
      v[i]=get_pixel(cursor.x,cursor.y+d,curfg);
      setpixelclip(cursor.x,cursor.y+d,&screenclip);
    }
  }
  fg=savedfg;
}

void show_cursor()
{
  if(++cursor.visible==1) swap_cursor();
}

void hide_cursor()
{
  if(--cursor.visible==0) swap_cursor();
}

void set_cursor(int x,int y)
{
  hide_cursor();
  cursor.x=x,cursor.y=y;
  show_cursor();
}
