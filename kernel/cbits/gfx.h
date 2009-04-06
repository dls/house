#include "vbe.h"

extern void init_gfx(vbe_modeinfoblock_t *mib,vbe_controlinfoblock_t *cib);

typedef unsigned short Color;

extern void
  set_color(Color red,Color green,Color blue),
  set_pixel(int x,int y),
  set_cursor(int x,int y),
  draw_line(int x1,int y1,int x2,int y2),
  fill_rectangle(int x,int y,int w,int h),
  set_clip(int l,int t,int r,int b),
  show_cursor(),
  hide_cursor();

extern unsigned gfx_width(void),gfx_height(void),gfx_bpp(void);
extern unsigned gfx_mask_size(PixelField p),gfx_field_position(PixelField p);
extern void *gfx_framebuffer(void);

/* Reading the VBE control info block: */

extern unsigned vbe_version(void);

extern char
  *vbe_oemstring(void),
  *vbe_oemvendorname(void),
  *vbe_oemproductname(void);

