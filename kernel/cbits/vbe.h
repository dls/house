
#ifndef VBE_H
#define VBE_H

/* From VBE 3.0, http://www.vesa.org/Public/VBE/vbe3.pdf */

typedef unsigned short dw;
typedef unsigned char db;
typedef unsigned int dd;

typedef struct { db mask_size,field_position; } pixel_layout_t;
typedef enum { Red,Green,Blue,Rsvd } PixelField;

typedef struct vbe_modeinfoblock
{
  /* Mandatory information for all VBE revisions */
  dw mode_attributes;
  db win_a_attributes,win_b_attributes;
  dw win_granulatiry, win_size, win_a_segment, win_b_segment;
  dd win_func_ptr;
  dw bytes_per_scan_line;

  /* Mandatory information for VBE 1.2 and above */
  dw x_resolution, y_resolution;
  db x_char_size, y_char_size, number_of_planes, bits_per_pixel;
  db number_of_banks, memory_model, bank_size, number_of_image_pages;
  db reserved1;

  pixel_layout_t pixel_layout[4];
  db direct_color_mode_info;

  /* Mandatory information for VBE 2.0 and above */
  dd phys_base_ptr;
  dd reserved2;
  dw reserved3;

  /* Mandatory information for VBE 3.0 and above */
  dw lin_bytes_per_scan_line;
  db bnk_number_of_image_pages;
  db lin_number_of_image_pages;
  pixel_layout_t lin_pixel_layout[4];
  dd max_pixel_clock;
  db reserved4[189];

} __attribute((packed)) vbe_modeinfoblock_t;


typedef struct vbe_controlinfoblock {
   db vbesignature[4];   // VBE Signature
   dw vbeversion;        // VBE Version
   dd oemstringptr;      // Pointer to OEM String
   db capabilities[4];   // Capabilities of graphics cont.
   dd videomodeptr;      // Pointer to Video Mode List
   dw totalmemory;       // number of 64Kb memory blocks
   dw oemsoftwarerev;    // VBE implementation Software revision
   dd oemvendornameptr;  // Pointer to Vendor Name String
   dd oemproductnameptr; // Pointer to Product Name String
   dd oemproductrevptr;  // Pointer to Product Revision String
   db reserved[222];     // Reserved for VBE implementation scratch area
   db oemdata[256];      // Data Area for OEM Strings
} __attribute((packed)) vbe_controlinfoblock_t;

#endif
