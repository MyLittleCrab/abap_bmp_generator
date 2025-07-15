# ABAP BMP Generator

This project provides a set of ABAP classes and example programs for generating and manipulating BMP (bitmap) images directly within SAP. It includes utilities for drawing basic shapes, lines, polygons, and text, as well as a demonstration for plotting a sine graph and displaying or downloading the resulting BMP image in SAP GUI.

## Features

- **BMP Image Generation**: Create BMP images of arbitrary size.
- **Drawing Primitives**: Draw pixels, lines, rectangles, triangles, polygons, and filled shapes.
- **Text Rendering**: Render text and symbols onto the BMP using a bitmap font.
- **SAP GUI Integration**: Display images in a custom container or download them directly from SAP GUI.

## Structure

- `bmp_generator/zcl_image_file.clas.abap`: Abstract base class for image file operations (e.g., loading, exporting, displaying, downloading images).
- `bmp_generator/zcl_bmp.clas.abap`: Core class for BMP image creation and drawing operations.
- `bmp_generator/zcl_bmp_color.clas.abap`, `zcl_bmp_coord.clas.abap`: Helper classes for color and coordinate management.
- `bmp_generator/z_bmp_smw0_downloader.func.abap`: Function module to download files (such as BMP fonts or images) from SAP's SMW0 repository to the application server or presentation server.
- `examples/zcl_bmp_sine_graph.clas.abap`: Example class for drawing a sine graph.
- `examples/zshow_sine_bmp.abap`: Example program to display a sine graph BMP in a SAP GUI container.
- `examples/zdownload_sine_bmp.abap`: Example program to generate and download a sine graph BMP.

## Usage

### 1. Display Sine Graph in SAP GUI

This program generates a sine graph and displays it in a custom SAP GUI container.

```abap
REPORT zshow_sine_bmp.

DATA: lv_width  TYPE i VALUE 600,
      lv_height TYPE i VALUE 600.

DATA: lo_container TYPE REF TO cl_gui_custom_container.

START-OF-SELECTION.

  DATA(lo_graph) = NEW zcl_bmp_sine_graph( iv_width = lv_width iv_height = lv_height ).
  lo_graph->draw_sine( ).

  " Create container and HTML viewer
  CALL SCREEN 100.

* Screen 100 should have a custom control named 'PICTURE_AREA'
MODULE status_0100 OUTPUT.
  IF lo_container IS INITIAL.
    lo_container = NEW cl_gui_custom_container( 'PICTURE_AREA' ).
    lo_graph->show_in_sapgui_container( lo_container ).
  ENDIF.
ENDMODULE.
```

**Screen 100** must be created in SE51 with a custom control named `PICTURE_AREA`.

### 2. Download Sine Graph as BMP

This program generates a sine graph and prompts the user to download the BMP file.

```abap
REPORT zdownload_sine_bmp.

DATA:
  lv_width    TYPE i VALUE 600,
  lv_height   TYPE i VALUE 600.

DATA(lo_graph) = NEW zcl_bmp_sine_graph( iv_width = lv_width iv_height = lv_height ).
lo_graph->draw_sine( ).
lo_graph->download_in_sapgui( ).
```

### 3. Drawing Custom Images

You can use the `zcl_bmp` class directly to create custom images:

```abap
DATA(lo_bmp) = NEW zcl_bmp( iv_width = 400 iv_height = 300 ).
" Draw a red line
lo_bmp->draw_line(
  io_color = NEW zcl_bmp_color( iv_r = 255 iv_g = 0 iv_b = 0 )
  io_coord_start = NEW zcl_bmp_coord( iv_x = 10 iv_y = 10 )
  io_coord_end   = NEW zcl_bmp_coord( iv_x = 390 iv_y = 290 )
).
" Export as xstring or display/download as needed
DATA(lv_xstring) = lo_bmp->get_xstring( ).
```

## Font Generation and Custom Fonts

This project supports custom bitmap fonts for text rendering in BMP images.  
An example font (`font_cp1251.bmp`) and a Python script (`converter.py`) for generating BMP fonts are provided in the `bmp_font_generator/` directory.

- **font_cp1251.bmp**: Example bitmap font (CP1251 encoding).
- **converter.py**: Script to convert TTF fonts to BMP format suitable for use in ABAP.

To use your own font:
1. Use `converter.py` to generate a BMP font from a TTF file.
2. Upload the resulting BMP to SAP (e.g., via SMW0) and reference it in your ABAP code.

## Class Diagram

Below is a class diagram showing the main classes and their relationships:

```mermaid
classDiagram
  class zcl_image_file {
    <<abstract>>
    +get_xstring() xstring
    +import_from_xstring(iv_xstring)
    +import_from_smw0(iv_object)
    +get_base64() string
    +get_base64_web_link() string
    +show_in_sapgui_container(io_container)
    +download_in_sapgui(iv_filename)
    #load_xstring_from_smw0(iv_file_name) xstring
  }
  class zcl_bmp {
    +draw_pixel(io_color, io_coord)
    +draw_line(io_color, io_coord_start, io_coord_end)
    +draw_rectangle(io_color, io_coord1, io_coord2, io_coord3, io_coord4, iv_fill)
    +draw_triangle(io_color, io_coord1, io_coord2, io_coord3, iv_fill)
    +draw_polygon(io_color, it_coords, iv_fill)
    +draw_text(iv_text, io_coord)
    +draw_symbol(iv_symbol, io_coord)
    +get_font_sizes() ts_sizes
    +get_image_sizes() ts_sizes
    +get_xstring() xstring
    +import_from_xstring(iv_xstring)
    #load_bmp_font()
    #parse_bmp_header(iv_file) ts_bmp_header
  }
  class zcl_bmp_sine_graph {
    +draw_sine(iv_amplitude, iv_frequency, iv_phase)
    #draw_axes()
  }
  class zcl_bmp_color {
    +constructor(iv_r, iv_g, iv_b)
    +get_rgb() tt_pixel
  }
  class zcl_bmp_coord {
    +constructor(iv_x, iv_y)
    +get_x() i
    +get_y() i
  }
  zcl_bmp --|> zcl_image_file
  zcl_bmp_sine_graph --|> zcl_bmp
```

## Requirements

- SAP NetWeaver system with ABAP stack.
- Authorization to create and run custom ABAP classes and reports.
- (Optional) SAP GUI for Windows for full GUI integration.

## Installation

1. Import the classes and reports into your SAP system using your preferred ABAP development tools (SE80, Eclipse, etc.).
2. (Optional) Create the required screen (e.g., 100) with a custom control for GUI display.

## Customization

- Extend `zcl_bmp_sine_graph` or create your own subclasses of `zcl_bmp` for custom drawing logic.
- Use helper classes for color (`zcl_bmp_color`) and coordinates (`zcl_bmp_coord`).

## License

MIT License