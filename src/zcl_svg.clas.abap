CLASS zcl_svg DEFINITION PUBLIC CREATE PUBLIC
  INHERITING FROM zcl_image_file.
  PUBLIC SECTION.

    "! SVG image manipulation class
    "! @description Provides functionality to create and manipulate SVG images
    "! using the same public API as zcl_bmp but generating SVG output instead

    TYPES ts_sizes TYPE zcl_bmp=>ts_sizes.

    METHODS:
      "! Creates a new SVG image instance
      "! @parameter iv_width | Width of the image in pixels
      "! @parameter iv_height | Height of the image in pixels
      constructor
        IMPORTING
          iv_width  TYPE i
          iv_height TYPE i,

      "! Draws a single pixel at specified coordinates (implemented as 1x1 rect)
      draw_pixel
        IMPORTING
          io_color TYPE REF TO zcl_bmp_color
          io_coord TYPE REF TO zcl_bmp_coord,

      "! Draws a line between two points
      draw_line
        IMPORTING
          io_color       TYPE REF TO zcl_bmp_color
          io_coord_start TYPE REF TO zcl_bmp_coord
          io_coord_end   TYPE REF TO zcl_bmp_coord,

      "! Draws a rectangle
      draw_rectangle
        IMPORTING
          io_color  TYPE REF TO zcl_bmp_color
          io_coord1 TYPE REF TO zcl_bmp_coord
          io_coord2 TYPE REF TO zcl_bmp_coord
          io_coord3 TYPE REF TO zcl_bmp_coord
          io_coord4 TYPE REF TO zcl_bmp_coord
          iv_fill   TYPE abap_bool OPTIONAL,

      "! Draws a triangle
      draw_triangle
        IMPORTING
          io_color  TYPE REF TO zcl_bmp_color
          io_coord1 TYPE REF TO zcl_bmp_coord
          io_coord2 TYPE REF TO zcl_bmp_coord
          io_coord3 TYPE REF TO zcl_bmp_coord
          iv_fill   TYPE abap_bool OPTIONAL,

      "! Draws a polygon
      draw_polygon
        IMPORTING
          io_color  TYPE REF TO zcl_bmp_color
          it_coords TYPE table
          iv_fill   TYPE abap_bool OPTIONAL,

      "! Draws text at specified coordinates
      draw_text
        IMPORTING
          iv_text  TYPE string
          io_coord TYPE REF TO zcl_bmp_coord,

      "! Draws a single character (delegates to draw_text)
      draw_symbol
        IMPORTING
          iv_symbol TYPE c
          io_coord  TYPE REF TO zcl_bmp_coord,

      "! Gets the dimensions of the image
      get_image_sizes
        RETURNING VALUE(rs_sizes) TYPE zcl_bmp=>ts_sizes,

      "! Dummy font size info (not supported for SVG)
      get_font_sizes
        RETURNING VALUE(rs_sizes) TYPE zcl_bmp=>ts_sizes,

      "! Returns the SVG content as XSTRING
      get_xstring REDEFINITION,

      "! Not supported for SVG yet – does nothing
      import_from_xstring REDEFINITION,

      "! Returns BASE64 web link with correct MIME type
      get_base64_web_link REDEFINITION.

  PROTECTED SECTION.
    DATA: mv_width  TYPE i,
          mv_height TYPE i.
  PRIVATE SECTION.
    DATA: mt_elements TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    METHODS color_to_css
      IMPORTING io_color TYPE REF TO zcl_bmp_color
      RETURNING VALUE(rv_css_color) TYPE string.
ENDCLASS.


CLASS zcl_svg IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mv_width  = iv_width.
    mv_height = iv_height.
    CLEAR mt_elements.
  ENDMETHOD.

  METHOD color_to_css.
    DATA lv_rgb TYPE xstring.
    lv_rgb = io_color->get_rgb( ).

    DATA: lv_r TYPE i, lv_g TYPE i, lv_b TYPE i.

    " Convert individual bytes (B,G,R order) to integers
    DATA(lo_conv) = cl_abap_conv_in_ce=>create( endian = 'B' ).
    lo_conv->convert( EXPORTING input = lv_rgb+2(1) IMPORTING data = lv_r ). " R
    lo_conv->convert( EXPORTING input = lv_rgb+1(1) IMPORTING data = lv_g ). " G
    lo_conv->convert( EXPORTING input = lv_rgb+0(1) IMPORTING data = lv_b ). " B

    rv_css_color = |rgb({ lv_r },{ lv_g },{ lv_b })|.
  ENDMETHOD.

  METHOD draw_pixel.
    DATA(lv_color) = color_to_css( io_color ).
    DATA(lv_x) = io_coord->get_x( ).
    DATA(lv_y) = io_coord->get_y( ).
    APPEND |<rect x="{ lv_x }" y="{ lv_y }" width="1" height="1" fill="{ lv_color }" />| TO mt_elements.
  ENDMETHOD.

  METHOD draw_line.
    DATA(lv_color) = color_to_css( io_color ).
    APPEND |<line x1="{ io_coord_start->get_x( ) }" y1="{ io_coord_start->get_y( ) }" x2="{ io_coord_end->get_x( ) }" y2="{ io_coord_end->get_y( ) }" stroke="{ lv_color }" />| TO mt_elements.
  ENDMETHOD.

  METHOD draw_polygon.
    DATA(lv_color) = color_to_css( io_color ).
    DATA(lv_points) = ''.

    FIELD-SYMBOLS <l_coord> TYPE REF TO zcl_bmp_coord.
    LOOP AT it_coords ASSIGNING <l_coord>.
      lv_points = |{ lv_points }{ <l_coord>->get_x( ) },{ <l_coord>->get_y( ) } |.
    ENDLOOP.

    DATA(lv_fill_attr) = COND string( WHEN iv_fill = abap_true THEN lv_color ELSE 'none' ).

    APPEND |<polygon points="{ lv_points }" fill="{ lv_fill_attr }" stroke="{ lv_color }" />| TO mt_elements.
  ENDMETHOD.

  METHOD draw_rectangle.
    DATA lt_coords TYPE STANDARD TABLE OF REF TO zcl_bmp_coord.
    APPEND io_coord1 TO lt_coords.
    APPEND io_coord2 TO lt_coords.
    APPEND io_coord3 TO lt_coords.
    APPEND io_coord4 TO lt_coords.
    draw_polygon( io_color = io_color it_coords = lt_coords iv_fill = iv_fill ).
  ENDMETHOD.

  METHOD draw_triangle.
    DATA lt_coords TYPE STANDARD TABLE OF REF TO zcl_bmp_coord.
    APPEND io_coord1 TO lt_coords.
    APPEND io_coord2 TO lt_coords.
    APPEND io_coord3 TO lt_coords.
    draw_polygon( io_color = io_color it_coords = lt_coords iv_fill = iv_fill ).
  ENDMETHOD.

  METHOD draw_text.
    DATA(lv_color) = color_to_css( NEW zcl_bmp_color( iv_r = 0 iv_g = 0 iv_b = 0 ) ).
    APPEND |<text x="{ io_coord->get_x( ) }" y="{ io_coord->get_y( ) }" fill="{ lv_color }">{ iv_text }</text>| TO mt_elements.
  ENDMETHOD.

  METHOD draw_symbol.
    draw_text( iv_text = iv_symbol io_coord = io_coord ).
  ENDMETHOD.

  METHOD get_image_sizes.
    rs_sizes = VALUE #( width = mv_width height = mv_height ).
  ENDMETHOD.

  METHOD get_font_sizes.
    rs_sizes = VALUE #( width = 0 height = 0 ).
  ENDMETHOD.

  METHOD get_xstring.
    DATA lv_svg TYPE string.
    lv_svg = |<?xml version="1.0" encoding="UTF-8" standalone="no"?>| &&
             |<svg xmlns="http://www.w3.org/2000/svg" width="{ mv_width }" height="{ mv_height }" viewBox="0 0 { mv_width } { mv_height }">|.

    " Concatenate all stored elements
    LOOP AT mt_elements INTO DATA(lv_elem).
      lv_svg = lv_svg && lv_elem.
    ENDLOOP.

    lv_svg = lv_svg && '</svg>'.

    " Convert to xstring (UTF-8)
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text      = lv_svg
        mimetype  = 'text/plain'
      IMPORTING
        buffer    = rv_xstring.
  ENDMETHOD.

  METHOD import_from_xstring.
    " Not implemented for SVG yet
    RETURN.
  ENDMETHOD.

  METHOD get_base64_web_link.
    rv_b64web_link = |data:image/svg+xml;base64,{ me->get_base64( ) }|.
  ENDMETHOD.

ENDCLASS. 