CLASS zcl_bmp DEFINITION PUBLIC CREATE PUBLIC
    INHERITING FROM zcl_image_file.
  PUBLIC SECTION.

    "! BMP image manipulation class
    "! @description Provides functionality to create, modify and manipulate BMP images
    "! including drawing basic shapes, text and importing/exporting BMP files

    TYPES: tt_pixel TYPE x LENGTH 3,
           BEGIN OF ts_sizes,
             width  TYPE i,
             height TYPE i,
           END OF ts_sizes.
    METHODS:
      "! Creates a new BMP image instance
      "! @parameter iv_width | Width of the image in pixels
      "! @parameter iv_height | Height of the image in pixels
      "! @parameter iv_font_object | Optional SMW0 object name containing font bitmap
      "! @parameter iv_glyph_width | Width of each font glyph in pixels
      "! @parameter iv_glyph_height | Height of each font glyph in pixels
      constructor
        IMPORTING
          iv_width        TYPE i
          iv_height       TYPE i
          iv_font_object  TYPE w3objid OPTIONAL
          iv_glyph_width  TYPE i DEFAULT 32
          iv_glyph_height TYPE i DEFAULT 32,

      "! Draws a single pixel at specified coordinates
      "! @parameter io_color | Color object containing RGB values
      "! @parameter io_coord | Coordinate object containing x,y position
      draw_pixel
        IMPORTING
          io_color TYPE REF TO zcl_bmp_color
          io_coord TYPE REF TO zcl_bmp_coord,

      "! Draws a line between two points
      "! @parameter io_color | Color object containing RGB values
      "! @parameter io_coord_start | Starting coordinate of the line
      "! @parameter io_coord_end | Ending coordinate of the line
      draw_line
        IMPORTING
          io_color       TYPE REF TO zcl_bmp_color
          io_coord_start TYPE REF TO zcl_bmp_coord
          io_coord_end   TYPE REF TO zcl_bmp_coord,

      "! Draws a rectangle using four coordinates
      "! @parameter io_color | Color object containing RGB values
      "! @parameter io_coord1 | First corner coordinate
      "! @parameter io_coord2 | Second corner coordinate
      "! @parameter io_coord3 | Third corner coordinate
      "! @parameter io_coord4 | Fourth corner coordinate
      "! @parameter iv_fill | Optional flag to fill the rectangle
      draw_rectangle
        IMPORTING
          io_color  TYPE REF TO zcl_bmp_color
          io_coord1 TYPE REF TO zcl_bmp_coord
          io_coord2 TYPE REF TO zcl_bmp_coord
          io_coord3 TYPE REF TO zcl_bmp_coord
          io_coord4 TYPE REF TO zcl_bmp_coord
          iv_fill   TYPE abap_bool OPTIONAL,

      "! Draws a triangle using three coordinates
      "! @parameter io_color | Color object containing RGB values
      "! @parameter io_coord1 | First corner coordinate
      "! @parameter io_coord2 | Second corner coordinate
      "! @parameter io_coord3 | Third corner coordinate
      "! @parameter iv_fill | Optional flag to fill the triangle
      draw_triangle
        IMPORTING
          io_color  TYPE REF TO zcl_bmp_color
          io_coord1 TYPE REF TO zcl_bmp_coord
          io_coord2 TYPE REF TO zcl_bmp_coord
          io_coord3 TYPE REF TO zcl_bmp_coord
          iv_fill   TYPE abap_bool OPTIONAL,

      "! Draws a polygon using a table of coordinates
      "! @parameter io_color | Color object containing RGB values
      "! @parameter it_coords | Table of coordinates defining the polygon vertices
      "! @parameter iv_fill | Optional flag to fill the polygon
      draw_polygon
        IMPORTING
          io_color  TYPE REF TO zcl_bmp_color
          it_coords TYPE table
          iv_fill   TYPE abap_bool OPTIONAL,

      "! Converts the BMP image to XSTRING format
      "! @parameter rv_xstring | Binary representation of the BMP image
      get_xstring REDEFINITION,

      "! Imports BMP image data from XSTRING format
      "! @parameter iv_xstring | Binary data to import
      import_from_xstring REDEFINITION,

      "! Draws text at specified coordinates using the loaded font
      "! @parameter iv_text | Text to draw
      "! @parameter io_coord | Starting coordinate for the text
      draw_text
        IMPORTING
          iv_text  TYPE string
          io_coord TYPE REF TO zcl_bmp_coord,

      "! Draws a single character at specified coordinates
      "! @parameter iv_symbol | Character to draw
      "! @parameter io_coord | Coordinate where to draw the character
      draw_symbol
        IMPORTING
          iv_symbol TYPE c
          io_coord  TYPE REF TO zcl_bmp_coord,

      "! Gets the dimensions of the loaded font
      "! @parameter rs_sizes | Structure containing font width and height
      get_font_sizes
        RETURNING VALUE(rs_sizes) TYPE ts_sizes,

      "! Gets the dimensions of the image
      "! @parameter rs_sizes | Structure containing image width and height
      get_image_sizes
        RETURNING VALUE(rs_sizes) TYPE ts_sizes.
  PROTECTED SECTION.
    DATA: mv_width       TYPE i,
          mv_height      TYPE i,
          mv_font_width  TYPE i,
          mv_font_height TYPE i.
  PRIVATE   SECTION.
    TYPES: BEGIN OF ts_bmp_header,
             width  TYPE i,
             height TYPE i,
             bpp    TYPE i,
             offset TYPE i,
           END OF ts_bmp_header.

    DATA: mt_pixels       TYPE TABLE OF tt_pixel,
          mv_font_object  TYPE w3objid,
          mv_glyph_width  TYPE i,
          mv_glyph_height TYPE i,
          mt_font_pixels  TYPE TABLE OF tt_pixel.
    METHODS:
      "! Loads bitmap font from SMW0 object
      "! @description Reads and processes font bitmap data from SMW0 repository
      load_bmp_font,

      "! Parses BMP file header information
      "! @parameter iv_file | Binary data containing BMP file
      "! @parameter rs_header | Structure containing parsed header information
      parse_bmp_header
        IMPORTING iv_file          TYPE xstring
        RETURNING VALUE(rs_header) TYPE ts_bmp_header.
ENDCLASS.

CLASS zcl_bmp IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    mv_width  = iv_width.
    mv_height = iv_height.
    IF iv_font_object IS SUPPLIED.
      mv_font_object = iv_font_object.
    ELSE.
      mv_font_object = 'ZBMPFONT'.
    ENDIF.
    IF iv_glyph_width IS SUPPLIED.
      mv_glyph_width = iv_glyph_width.
    ELSE.
      mv_glyph_width = 28.
    ENDIF.
    IF iv_glyph_height IS SUPPLIED.
      mv_glyph_height = iv_glyph_height.
    ELSE.
      mv_glyph_height = 32.
    ENDIF.
    CLEAR mt_pixels.
    DATA lv_white TYPE x LENGTH 3.
    lv_white = 'FFFFFF'.
    DO mv_width * mv_height TIMES.
      APPEND lv_white TO mt_pixels.
    ENDDO.
  ENDMETHOD.

  METHOD draw_pixel.
    DATA lv_index TYPE i.
    lv_index = ( io_coord->get_y( ) * mv_width ) + io_coord->get_x( ) + 1.
    IF lv_index > 0 AND lv_index <= lines( mt_pixels ).
      DATA lv_color TYPE x LENGTH 3.
      lv_color = io_color->get_rgb( ).
      mt_pixels[ lv_index ] = lv_color.
    ENDIF.
  ENDMETHOD.

  METHOD draw_line.
    DATA: x0 TYPE i, y0 TYPE i, x1 TYPE i, y1 TYPE i.
    x0 = io_coord_start->get_x( ).
    y0 = io_coord_start->get_y( ).
    x1 = io_coord_end->get_x( ).
    y1 = io_coord_end->get_y( ).
    DATA dx TYPE i.
    dx = abs( x1 - x0 ).

    DATA dy TYPE i.
    dy = abs( y1 - y0 ).
    DATA sx TYPE i.
    sx = COND #( WHEN x0 < x1 THEN 1 ELSE -1 ).
    DATA sy TYPE i.
    sy = COND #( WHEN y0 < y1 THEN 1 ELSE -1 ).
    DATA err TYPE i.
    err = dx - dy.
    DATA e2 TYPE i.
    WHILE x0 <> x1 OR y0 <> y1.
      draw_pixel( io_color = io_color io_coord = NEW zcl_bmp_coord( iv_x =  x0 iv_y =  y0 ) ).
      e2 = 2 * err.
      IF e2 > dy.
        err = err - dy.
        x0 = x0 + sx.
      ENDIF.
      IF e2 < dx.
        err = err + dx.
        y0 = y0 + sy.
      ENDIF.
    ENDWHILE.
    draw_pixel( io_color = io_color io_coord = NEW zcl_bmp_coord( iv_x =  x0 iv_y =  y0 ) ).
  ENDMETHOD.

  METHOD draw_polygon.
    DATA lt_coords TYPE STANDARD TABLE OF REF TO zcl_bmp_coord.
    DATA lo_coord1 TYPE REF TO zcl_bmp_coord.
    DATA lo_coord2 TYPE REF TO zcl_bmp_coord.
    DATA lv_count TYPE i.
    DATA i TYPE i.

    lt_coords = it_coords.
    lv_count = lines( lt_coords ).

    IF lv_count < 2.
      RETURN.
    ENDIF.

    LOOP AT lt_coords INTO lo_coord1 FROM 1 TO lv_count.
      IF sy-tabix = lv_count.
        lo_coord2 = lt_coords[ 1 ].
      ELSE.
        lo_coord2 = lt_coords[ sy-tabix + 1 ].
      ENDIF.
      draw_line(
        io_color = io_color
        io_coord_start = lo_coord1
        io_coord_end   = lo_coord2
      ).
    ENDLOOP.

    IF iv_fill = abap_true.
      DATA: lv_min_y   TYPE i, lv_max_y TYPE i, lv_y TYPE i,
            lt_xints   TYPE STANDARD TABLE OF i,
            lo_coord_a TYPE REF TO zcl_bmp_coord,
            lo_coord_b TYPE REF TO zcl_bmp_coord,
            lv_xa      TYPE i, lv_ya TYPE i, lv_xb TYPE i, lv_yb TYPE i,
            lv_x       TYPE i, lv_x1 TYPE i, lv_x2 TYPE i, lv_i TYPE i.

      " Find min and max y
      READ TABLE lt_coords INDEX 1 INTO lo_coord1.
      lv_min_y = lo_coord1->get_y( ).
      lv_max_y = lo_coord1->get_y( ).
      LOOP AT lt_coords INTO lo_coord1.
        IF lo_coord1->get_y( ) < lv_min_y.
          lv_min_y = lo_coord1->get_y( ).
        ENDIF.
        IF lo_coord1->get_y( ) > lv_max_y.
          lv_max_y = lo_coord1->get_y( ).
        ENDIF.
      ENDLOOP.

      " For each scanline
      DO lv_max_y - lv_min_y + 1 TIMES.
        lv_y = lv_min_y + sy-index - 1.
        CLEAR lt_xints.

        " For each edge
        LOOP AT lt_coords INTO lo_coord_a FROM 1 TO lv_count.
          IF sy-tabix = lv_count.
            lo_coord_b = lt_coords[ 1 ].
          ELSE.
            lo_coord_b = lt_coords[ sy-tabix + 1 ].
          ENDIF.

          lv_ya = lo_coord_a->get_y( ).
          lv_yb = lo_coord_b->get_y( ).
          lv_xa = lo_coord_a->get_x( ).
          lv_xb = lo_coord_b->get_x( ).

          IF ( ( lv_ya <= lv_y AND lv_y < lv_yb ) OR ( lv_yb <= lv_y AND lv_y < lv_ya ) ) AND ( lv_ya <> lv_yb ).
            " Compute intersection
            lv_x = lv_xa + ( ( lv_y - lv_ya ) * ( lv_xb - lv_xa ) ) / ( lv_yb - lv_ya ).
            APPEND lv_x TO lt_xints.
          ENDIF.
        ENDLOOP.

        " Sort intersections
        SORT lt_xints ASCENDING.

        " Fill between pairs
        lv_i = 1.
        WHILE lv_i < lines( lt_xints ).
          lv_x1 = lt_xints[ lv_i ].
          lv_x2 = lt_xints[ lv_i + 1 ].
          DO lv_x2 - lv_x1 + 1 TIMES.
            draw_pixel(
              io_color = io_color
              io_coord = NEW zcl_bmp_coord( iv_x =  lv_x1 + sy-index - 1 iv_y =  lv_y )
            ).
          ENDDO.
          lv_i = lv_i + 2.
        ENDWHILE.
      ENDDO.
    ENDIF.
  ENDMETHOD.

  METHOD draw_rectangle.
    DATA lt_coords TYPE STANDARD TABLE OF REF TO zcl_bmp_coord.
    APPEND io_coord1 TO lt_coords.
    APPEND io_coord2 TO lt_coords.
    APPEND io_coord3 TO lt_coords.
    APPEND io_coord4 TO lt_coords.
    draw_polygon(
      io_color = io_color
      it_coords = lt_coords
      iv_fill = iv_fill
    ).
  ENDMETHOD.

  METHOD draw_triangle.
    DATA lt_coords TYPE STANDARD TABLE OF REF TO zcl_bmp_coord.
    APPEND io_coord1 TO lt_coords.
    APPEND io_coord2 TO lt_coords.
    APPEND io_coord3 TO lt_coords.
    draw_polygon(
      io_color = io_color
      it_coords = lt_coords
      iv_fill = iv_fill
    ).
  ENDMETHOD.

  METHOD get_xstring.
    DATA: lv_xstring    TYPE xstring,
          lv_header     TYPE x LENGTH 54,
          lv_size       TYPE i,
          lv_offset     TYPE i VALUE 54,
          lv_width      TYPE i,
          lv_height     TYPE i,
          lv_planes     TYPE i VALUE 1,
          lv_bpp        TYPE i VALUE 24,
          lv_image_size TYPE i,
          lv_pad        TYPE i,
          lv_row        TYPE xstring,
          lv_pixel      TYPE tt_pixel,
          lv_i          TYPE i,
          lv_j          TYPE i.
    lv_width = mv_width.
    lv_height = mv_height.
    lv_pad = ( 4 - ( lv_width * 3 ) MOD 4 ) MOD 4.
    lv_image_size = ( ( lv_width * 3 ) + lv_pad ) * lv_height.
    lv_size = lv_offset + lv_image_size.
    CLEAR lv_header.
    lv_header = '424D'. " 'BM' signature
    DATA lv_tmp_x TYPE x LENGTH 4.
    DATA lv_tmp_x2 TYPE x LENGTH 2.
    DATA lv_tmp_x1 TYPE x LENGTH 1.

    cl_abap_conv_out_ce=>create( )->convert( EXPORTING data = lv_size IMPORTING buffer = lv_tmp_x ).
    lv_header+2(4) = lv_tmp_x.
    cl_abap_conv_out_ce=>create( )->convert( EXPORTING data = 0 IMPORTING buffer = lv_tmp_x ).
    lv_header+6(4) = lv_tmp_x.
    cl_abap_conv_out_ce=>create( )->convert( EXPORTING data = lv_offset IMPORTING buffer = lv_tmp_x ).
    lv_header+10(4) = lv_tmp_x.
    cl_abap_conv_out_ce=>create( )->convert( EXPORTING data = 40 IMPORTING buffer = lv_tmp_x ).
    lv_header+14(4) = lv_tmp_x.
    cl_abap_conv_out_ce=>create( )->convert( EXPORTING data = lv_width IMPORTING buffer = lv_tmp_x ).
    lv_header+18(4) = lv_tmp_x.
    cl_abap_conv_out_ce=>create( )->convert( EXPORTING data = lv_height IMPORTING buffer = lv_tmp_x ).
    lv_header+22(4) = lv_tmp_x.
    cl_abap_conv_out_ce=>create( )->convert( EXPORTING data = lv_planes IMPORTING buffer = lv_tmp_x2 ).
    lv_header+26(2) = lv_tmp_x2.
    cl_abap_conv_out_ce=>create( )->convert( EXPORTING data = lv_bpp IMPORTING buffer = lv_tmp_x2 ).
    lv_header+28(2) = lv_tmp_x2.
    cl_abap_conv_out_ce=>create( )->convert( EXPORTING data = 0 IMPORTING buffer = lv_tmp_x ).
    lv_header+30(4) = lv_tmp_x.
    cl_abap_conv_out_ce=>create( )->convert( EXPORTING data = lv_image_size IMPORTING buffer = lv_tmp_x ).
    lv_header+34(4) = lv_tmp_x.
    cl_abap_conv_out_ce=>create( )->convert( EXPORTING data = 0 IMPORTING buffer = lv_tmp_x ).
    lv_header+38(4) = lv_tmp_x.
    cl_abap_conv_out_ce=>create( )->convert( EXPORTING data = 0 IMPORTING buffer = lv_tmp_x ).
    lv_header+42(4) = lv_tmp_x.
    cl_abap_conv_out_ce=>create( )->convert( EXPORTING data = 0 IMPORTING buffer = lv_tmp_x ).
    lv_header+46(4) = lv_tmp_x.
    cl_abap_conv_out_ce=>create( )->convert( EXPORTING data = 0 IMPORTING buffer = lv_tmp_x ).
    lv_header+50(4) = lv_tmp_x.
    lv_xstring = lv_header.
    DO lv_height TIMES.
      DATA(lv_sy_index1) = sy-index.
      CLEAR lv_row.
      DO lv_width TIMES.
        DATA(lv_sy_index2) = sy-index.
        lv_i = ( lv_height - lv_sy_index1 ) * lv_width + lv_sy_index2.
        lv_pixel = mt_pixels[ lv_i ].
        DATA lv_pixel_xstring TYPE xstring.
        lv_pixel_xstring = lv_pixel.
        CONCATENATE lv_row lv_pixel_xstring INTO lv_row IN BYTE MODE.
      ENDDO.
      IF lv_pad > 0.
        cl_abap_conv_out_ce=>create( )->convert( EXPORTING data = 0 IMPORTING buffer = lv_tmp_x ).
        lv_pixel_xstring = lv_tmp_x(lv_pad).
        CONCATENATE lv_row lv_pixel_xstring INTO lv_row IN BYTE MODE.
      ENDIF.
      CONCATENATE lv_xstring lv_row INTO lv_xstring IN BYTE MODE.
    ENDDO.
    rv_xstring = lv_xstring.
  ENDMETHOD.

  METHOD import_from_xstring.
    DATA:
      lv_offset     TYPE i,
      lv_pad        TYPE i,
      lv_image_size TYPE i,
      lv_x          TYPE xstring,
      lv_pixel      TYPE x LENGTH 3,
      lv_pos        TYPE i.

    DATA(ls_header) = parse_bmp_header( iv_xstring ).

    IF ls_header-bpp <> 24.
      " Only 24bpp supported
      RETURN.
    ENDIF.
    mv_width = ls_header-width.
    mv_height = ls_header-height.
    lv_offset = ls_header-offset.

    CLEAR mt_pixels.
    lv_pad = ( 4 - ( mv_width * 3 ) MOD 4 ) MOD 4.
    lv_image_size = ( ( mv_width * 3 ) + lv_pad ) * mv_height.
    lv_x = iv_xstring+lv_offset(lv_image_size).

    " BMP files store image data from bottom to top, so we need to read rows in reverse order
    " Calculate the starting position for the last row (bottom of image)
    lv_pos = lv_image_size - ( ( mv_width * 3 ) + lv_pad ).

    DO mv_height TIMES.
      " Read each row from left to right
      DO mv_width TIMES.
        lv_pixel = lv_x+lv_pos(3).
        APPEND lv_pixel TO mt_pixels.
        lv_pos = lv_pos + 3.
      ENDDO.
      " Move to the previous row (going up in the image)
      lv_pos = lv_pos - ( ( mv_width * 3 ) + lv_pad ) - ( mv_width * 3 ) - lv_pad.
    ENDDO.
  ENDMETHOD.

  METHOD draw_text.
    DATA: lv_len   TYPE i,
          lv_idx   TYPE i,
          lv_char  TYPE c LENGTH 1,
          lv_x     TYPE i,
          lv_y     TYPE i,
          lo_coord TYPE REF TO zcl_bmp_coord.

    lv_len = strlen( iv_text ).
    lv_x = io_coord->get_x( ).
    lv_y = io_coord->get_y( ).

    DO lv_len TIMES.
      lv_idx = sy-index - 1.
      lv_char = iv_text+lv_idx(1).
      CREATE OBJECT lo_coord EXPORTING iv_x = lv_x iv_y = lv_y.
      draw_symbol( iv_symbol = lv_char io_coord = lo_coord ).
      lv_x = lv_x + mv_glyph_width.
    ENDDO.
  ENDMETHOD.

  METHOD draw_symbol.
    DATA: lv_code        TYPE i,
          lv_glyph_x     TYPE i,
          lv_glyph_y     TYPE i,
          lv_src_x       TYPE i,
          lv_src_y       TYPE i,
          lv_dst_x       TYPE i,
          lv_dst_y       TYPE i,
          lv_glyph_index TYPE i,
          lv_symbol      TYPE c LENGTH 1,
          lv_code_x      TYPE xstring.

    lv_symbol = iv_symbol.
    cl_abap_conv_out_ce=>create( encoding = '1504' )->convert(
        EXPORTING data = lv_symbol
        IMPORTING buffer = lv_code_x
        ).

    lv_code = lv_code_x.
    IF lv_code < 0 OR lv_code > 255.
      RETURN.
    ENDIF.


    IF mv_font_object IS INITIAL.
      RETURN.
    ENDIF.

    " Load font if not loaded yet
    IF mv_font_width IS INITIAL OR mv_font_height IS INITIAL OR mt_font_pixels IS INITIAL.
      load_bmp_font( ).
    ENDIF.

    IF mv_font_width IS INITIAL OR mv_font_height IS INITIAL OR mt_font_pixels IS INITIAL.
      RETURN.
    ENDIF.

    DATA: lv_font_glyphs      TYPE i,
          lv_font_glyph_index TYPE i,
          lv_font_pixel_idx   TYPE i,
          lv_pixel            TYPE x LENGTH 3.

    lv_font_glyphs = mv_font_width / mv_glyph_width.
    IF lv_code >= lv_font_glyphs.
      RETURN.
    ENDIF.
    lv_font_glyph_index = lv_code.
    lv_glyph_x = lv_font_glyph_index * mv_glyph_width.
    lv_glyph_y = 0.

    DO mv_glyph_height TIMES.
      DATA(lv_sy_index1) = sy-index.
      lv_src_y = sy-index - 1.
      lv_dst_y = io_coord->get_y( ) + lv_src_y.
      IF lv_dst_y < 0 OR lv_dst_y >= mv_height.
        CONTINUE.
      ENDIF.
      DO mv_glyph_width TIMES.
        DATA(lv_sy_index2) = sy-index.
        lv_src_x = lv_glyph_x + lv_sy_index2 - 1.
        lv_dst_x = io_coord->get_x( ) + lv_sy_index2 - 1.
        IF lv_dst_x < 0 OR lv_dst_x >= mv_width.
          CONTINUE.
        ENDIF.
        lv_font_pixel_idx = lv_src_y * mv_font_width + lv_src_x + 1.
        IF lv_font_pixel_idx > 0 AND lv_font_pixel_idx <= lines( mt_font_pixels ).
          lv_pixel = mt_font_pixels[ lv_font_pixel_idx ].
          mt_pixels[ lv_dst_y * mv_width + lv_dst_x + 1 ] = lv_pixel.
        ENDIF.
      ENDDO.
    ENDDO.
  ENDMETHOD.

  METHOD load_bmp_font.
    DATA lv_font_xstring TYPE xstring.
    CLEAR: mt_font_pixels, mv_font_width, mv_font_height.
    lv_font_xstring = load_xstring_from_smw0( iv_file_name = mv_font_object ).
    IF lv_font_xstring IS INITIAL.
      CLEAR: mt_font_pixels, mv_font_width, mv_font_height.
      RETURN.
    ENDIF.

    DATA(ls_header) = parse_bmp_header( lv_font_xstring ).

    DATA: lv_image_size TYPE i,
          lv_x          TYPE xstring,
          lv_pixel      TYPE x LENGTH 3,
          lv_pad        TYPE i,
          lv_pos        TYPE i,
          lv_offset     TYPE i.

    lv_offset = ls_header-offset.
    mv_font_width = ls_header-width.
    mv_font_height = ls_header-height.

    IF ls_header-bpp <> 24.
      CLEAR: mt_font_pixels, mv_font_width, mv_font_height.
      RETURN.
    ENDIF.
    lv_pad = ( 4 - ( mv_font_width * 3 ) MOD 4 ) MOD 4.
    lv_image_size = ( ( mv_font_width * 3 ) + lv_pad ) * mv_font_height.
    lv_x = lv_font_xstring+lv_offset(lv_image_size).
    CLEAR mt_font_pixels.

    " BMP files store image data from bottom to top, so we need to read rows in reverse order
    " Calculate the starting position for the last row (bottom of image)
    lv_pos = lv_image_size - ( ( mv_font_width * 3 ) + lv_pad ).

    DO mv_font_height TIMES.
      " Read each row from left to right
      DO mv_font_width TIMES.
        lv_pixel = lv_x+lv_pos(3).
        APPEND lv_pixel TO mt_font_pixels.
        lv_pos = lv_pos + 3.
      ENDDO.
      " Move to the previous row (going up in the image)
      lv_pos = lv_pos - ( ( mv_font_width * 3 ) + lv_pad ) - ( mv_font_width * 3 ) - lv_pad.
    ENDDO.
  ENDMETHOD.
  METHOD get_font_sizes.
    rs_sizes = VALUE #(
        width = mv_font_width
        height = mv_font_height
     ).
  ENDMETHOD.

  METHOD get_image_sizes.
    rs_sizes = VALUE #(
        width = mv_width
        height = mv_height
     ).
  ENDMETHOD.

  METHOD parse_bmp_header.
    DATA: lv_header     TYPE x LENGTH 54.

    lv_header = iv_file(54).

    DATA(lo_converter) = cl_abap_conv_in_ce=>create( endian = 'L' ).

    " Width (offset 18, 4 bytes, little-endian)
    lo_converter->convert(
      EXPORTING
        input = lv_header+18(4)
      IMPORTING
        data = rs_header-width ).

    " Height (offset 22, 4 bytes, little-endian)
    lo_converter->convert(
      EXPORTING
        input = lv_header+22(4)
      IMPORTING
        data = rs_header-height ).

    " Bits per pixel (offset 28, 4 bytes, little-endian)
    lo_converter->convert(
      EXPORTING
        input = lv_header+28(4)
      IMPORTING
        data = rs_header-bpp ).

    " Offset (offset 10, 4 bytes, little-endian)
    lo_converter->convert(
      EXPORTING
        input = lv_header+10(4)
      IMPORTING
        data = rs_header-offset ).
  ENDMETHOD.

ENDCLASS.

