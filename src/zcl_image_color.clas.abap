CLASS zcl_image_color DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.

    TYPES: tt_pixel TYPE x LENGTH 3.
    METHODS:
      constructor IMPORTING iv_r TYPE i iv_g TYPE i iv_b TYPE i,
      get_rgb_hex RETURNING VALUE(rv_rgb) TYPE tt_pixel,
      get_red RETURNING VALUE(rv_red) TYPE i,
      get_green RETURNING VALUE(rv_green) TYPE i,
      get_blue RETURNING VALUE(rv_blue) TYPE i,

      to_css
      RETURNING VALUE(rv_css_color) TYPE string.
      .
  PRIVATE SECTION.
    DATA: mv_r TYPE i,
          mv_g TYPE i,
          mv_b TYPE i.
ENDCLASS.



CLASS ZCL_IMAGE_COLOR IMPLEMENTATION.


  METHOD constructor.
    mv_r = iv_r.
    mv_g = iv_g.
    mv_b = iv_b.
  ENDMETHOD.


  METHOD get_blue.
    rv_blue = mv_b.
  ENDMETHOD.


  METHOD get_green.
    rv_green = mv_g.
  ENDMETHOD.


  METHOD get_red.
    rv_red = mv_r.
  ENDMETHOD.


  METHOD get_rgb_hex.
    DATA lv_tmp_x TYPE x LENGTH 1.
    DATA(lo_converter) = cl_abap_conv_out_ce=>create( ).
    lo_converter->convert( EXPORTING data = mv_b IMPORTING buffer = lv_tmp_x ).
    rv_rgb = lv_tmp_x.
    lo_converter->convert( EXPORTING data = mv_g IMPORTING buffer = lv_tmp_x ).
    rv_rgb = rv_rgb && lv_tmp_x.
    lo_converter->convert( EXPORTING data = mv_r IMPORTING buffer = lv_tmp_x ).
    rv_rgb = rv_rgb && lv_tmp_x.
  ENDMETHOD.


  METHOD to_css.
    rv_css_color = |rgb({ mv_r },{ mv_g },{ mv_b })|.
  ENDMETHOD.
ENDCLASS.
