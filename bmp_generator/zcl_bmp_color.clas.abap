
CLASS zcl_bmp_color DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.

    TYPES: tt_pixel TYPE x LENGTH 3.
    METHODS:
      constructor IMPORTING iv_r TYPE i iv_g TYPE i iv_b TYPE i,
      get_rgb RETURNING VALUE(rv_rgb) TYPE tt_pixel.
  PRIVATE SECTION.
    DATA: mv_r TYPE i,
          mv_g TYPE i,
          mv_b TYPE i.
ENDCLASS.

CLASS zcl_bmp_color IMPLEMENTATION.
  METHOD constructor.
    mv_r = iv_r.
    mv_g = iv_g.
    mv_b = iv_b.
  ENDMETHOD.
  METHOD get_rgb.
    DATA lv_tmp_x TYPE x LENGTH 1.
    cl_abap_conv_out_ce=>create( )->convert( EXPORTING data = mv_b IMPORTING buffer = lv_tmp_x ).
    rv_rgb = lv_tmp_x.
    cl_abap_conv_out_ce=>create( )->convert( EXPORTING data = mv_g IMPORTING buffer = lv_tmp_x ).
    rv_rgb = rv_rgb && lv_tmp_x.
    cl_abap_conv_out_ce=>create( )->convert( EXPORTING data = mv_r IMPORTING buffer = lv_tmp_x ).
    rv_rgb = rv_rgb && lv_tmp_x.
  ENDMETHOD.
ENDCLASS.
