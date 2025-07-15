CLASS zcl_bmp_sine_graph DEFINITION PUBLIC CREATE PUBLIC
    INHERITING FROM zcl_bmp.
  PUBLIC SECTION.
    METHODS:
      draw_sine IMPORTING iv_amplitude TYPE f DEFAULT 1
                          iv_frequency TYPE f DEFAULT 1
                          iv_phase     TYPE f DEFAULT 0.
  PRIVATE SECTION.
    METHODS:
      draw_axes.
ENDCLASS.

CLASS zcl_bmp_sine_graph IMPLEMENTATION.

  METHOD draw_axes.
    DATA: lo_color  TYPE REF TO zcl_bmp_color,
          lo_coord1 TYPE REF TO zcl_bmp_coord,
          lo_coord2 TYPE REF TO zcl_bmp_coord.


    CREATE OBJECT lo_color EXPORTING iv_r = 0 iv_g = 0 iv_b = 0.

    " Draw header
    CREATE OBJECT lo_coord1 EXPORTING iv_x = 80 iv_y = 10.
    draw_text( iv_text = '[График синуса]' io_coord = lo_coord1 ).
    " Draw -1, 0, +1 labels on Y axis
    CREATE OBJECT lo_coord1 EXPORTING iv_x = 2 iv_y = 5.
    draw_text( iv_text = '+1' io_coord = lo_coord1 ).
    CREATE OBJECT lo_coord1 EXPORTING iv_x = 2 iv_y = mv_height / 2 - ( mv_font_height / 2 ).
    draw_text( iv_text = '0' io_coord = lo_coord1 ).
    CREATE OBJECT lo_coord1 EXPORTING iv_x = 2 iv_y = mv_height - mv_font_height - 5.
    draw_text( iv_text = '-1' io_coord = lo_coord1 ).

    CREATE OBJECT lo_coord1 EXPORTING iv_x = 80 iv_y = mv_height - mv_font_height - 5.
    DATA lv_string_date TYPE char50.
    WRITE sy-datum TO lv_string_date.
    draw_text( iv_text = |Сегодня: { lv_string_date }| io_coord = lo_coord1 ).


    " Draw X axis (horizontal, center)
    CREATE OBJECT lo_coord1 EXPORTING iv_x = 0 iv_y = mv_height / 2.
    CREATE OBJECT lo_coord2 EXPORTING iv_x = mv_width - 1 iv_y = mv_height / 2.
    draw_line( io_color = lo_color io_coord_start = lo_coord1 io_coord_end = lo_coord2 ).
    " Draw Y axis (vertical, left)
    CREATE OBJECT lo_coord1 EXPORTING iv_x = mv_width / 2 iv_y = 0.
    CREATE OBJECT lo_coord2 EXPORTING iv_x = mv_width / 2 iv_y = mv_height - 1.
    draw_line( io_color = lo_color io_coord_start = lo_coord1 io_coord_end = lo_coord2 ).


  ENDMETHOD.

  METHOD draw_sine.
    draw_axes( ).

    CONSTANTS kv_pi TYPE f VALUE '3.14159265359'.

    DATA: lo_color  TYPE REF TO zcl_bmp_color,
          lo_coord1 TYPE REF TO zcl_bmp_coord,
          lo_coord2 TYPE REF TO zcl_bmp_coord,
          lv_y      TYPE i,
          lv_prev_x TYPE i,
          lv_prev_y TYPE i,
          lv_t      TYPE f,
          lv_val    TYPE f,
          lv_ampl   TYPE f,
          lv_freq   TYPE f,
          lv_phase  TYPE f.
    CREATE OBJECT lo_color EXPORTING iv_r = 0 iv_g = 0 iv_b = 255.
    lv_ampl = iv_amplitude.
    lv_freq = iv_frequency.
    lv_phase = iv_phase.
    DO mv_width - 1 TIMES.
      DATA(lv_x) = sy-index.
      lv_t = ( lv_x * 2 * kv_pi ) / mv_width.
      lv_val = lv_ampl * sin( lv_freq * lv_t + lv_phase ).
      lv_y = mv_height / 2 - trunc( lv_val * ( mv_height / 2 - 20 ) ).
      IF lv_x > 0.
        CREATE OBJECT lo_coord2 EXPORTING iv_x = lv_x iv_y = mv_height - lv_y.
        draw_pixel( io_color = lo_color  io_coord = lo_coord2 ).
      ENDIF.
      lv_prev_x = lv_x.
      lv_prev_y = lv_y.
    ENDDO.
  ENDMETHOD.
ENDCLASS.