REPORT zshow_sine_bmp.

DATA: lo_bmp        TYPE REF TO zcl_bmp,
      lo_sine_graph TYPE REF TO zcl_bmp_sine_graph,
      lv_xstring    TYPE xstring,
      lv_b64        TYPE string,
      lv_html       TYPE string,
      lv_width      TYPE i VALUE 600,
      lv_height     TYPE i VALUE 600.

DATA: lo_container TYPE REF TO cl_gui_custom_container,
      lo_html      TYPE REF TO cl_gui_html_viewer.

START-OF-SELECTION.
  " 1. Create BMP and sine graph objects
  CREATE OBJECT lo_bmp EXPORTING iv_width = lv_width iv_height = lv_height.
  CREATE OBJECT lo_sine_graph EXPORTING iv_width = lv_width iv_height = lv_height.

  " 2. Draw axes and sine wave
  lo_sine_graph->draw_axes( io_bmp = lo_bmp ).
  lo_sine_graph->draw_sine( io_bmp = lo_bmp ).

  " 3. Get BMP as xstring
  lv_xstring = lo_bmp->get_xstring( ).

  " 4. Convert xstring to base64
  CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
    EXPORTING
      input  = lv_xstring
    IMPORTING
      output = lv_b64.

  " 5. Prepare HTML with embedded image
  lv_html = |<html><body><img src="data:image/bmp;base64,{ lv_b64 }" /></body></html>|.

  " 6. Create container and HTML viewer
  CALL SCREEN 100.



*----------------------------------------------------------------------*
*  Screen 100 definition (pseudo-code, must be created in SE51):       *
*  - Custom control with name 'PICTURE_AREA'                          *
*  - OK code field (e.g., sy-ucomm)                                   *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  IF lo_container IS INITIAL.
    CREATE OBJECT lo_container
      EXPORTING
        container_name = 'PICTURE_AREA'.
    CREATE OBJECT lo_html
      EXPORTING
        parent = lo_container.

    DATA lt_html_data TYPE w3_htmltab.

    CALL FUNCTION 'SCMS_STRING_TO_FTEXT'
      EXPORTING
        text      = lv_html
      TABLES
        ftext_tab = lt_html_data.

    DATA lv_url TYPE char255.
    lo_html->load_data(
        IMPORTING assigned_url = lv_url
        CHANGING data_table = lt_html_data ).

    lo_html->show_url( url = lv_url ).
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT' OR 'BACK' OR 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.