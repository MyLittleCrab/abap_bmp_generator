REPORT zshow_sine_bmp.

DATA: lv_width  TYPE i VALUE 600,
      lv_height TYPE i VALUE 600.

DATA: lo_container TYPE REF TO cl_gui_custom_container.

START-OF-SELECTION.


  DATA(lo_graph) = NEW zcl_bmp_sine_graph( iv_width = lv_width iv_height = lv_height ).
  lo_graph->draw_sine( ).

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
    lo_container = NEW cl_gui_custom_container( 'PICTURE_AREA' ).
    lo_graph->show_in_sapgui_container( lo_container ).
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
