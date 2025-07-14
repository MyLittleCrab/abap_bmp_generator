REPORT zdownload_sine_bmp.

DATA: lo_bmp         TYPE REF TO zcl_bmp,
      lo_sine_graph  TYPE REF TO zcl_bmp_sine_graph,
      lv_xstring     TYPE xstring,
      lt_bin         TYPE STANDARD TABLE OF x255,
      lv_filename    TYPE string,
      lv_fullpath    TYPE string,
      lv_width       TYPE i VALUE 800,
      lv_height      TYPE i VALUE 400.

" 1. Create BMP object
CREATE OBJECT lo_bmp
  EXPORTING
    iv_width  = lv_width
    iv_height = lv_height.

" 2. Create sine graph object
CREATE OBJECT lo_sine_graph
  EXPORTING
    iv_width  = lv_width
    iv_height = lv_height.

" 3. Draw axes and sine wave
lo_sine_graph->draw_axes( io_bmp = lo_bmp ).
lo_sine_graph->draw_sine( io_bmp = lo_bmp ).

" 4. Get BMP as xstring
lv_xstring = lo_bmp->get_xstring( ).

" 5. Convert xstring to binary table for download
CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
  EXPORTING
    buffer        = lv_xstring
  TABLES
    binary_tab    = lt_bin.

" 6. Ask user for file path
lv_filename = 'sine_graph.bmp'.
CALL METHOD cl_gui_frontend_services=>file_save_dialog
  EXPORTING
    default_extension = 'bmp'
    default_file_name = lv_filename
  CHANGING
    filename          = lv_filename
    fullpath          = lv_fullpath.

IF sy-subrc = 0 AND lv_fullpath IS NOT INITIAL.
  " 7. Download file to client
  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      bin_filesize = xstrlen( lv_xstring )
      filename     = lv_fullpath
      filetype     = 'BIN'
    CHANGING
      data_tab     = lt_bin.
  MESSAGE |File saved to { lv_fullpath }| TYPE 'S'.
ELSE.
  MESSAGE 'File save cancelled.' TYPE 'I'.
ENDIF.