FUNCTION Z_BMP_SMW0_DOWNLOADER
  IMPORTING
    IV_FILE_NAME TYPE W3OBJID
  EXPORTING
    EV_FILE_CONTENT TYPE XSTRING.

  DATA: mime      TYPE STANDARD TABLE OF w3mime.
  DATA lv_bytelen TYPE i.
  DATA lv_lines   TYPE i.
  DATA lv_value_len TYPE wwwparams-value.
  IMPORT mime FROM DATABASE wwwdata(mi) ID iv_file_name.
  DESCRIBE TABLE mime[] LINES lv_lines.

  CALL FUNCTION 'WWWPARAMS_READ'
    EXPORTING
      relid  = 'MI'
      objid  = iv_file_name
      name   = 'filesize'
    IMPORTING
      value  = lv_value_len
    EXCEPTIONS
      OTHERS = 1.

  lv_bytelen = lv_value_len.

  CALL METHOD cl_rswad_convert=>w3mime_to_xstring(
    EXPORTING
      i_bytelen = lv_bytelen
      i_t_table = mime[]
    IMPORTING
      e_xstring = ev_file_content ).

ENDFUNCTION.