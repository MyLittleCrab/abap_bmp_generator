CLASS zcl_image_file DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      get_xstring ABSTRACT
        RETURNING VALUE(rv_xstring) TYPE xstring ,
      import_from_xstring ABSTRACT
        IMPORTING
          iv_xstring TYPE xstring,
      import_from_smw0
        IMPORTING
          iv_object TYPE w3objid,
      get_base64
        RETURNING VALUE(rv_b64string) TYPE string,
      get_base64_web_link
        RETURNING VALUE(rv_b64web_link) TYPE string,
      show_in_sapgui_container
        IMPORTING io_container TYPE REF TO cl_gui_custom_container,
      download_in_sapgui
        IMPORTING
          VALUE(iv_filename) TYPE string DEFAULT 'graph.bmp'.

  PROTECTED SECTION.
    METHODS: load_xstring_from_smw0
      IMPORTING iv_file_name           TYPE w3objid
      RETURNING VALUE(rv_file_content) TYPE xstring.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_image_file IMPLEMENTATION.

  METHOD import_from_smw0.
    DATA lv_xstring TYPE xstring.
    lv_xstring = load_xstring_from_smw0( iv_file_name = iv_object ).
    IF lv_xstring IS INITIAL.
      RETURN.
    ENDIF.
    import_from_xstring( iv_xstring = lv_xstring ).
  ENDMETHOD.

  METHOD load_xstring_from_smw0.
    CALL FUNCTION 'Z_BMP_SMW0_DOWNLOADER'
      EXPORTING
        iv_file_name    = iv_file_name
      IMPORTING
        ev_file_content = rv_file_content.
  ENDMETHOD.

  METHOD get_base64.
    CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
      EXPORTING
        input  = me->get_xstring( )
      IMPORTING
        output = rv_b64string.
  ENDMETHOD.

  METHOD get_base64_web_link.
    rv_b64web_link = |data:image/bmp;base64,{ me->get_base64( ) }|.
  ENDMETHOD.

  METHOD show_in_sapgui_container.
    DATA(lo_html_viewer) = NEW cl_gui_html_viewer( io_container ).

    DATA(lv_html) = |<html><body><img src="{ get_base64_web_link( ) }" /></body></html>|.
    DATA lt_html_data TYPE w3_htmltab.

    CALL FUNCTION 'SCMS_STRING_TO_FTEXT'
      EXPORTING
        text      = lv_html
      TABLES
        ftext_tab = lt_html_data.

    DATA lv_url TYPE char255.
    lo_html_viewer->load_data(
        IMPORTING assigned_url = lv_url
        CHANGING data_table = lt_html_data ).

    lo_html_viewer->show_url( url = lv_url ).
  ENDMETHOD.

  METHOD download_in_sapgui.
    DATA(lv_xstring) = get_xstring( ).
    DATA:
          lt_bin        TYPE STANDARD TABLE OF x255.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = lv_xstring
      TABLES
        binary_tab = lt_bin.

    DATA: lv_path     TYPE string,
          lv_fullpath TYPE string.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        initial_directory = 'C:\'
        default_extension = 'bmp'
        default_file_name = iv_filename
      CHANGING
        filename          = iv_filename
        fullpath          = lv_fullpath
        path              = lv_path.


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
  ENDMETHOD.

ENDCLASS.