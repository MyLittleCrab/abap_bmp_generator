CLASS zcl_bmp_coord DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_x TYPE i iv_y TYPE i,
      get_x RETURNING VALUE(rv_x) TYPE i,
      get_y RETURNING VALUE(rv_y) TYPE i.
  PRIVATE SECTION.
    DATA: mv_x TYPE i,
          mv_y TYPE i.
ENDCLASS.

CLASS zcl_bmp_coord IMPLEMENTATION.
  METHOD constructor.
    mv_x = iv_x.
    mv_y = iv_y.
  ENDMETHOD.
  METHOD get_x.
    rv_x = mv_x.
  ENDMETHOD.
  METHOD get_y.
    rv_y = mv_y.
  ENDMETHOD.
ENDCLASS.
