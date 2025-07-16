REPORT zdownload_sine_bmp.

DATA:
  lv_width    TYPE i VALUE 600,
  lv_height   TYPE i VALUE 600.



DATA(lo_graph) = NEW zcl_bmp_sine_graph( iv_width = lv_width iv_height = lv_height ).
lo_graph->draw_sine( ).
lo_graph->download_in_sapgui( ).
