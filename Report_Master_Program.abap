*&---------------------------------------------------------------------*
*& Report zreport_test_case
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zreport_test_case.

*****ZSTRU_FINAL_ITAB*****

****************************************
************Include Classes*************
****************************************
INCLUDE zdata_declearation.
INCLUDE ziselection_screen.
INCLUDE zcumulate_litem.
***************************************
********End Of Include Classes**********
****************************************
AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_validation.

START-OF-SELECTION.
  PERFORM get_data.
  PERFORM data_into_itabs.
  perform Display_Output.
