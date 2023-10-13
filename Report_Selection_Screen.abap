*&---------------------------------------------------------------------*
*& Include ziselection_screen
*&---------------------------------------------------------------------*
TABLES acdoca.

SELECTION-SCREEN BEGIN OF BLOCK blk_1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS s_cnum FOR acdoca-kunnr.
  PARAMETERS : p_ccode TYPE acdoca-rbukrs,
               p_kdate TYPE acdoca-budat.
SELECTION-SCREEN END OF BLOCK blk_1.

SELECTION-SCREEN BEGIN OF BLOCK blk_2 WITH FRAME TITLE TEXT-002.
  PARAMETERS : rb_regr RADIOBUTTON GROUP rb_1 USER-COMMAND ucom,
               rb_ager RADIOBUTTON GROUP rb_1 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK blk_2.

SELECTION-SCREEN BEGIN OF BLOCK blk_3 WITH FRAME TITLE TEXT-003.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(10) TEXT-004 MODIF ID brc.
    PARAMETERS : p_brack1 TYPE n LENGTH 3 MODIF ID brc.
    SELECTION-SCREEN POSITION 18.
    PARAMETERS : p_brack2 TYPE n LENGTH 3 MODIF ID brc.
    SELECTION-SCREEN POSITION 24.
    PARAMETERS : p_brack3 TYPE n LENGTH 3 MODIF ID brc.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blk_3.
