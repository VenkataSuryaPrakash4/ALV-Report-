*&---------------------------------------------------------------------*
*& Include          ZCUMULATE_LITEM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form Screen_Validation
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM screen_validation .
  LOOP AT SCREEN INTO DATA(gwa_screen).
    IF gwa_screen-group1 EQ 'BRC' AND
       rb_regr EQ abap_true.
      gwa_screen-active = 0.
    ENDIF.
    MODIFY SCREEN FROM gwa_screen.
    CLEAR : gwa_screen.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form Get_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data.

  SELECT rbukrs,gjahr,belnr,docln,tsl,budat,kunnr
        FROM acdoca
        INTO TABLE @gt_acdoca
        WHERE rldnr = '0L' AND
              rbukrs = @p_ccode AND
              kunnr IN @s_cnum AND
              koart = 'D'.
   if sy-subrc = 0.

    PERFORM process_data TABLES gt_acdoca.
   endif.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form Process_Data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*

FORM process_data TABLES gt_acdoca.
  LOOP AT gt_acdoca INTO gwa_acdoca.
    gv_kunnr = gwa_acdoca-kunnr.
    gv_bukrs = gwa_acdoca-rbukrs.
    gv_budat = gwa_acdoca-budat.

    CALL FUNCTION 'BAPI_AR_ACC_GETOPENITEMS'
      EXPORTING
        companycode = gv_bukrs
        customer    = gv_kunnr
        keydate     = gv_budat
        noteditems  = 'X'
        secindex    = 'X'
      TABLES
        lineitems   = gt_opencust.

    APPEND LINES OF gt_opencust TO gt_opencust_temp.
    CLEAR: gwa_acdoca,
           gv_bukrs,
           gv_kunnr,
           gv_budat.
  ENDLOOP.
*ENDIF.

IF gt_opencust_temp IS NOT INITIAL.
  LOOP AT gt_opencust_temp INTO DATA(gwa_opencust_temp).
    IF gwa_opencust_temp-bill_doc IS NOT INITIAL.
      APPEND gwa_opencust_temp TO gt_items.
    ENDIF.
    CLEAR : gwa_opencust_temp.
  ENDLOOP.
ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form Data_Into_Itabs
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM data_into_itabs .
**********************************************************
***************Cumulating Line Item Amount(4)****************
**********************************************************
  gt_items_aggr = gt_items.
  SORT gt_items_aggr BY comp_code customer fisc_year doc_no doc_type.
  LOOP AT gt_items_aggr INTO DATA(gwa_items_aggr).
    DATA(gwa_clt) = gwa_items_aggr.
    COLLECT gwa_clt INTO gt_clt.

    CLEAR:gwa_items_aggr.
    "gwa_clt.
  ENDLOOP.

**********************************************************
*****************Fetching data from VBRK(5)******************
**********************************************************
  SELECT vbeln,fkart,fkdat
    INTO TABLE @DATA(gt_invoice)
    FROM vbrk
    FOR ALL ENTRIES IN @gt_items
    WHERE vbeln = @gt_items-bill_doc.
**********************************************************
***************Fetching Invoice line Items(6)****************
**********************************************************
    SELECT vbeln,aubel,matkl,matnr,vkbur,ean11
      INTO TABLE @DATA(gt_invoicelt)
      FROM vbrp
      FOR ALL ENTRIES IN @gt_invoice
      WHERE vbeln = @gt_invoice-vbeln.
**********************************************************
*****************Fetching Material Text(7)*******************
**********************************************************
      SELECT matnr,maktx
        INTO TABLE @DATA(gt_matdis)
        FROM makt
        FOR ALL ENTRIES IN @gt_invoicelt
        WHERE matnr = @gt_invoicelt-matnr
        AND spras = 'E'.
**********************************************************
***************Fetching FI Document Item(8)***************
**********************************************************
        SELECT rldnr,rbukrs,gjahr,belnr,docln,segment,rebzg,rebzj
          INTO TABLE @DATA(gt_fidocit)
          FROM acdoca
          FOR ALL ENTRIES IN @gt_opencust
          WHERE rbukrs = @gt_opencust-comp_code
          AND gjahr = @gt_opencust-fisc_year
          AND belnr = @gt_opencust-doc_no.
*AND rldnr = 'OL'.
**********************************************************
**************Fetching Segment Table Data(9)**************
**********************************************************
          SELECT langu,segment,name
            INTO TABLE @DATA(gt_segment)
            FROM fagl_segmt
            FOR ALL ENTRIES IN @gt_fidocit
            WHERE langu = 'E'
            AND segment = @gt_fidocit-segment.
**********************************************************
***************Fetching BSID Table Data(10)***************
**********************************************************
            SELECT *
              INTO TABLE @gt_bsid
              FROM bsid
             FOR ALL ENTRIES IN @gt_fidocit
            WHERE rebzg = @gt_fidocit-rebzg
              AND rebzj = @gt_fidocit-rebzj.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form Display_Output
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_output .
************************************************************
***********Display Open line items Cust data(11-a)**********
************************************************************
*data : gwa_opencust_temp
  DELETE gt_opencust_temp WHERE  lc_amount = 0.
  DELETE gt_clt WHERE lc_amount = 0.
  SORT gt_opencust_temp BY customer fisc_year doc_no.

  LOOP AT gt_opencust_temp INTO Data(gwa_opencust_temp).
    ON CHANGE OF gwa_opencust_temp-customer
              OR gwa_opencust_temp-fisc_year
              OR gwa_opencust_temp-doc_no.
      gwa_final_itab-kunnr = gwa_opencust_temp-customer.
      gwa_final_itab-bukrs = gwa_opencust_temp-comp_code.
      gwa_final_itab-gjahr = gwa_opencust_temp-fisc_year.
      gwa_final_itab-belnr = gwa_opencust_temp-doc_no.
      gwa_final_itab-budat = gwa_opencust_temp-pstng_date.
      READ TABLE gt_clt INTO Data(gwa_clt) WITH KEY customer = gwa_opencust_temp-customer
                                              comp_code = gwa_opencust_temp-comp_code
                                              fisc_year = gwa_opencust_temp-fisc_year
                                              doc_no = gwa_opencust_temp-doc_no.
      gwa_final_itab-dmbtr = gwa_clt-lc_amount.
      gwa_final_itab-ref_key = gwa_opencust_temp-ref_key_1.
      gwa_final_itab-umskz = gwa_opencust_temp-sp_gl_ind.
      gwa_final_itab-currency = gwa_opencust_temp-currency.
      gwa_final_itab-zterm = gwa_opencust_temp-pmnttrms.
      READ TABLE gt_fidocit INTO DATA(gwa_fidocit) WITH KEY rbukrs = gwa_opencust_temp-comp_code
                                                            belnr = gwa_opencust_temp-doc_no
                                                            gjahr = gwa_opencust_temp-fisc_year.

      gwa_final_itab-segment = gwa_fidocit-segment.
      READ TABLE gt_segment INTO DATA(gwa_segment) WITH KEY segment = gwa_fidocit-segment.
      gwa_final_itab-name = gwa_segment-name.
      READ TABLE gt_invoice INTO DATA(gwa_invoice) WITH KEY vbeln = gwa_opencust_temp-bill_doc.
      gwa_final_itab-vbeln = gwa_invoice-vbeln.
      gwa_final_itab-fkart = gwa_invoice-fkart.
      gwa_final_itab-fkdat = gwa_invoice-fkdat.
      READ TABLE gt_invoicelt INTO DATA(gwa_invoicelt) WITH KEY vbeln = gwa_invoice-vbeln.
      gwa_final_itab-aubel = gwa_invoicelt-aubel.
      gwa_final_itab-matkl = gwa_invoicelt-matkl.
      gwa_final_itab-ean11 = gwa_invoicelt-ean11.
      gwa_final_itab-matnr = gwa_invoicelt-matnr.
      gwa_final_itab-vkbur = gwa_invoicelt-vkbur.
      READ TABLE gt_matdis INTO DATA(gwa_matdis) WITH KEY matnr = gwa_invoicelt-matnr.
      gwa_final_itab-maktx = gwa_matdis-maktx.
      "Calling function Import Params.
*           lv_koart TYPE rfpos-koart.
      gv_zfbdt = gwa_opencust_temp-bline_date.
      gv_zbd1t = gwa_opencust_temp-dsct_days1.
      gv_zbd2t = gwa_opencust_temp-dsct_days2.
      gv_zbd3t = gwa_opencust_temp-netterms.
      gv_shkzg = gwa_opencust_temp-db_cr_ind.
      gv_rebzg = gwa_opencust_temp-inv_ref.
      CALL FUNCTION 'NET_DUE_DATE_GET'
        EXPORTING
          i_zfbdt = gv_zfbdt
          i_zbd1t = gv_zbd1t
          i_zbd2t = gv_zbd2t
          i_zbd3t = gv_zbd3t
          i_shkzg = gv_shkzg
          i_rebzg = gv_rebzg
          i_koart = 'D'
        IMPORTING
          e_faedt = gv_faedt.
      IF gv_faedt IS NOT INITIAL AND p_kdate IS NOT INITIAL.
        DATA(date) = p_kdate - gv_faedt.
      ELSEIF gv_faedt IS NOT INITIAL AND p_kdate IS INITIAL.
        date = sy-datum - gv_faedt.
      ELSEIF gv_faedt IS INITIAL AND p_kdate IS NOT INITIAL.
        date = p_kdate - sy-datum .
      ELSE.
        date = sy-datum - gwa_opencust_temp-pstng_date.
      ENDIF.

      IF date LT 0.
        date = date * gv_value.
      ENDIF.
******************************************************
      IF gwa_clt-lc_amount NE 0.
        IF date LE p_brack1.
          DATA(lv_ind1) = 'X'.
          gwa_final_itab-days_1 = gwa_clt-lc_amount.
          CONCATENATE p_brack1 TEXT-005 INTO DATA(lv_concat1) SEPARATED BY '-'.
        ELSEIF ( date GT p_brack1 ) AND ( date LE p_brack2 ).
          DATA(lv_ind2) = 'X'.
          gwa_final_itab-days_2 = gwa_clt-lc_amount.
          CONCATENATE p_brack2 TEXT-005 INTO DATA(lv_concat2) SEPARATED BY '-'.
        ELSEIF ( date GT p_brack2 ) AND ( date LE p_brack3 ).
          DATA(lv_ind3) = 'X'.
          gwa_final_itab-days_3 = gwa_clt-lc_amount.
          CONCATENATE p_brack3 TEXT-005 INTO DATA(lv_concat3) SEPARATED BY '-'.
        ELSE.
          DATA(lv_ind4) = 'X'.
          gwa_final_itab-days_4 = gwa_clt-lc_amount.
          CONCATENATE gv_lastcol p_brack3 TEXT-005 INTO DATA(lv_concat4).
        ENDIF.
      ENDIF.

*    IF gwa_final_itab-dmbtr IS NOT INITIAL.
      APPEND gwa_final_itab TO gt_dummy.
*    ENDIF.
    ENDON.
    CLEAR : gwa_opencust_temp,gwa_final_itab,gwa_clt,gwa_fidocit,
            gwa_segment,gwa_invoice,
            gwa_invoicelt,
            gwa_matdis,
            gv_zfbdt,
            gv_zbd1t,
            gv_zbd2t,
            gv_zbd3t,
            gv_shkzg,
            gv_rebzg.

  ENDLOOP.

*************************************************
****************ALV Standard FM******************
*************************************************
  IF rb_regr EQ abap_true.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program = sy-repid
        i_structure_name   = 'ZSTRU_FINAL_ITAB'
        i_grid_title       = 'Open Items'
      TABLES
        t_outtab           = gt_dummy
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.
  ELSE.
    gwa_layout-zebra = 'X'.

    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'KUNNR' seltext_s = 'Customer_Number' ) TO gt_fcat.
    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'BUKRS' seltext_s = 'Company_Code' ) TO gt_fcat.
    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'BELNR' seltext_s = 'Doc_Num' ) TO gt_fcat.
    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'BUDAT' seltext_s = 'Posting_Date' ) TO gt_fcat.
    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'GJAHR' seltext_s = 'Fiscal_year' ) TO gt_fcat.
    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'REF_KEY' seltext_s = 'Reference_Key' ) TO gt_fcat.
    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'UMSKZ' seltext_s = 'G/L' ) TO gt_fcat.
    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'DMBTR' seltext_s = 'Amount' ) TO gt_fcat.
    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'CURRENCY' seltext_s = 'Currency' ) TO gt_fcat.
    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'ZTERM' seltext_s = 'Payment_Term' ) TO gt_fcat.
    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'SEGMENT' seltext_s = 'Segment' ) TO gt_fcat.
    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'NAME' seltext_s = 'Text' ) TO gt_fcat.
    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'VBELN' seltext_s = 'Billing_Doc' ) TO gt_fcat.
    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'FKART' seltext_s = 'Billing_Type' ) TO gt_fcat.
    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'FKDAT' seltext_s = 'Billing_Date' ) TO gt_fcat.
    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'AUBEL' seltext_s = 'Goods_Issue' ) TO gt_fcat.
    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'MATNR' seltext_s = 'Material_Num' ) TO gt_fcat.
    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'MAKTX' seltext_s = 'Material_Description' ) TO gt_fcat.
    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'MATKL' seltext_s = 'Material_Group' ) TO gt_fcat.
    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'EAN11' seltext_s = 'EAN' ) TO gt_fcat.
    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'VKBUR' seltext_s = 'Sales_Office' ) TO gt_fcat.

    ADD 1 TO gv_count.
    IF lv_ind1 EQ 'X'.
      APPEND VALUE #( col_pos = gv_count fieldname = 'DAYS_1' do_sum = 'X' seltext_m = lv_concat1 ) TO gt_fcat.
    ELSE.
      APPEND VALUE #( col_pos = gv_count no_out = 'X' fieldname = 'DAYS_1' do_sum = 'X' seltext_m = lv_concat1 ) TO gt_fcat.
    ENDIF.

    ADD 1 TO gv_count.
    IF lv_ind2 EQ 'X'.
      APPEND VALUE #( col_pos = gv_count fieldname = 'DAYS_2' do_sum = 'X' seltext_m = lv_concat2 ) TO gt_fcat.
    ELSE.
      APPEND VALUE #( col_pos = gv_count no_out = 'X' fieldname = 'DAYS_2' do_sum = 'X' seltext_m = lv_concat2 ) TO gt_fcat.
    ENDIF.

    ADD 1 TO gv_count.
    IF lv_ind3 EQ 'X'.
      APPEND VALUE #( col_pos = gv_count fieldname = 'DAYS_3' do_sum = 'X' seltext_m = lv_concat3 ) TO gt_fcat.
    ELSE.
      APPEND VALUE #( col_pos = gv_count no_out = 'X' fieldname = 'DAYS_3' do_sum = 'X' seltext_m = lv_concat3 ) TO gt_fcat.
    ENDIF.

    ADD 1 TO gv_count.
    IF lv_ind4 EQ 'X'.
      APPEND VALUE #( col_pos = gv_count fieldname = 'DAYS_4' do_sum = 'X' seltext_m = lv_concat4 ) TO gt_fcat.
    ELSE.
      APPEND VALUE #( col_pos = gv_count  no_out = 'X' fieldname = 'DAYS_4' do_sum = 'X' seltext_m = lv_concat4 ) TO gt_fcat.
    ENDIF.


    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program = sy-repid
        is_layout          = gwa_layout
        it_fieldcat        = gt_fcat
      TABLES
        t_outtab           = gt_dummy
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.

  ENDIF.


  IF sy-subrc = 0.

  ENDIF.
ENDFORM.
