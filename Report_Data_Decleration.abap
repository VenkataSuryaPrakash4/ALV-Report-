*&---------------------------------------------------------------------*
*& Include          ZDATA_DECLEARATION
*&---------------------------------------------------------------------*

   TYPES : BEGIN OF ty_acdoca,
             rbukrs TYPE bukrs,
             gjahr  TYPE gjahr,
             belnr  TYPE belnr_d,
             docln  TYPE docln6,
             tsl    TYPE fins_vtcur12,
             budat  TYPE budat,
             kunnr  TYPE kunnr,
           END OF ty_acdoca,

           BEGIN OF ty_invoice,
             vbeln TYPE vbeln_vf,
             fkart TYPE fkart,
             fkdat TYPE fkdat,
           END OF ty_invoice,

           BEGIN OF ty_invoicelt,
             vbeln TYPE vbeln_vf,
             aubel TYPE vbeln_va,
             matkl TYPE matkl,
             matnr TYPE matnr,
             vkbur TYPE vkbur,
             ean11 TYPE ean11,
           END OF ty_invoicelt,

           BEGIN OF ty_segment,
             langu   TYPE spras,
             segment TYPE fb_segment,
             name    TYPE text50,
           END OF ty_segment,

           BEGIN OF ty_matdis,
             matnr TYPE matnr,
             maktx TYPE maktx,
           END OF ty_matdis,

           BEGIN OF ty_fidocit,
             rldnr   TYPE fins_ledger,
             rbukrs  TYPE bukrs,
             gjahr   TYPE gjahr,
             belnr   TYPE belnr_d,
             docln   TYPE docln6,
             segment TYPE fb_segment,
             rebzg   TYPE rebzg,
             rebzj   TYPE rebzj,
           END OF ty_fidocit.

   DATA : gt_display       TYPE TABLE OF bsid,
          gt_acdoca        TYPE TABLE OF ty_acdoca,
          gt_matdis        TYPE TABLE OF ty_matdis,
          gt_final_itab    TYPE TABLE OF zstru_final_itab,
          gt_fidocit       TYPE TABLE OF ty_fidocit,
          gt_segment       TYPE TABLE OF ty_segment,
          gt_invoice       TYPE TABLE OF ty_invoice,
          gt_invoicelt     TYPE TABLE OF ty_invoicelt,
          gt_opencust_temp TYPE TABLE OF bapi3007_2,
          gt_items         TYPE TABLE OF bapi3007_2,
          gt_opencust      TYPE TABLE OF bapi3007_2,
          gt_items_aggr    TYPE TABLE OF bapi3007_2,
          gt_clt           TYPE TABLE OF bapi3007_2,
          gt_dummy         TYPE TABLE OF zstru_final_itab,
          gt_bsid          TYPE TABLE OF bsid,
          gwa_segment      TYPE ty_segment,
          gwa_invoice      TYPE ty_invoicelt,
          gwa_fidocit      TYPE ty_fidocit,
          gwa_matdis       TYPE ty_matdis,
          gwa_invoicelt    TYPE ty_invoicelt,
          gwa_acdoca       TYPE ty_acdoca,
          gwa_final_itab   TYPE zstru_final_itab,
          gwa_screen       TYPE screen,
          gwa_layout       TYPE slis_layout_alv,
          gt_fcat          TYPE slis_t_fieldcat_alv,
          gv_kunnr         TYPE bapi3007_1-customer,
          gv_bukrs         TYPE bapi3007_1-comp_code,
          gv_budat         TYPE bapi3007-key_date,
          gv_zfbdt         TYPE bsid-zfbdt,
          gv_zbd1t         TYPE bsid-zbd1t,
          gv_zbd2t         TYPE bsid-zbd2t,
          gv_zbd3t         TYPE bsid-zbd3t,
          gv_shkzg         TYPE bsid-shkzg,
          gv_rebzg         TYPE bsid-rebzg,
          gv_faedt         TYPE rfpos-faedt,
          gv_count         TYPE n LENGTH 2.

   CONSTANTS : gv_value   TYPE int8 VALUE -1,
               gv_lastcol TYPE c VALUE '>'.
