REPORT zcclm_find_referenced_sub.

TABLES: ags_cc_attrib, agsccl_attr_meta, agsccl_object.

SELECTION-SCREEN BEGIN OF BLOCK 0 WITH FRAME.
SELECTION-SCREEN COMMENT /1(50) comm1.
SELECTION-SCREEN COMMENT /1(79) comm2.
SELECTION-SCREEN COMMENT /1(79) comm3.
SELECTION-SCREEN END OF BLOCK 0.

SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: p_objnam FOR  agsccl_object-obj_name  DEFAULT 'ZDMSH_MIG_BP_REQUEST' NO INTERVALS.
PARAMETERS:     p_objtyp TYPE agsccl_object-obj_type  DEFAULT 'PROG'.
SELECTION-SCREEN END OF BLOCK 1.

SELECTION-SCREEN BEGIN OF BLOCK 2 WITH FRAME TITLE text-002.
PARAMETERS: p_rtype TYPE agsccl_object-obj_type DEFAULT 'FUNC'.
SELECTION-SCREEN END OF BLOCK 2.


SELECTION-SCREEN BEGIN OF BLOCK 3 WITH FRAME TITLE text-003.
PARAMETERS:     p_system TYPE agsccl_object-system_id DEFAULT 'CRH'.
SELECTION-SCREEN END OF BLOCK 3.

SELECTION-SCREEN BEGIN OF BLOCK 4 WITH FRAME TITLE text-004.
SELECT-OPTIONS  p_mattr FOR agsccl_attr_meta-attrib_name  NO-EXTENSION NO INTERVALS DEFAULT 'SAPLIFECYCLESTATUS'.
PARAMETERS:     p_mval(15) TYPE c DEFAULT '40'.
SELECTION-SCREEN END OF BLOCK 4.

SELECTION-SCREEN BEGIN OF BLOCK 5 WITH FRAME TITLE text-005.
SELECT-OPTIONS  p_fattr FOR agsccl_attr_meta-attrib_name  NO-EXTENSION NO INTERVALS DEFAULT 'ZDELSUBOBJ'.
PARAMETERS:     p_fval(15) TYPE c DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK 5.

TYPE-POOLS: rs, rsdrc.
DATA: p_mtype(4) TYPE c.




TYPES:
  BEGIN OF gt_s_data,
    "0smd_lsid(8)        TYPE c,
    0sm_cclcnam(40) TYPE c, "main object name
    0sm_cclctyp(4)  TYPE c, "main object type
    0sm_cclcobj(60) TYPE c, "caller object name
    0sm_cclcty(4)   TYPE c, "caller object type
    0sm_cclobj(60)  TYPE c, "referenced object name
    0sm_ccltyp(4)   TYPE c, "referenced object type
  END OF gt_s_data.

DATA:
* G_S_DATA = a working area to hold one record as returned by a
*            query one the infocube; the structure /BI*/V<Infocube>2
*            has a column for every characteristic, navigational
*            attribute and key figure of the related infocube. Please
*            note that the query might only fill a few of those columns
*            that have been requested within the query.
  g_s_data  TYPE gt_s_data,
* G_T_DATA = an internal table that can hold the result set
  g_t_data  TYPE STANDARD TABLE OF gt_s_data WITH DEFAULT KEY INITIAL SIZE 10,
* G_S_SFC  = description of a characteristic or navigational attribute that is requested by a query
  g_s_sfc   TYPE rsdri_s_sfc,
  g_th_sfc  TYPE rsdri_th_sfc,
* G_S_SFK  = description of a key figure that is requested by a query
  g_s_sfk   TYPE rsdri_s_sfk,
  g_th_sfk  TYPE rsdri_th_sfk,
* G_S_RANGE = description of a restriction on a characteristic or navigational attribute
  g_s_range TYPE rsdri_s_range,
  g_t_range TYPE rsdri_t_range.
FIELD-SYMBOLS: <fs_t_data> LIKE LINE OF g_t_data.
DATA: g_end_of_data TYPE rs_bool,
      g_first_call  TYPE rs_bool.
* --- this variable will be set to TRUE when the last data package is read
g_end_of_data = rs_c_false.
* --- this variable indicates whether this is an initial call to the reading module or a follow-up call (which simply retrieves already selected data)
g_first_call  = rs_c_true.
DATA: BEGIN OF ls_objectlist,
        pgmid TYPE agsccl_object-pgmid,
        type  TYPE agsccl_object-obj_type,
        name  TYPE agsccl_object-obj_name,
      END OF ls_objectlist.
DATA: lt_objectlist_includes LIKE TABLE OF ls_objectlist.
DATA: lt_objectlist_marked LIKE TABLE OF ls_objectlist.
DATA: lt_objectlist_ref_ok LIKE TABLE OF ls_objectlist.
DATA: lt_objectlist_banned LIKE TABLE OF ls_objectlist.
DATA: lt_objectlist_nonzerolevel LIKE TABLE OF ls_objectlist.
FIELD-SYMBOLS: <fs_objectlist> LIKE LINE OF lt_objectlist_includes.
DATA: BEGIN OF ls_temp,
        obj_type TYPE agsccl_object-obj_type,
        obj_name TYPE agsccl_object-obj_name,
      END OF ls_temp.
DATA: lt_temp LIKE TABLE OF ls_temp.
FIELD-SYMBOLS: <fs_temp> LIKE LINE OF lt_temp.
DATA: sv_decis TYPE string.
DATA: sv_objtype TYPE string.
DATA: data_ref      TYPE REF TO data,
      lv_where(255) TYPE c,
      where_tab     LIKE TABLE OF lv_where.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_mattr-low.
  PERFORM get_attr CHANGING sv_decis.
  p_mattr-low = sv_decis.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_rtype.
  PERFORM get_obj_type CHANGING sv_objtype.
  p_rtype = sv_objtype.

AT SELECTION-SCREEN OUTPUT.
  comm1 = 'CCLM Dependency analyzer'.
  comm2 = 'This program finds the dependent sub-objects, which are called by main objects'.
  comm3 = 'of certain selection criterias'.

START-OF-SELECTION.
  MESSAGE 'Start first phase - search for Main objects' TYPE 'I'.
* Get objects from CCLM with selected attribute.

*  CREATE DATA data_ref TYPE agsccl_object.
*  ASSIGN data_ref->* TO FIELD-SYMBOL(<dynamic_structure>).
  CONCATENATE p_mattr-low ' =' ' '''  p_mval ''''  ' AND SYSTEM_ID = ''' p_system '''' INTO lv_where.
  IF p_objnam IS NOT INITIAL and p_objnam <> '*'.

    LOOP AT p_objnam ASSIGNING FIELD-SYMBOL(<fs_objnam>).
      CONCATENATE lv_where ' AND OBJ_NAME = ''' <fs_objnam>-low '''' INTO lv_where.
    ENDLOOP.

  ENDIF.

  IF p_objtyp IS NOT INITIAL.
    CONCATENATE lv_where ' AND OBJ_TYPE = ''' p_objtyp '''' INTO lv_where.
  ENDIF.
  APPEND lv_where TO where_tab.

  " First we search for Main objectsa matching selection criteria.
  SELECT pgmid obj_type obj_name FROM agsccl_object INTO TABLE lt_objectlist_marked WHERE (where_tab).

  SORT lt_objectlist_marked BY pgmid type name ASCENDING.
  DATA: lv_lines TYPE i.
  DESCRIBE TABLE lt_objectlist_marked LINES lv_lines.
  DATA: lv_msg TYPE string.
  DATA: lv_linesc(6) TYPE c.
  MOVE lv_lines TO lv_linesc.
  CONCATENATE 'Main objects fetched from DB with following selection criteria ' p_mattr-low ' =' ' ' p_mval ' :'   lv_linesc INTO lv_msg.
  MESSAGE lv_msg TYPE 'I'.

  CLEAR: lv_lines, lv_msg, lv_linesc.


  IF      p_rtype = 'INCL'. SELECT pgmid obj_type obj_name FROM agsccl_object INTO TABLE lt_objectlist_includes WHERE obj_type = 'PROG' AND system_id = p_system AND obj_name LIKE 'Z%' OR obj_name LIKE 'Y%' AND programtype = 'I'.
  ELSE.                     SELECT pgmid obj_type obj_name FROM agsccl_object INTO TABLE lt_objectlist_includes WHERE obj_type = p_rtype AND system_id = p_system AND ( obj_name LIKE 'Z%' OR obj_name LIKE 'Y%' ).
ENDIF.

*  SORT lt_temp  ASCENDING.
*  DELETE ADJACENT DUPLICATES FROM lt_temp.

*  DESCRIBE TABLE lt_objectlist_includes LINES lv_lines.
*  MOVE lv_lines TO lv_linesc.
*  CONCATENATE 'SubObjects fetched from DB: ' lv_linesc INTO lv_msg.
*  MESSAGE lv_msg TYPE 'I'.
*  clear: lv_msg,lv_lines,lv_linesc.
*
*  MESSAGE 'Now we keep only Z* and Y* objects' TYPE 'I'.
**Process the list -> split by PGMID, TYPE, NAME.
*  LOOP AT lt_temp ASSIGNING <fs_temp>.
*    SPLIT <fs_temp> AT '+' INTO ls_objectlist-pgmid ls_objectlist-type ls_objectlist-name.
*
*    IF ls_objectlist-name CP 'Z*'.
*      APPEND ls_objectlist TO lt_objectlist_includes.
*    ELSEIF ls_objectlist-name CP 'Y*'.
*      APPEND ls_objectlist TO lt_objectlist_includes.
*    ENDIF.
*  ENDLOOP.
*
*  REFRESH: lt_temp.

SORT lt_objectlist_includes BY pgmid type name ASCENDING.
DESCRIBE TABLE lt_objectlist_includes LINES lv_lines.
IF lv_lines = 0.
  MESSAGE 'No objects found matching criteria' TYPE 'S'.
  EXIT.
ENDIF.
MOVE lv_lines TO lv_linesc.
CONCATENATE 'SUB-level ZY Objects fetched from DB: ' lv_linesc INTO lv_msg.
MESSAGE lv_msg TYPE 'I'.

* prepare BW infrastructure.
** First - Find only sub objects from previosuly identified 70s+ main objects
"MESSAGE 'Start first phase - search for Referenced objects' TYPE 'I'.
"PERFORM search_refobjects_from_main.

* SECOND - find ALL main objects based on identified referenced.
MESSAGE 'Start second phase - search for MAINs of SUBs' TYPE 'I'.
PERFORM  search_allmain_from_refobjects.

*THIRD - now let's loop through ALL Main objects and find intersections with 70s.
MESSAGE 'Start third phase -  internal processing (loops)' TYPE 'I'.

DATA: lv_count TYPE i.
DATA: lv_main TYPE char255,
      lv_call TYPE char255.


LOOP AT g_t_data ASSIGNING <fs_t_data>.
  CONCATENATE <fs_t_data>-0sm_cclctyp <fs_t_data>-0sm_cclcnam INTO lv_main.
  CONCATENATE <fs_t_data>-0sm_cclcty  <fs_t_data>-0sm_cclcobj INTO lv_call.
  IF lv_main <> lv_call.
    MOVE <fs_t_data>-0sm_cclobj TO ls_objectlist-name.
    MOVE <fs_t_data>-0sm_ccltyp TO ls_objectlist-type.
    ls_objectlist-pgmid = 'R3TR'.
    APPEND ls_objectlist TO lt_objectlist_nonzerolevel. "  -> store non zero level references
    CLEAR: lv_main, lv_call.
    CONTINUE.
  ENDIF.

  READ TABLE lt_objectlist_banned WITH KEY pgmid = 'R3TR' type = <fs_t_data>-0sm_ccltyp name = <fs_t_data>-0sm_cclobj TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    ADD 1 TO lv_count.
    CONTINUE.
  ENDIF.

  READ TABLE lt_objectlist_marked WITH KEY
  pgmid = 'R3TR'
  type = <fs_t_data>-0sm_cclctyp
  name = <fs_t_data>-0sm_cclcnam TRANSPORTING NO FIELDS.

  IF sy-subrc <> 0. "main object is not '70'
    MOVE <fs_t_data>-0sm_cclobj TO ls_objectlist-name.
    MOVE <fs_t_data>-0sm_ccltyp TO ls_objectlist-type.
    ls_objectlist-pgmid = 'R3TR'.
    APPEND ls_objectlist TO lt_objectlist_banned. "  -> ban the referenced object
  ELSE.
    MOVE <fs_t_data>-0sm_cclobj TO ls_objectlist-name.
    MOVE <fs_t_data>-0sm_ccltyp TO ls_objectlist-type.
    ls_objectlist-pgmid = 'R3TR'.
    APPEND ls_objectlist TO lt_objectlist_ref_ok.
  ENDIF.
  CLEAR: ls_objectlist, lv_main, lv_call.
ENDLOOP.

CLEAR: lv_lines, lv_linesc, lv_msg.
SORT lt_objectlist_nonzerolevel BY type name.

MOVE lv_count TO lv_linesc.
CONCATENATE 'Times we skipped the banned object calls: ' lv_linesc INTO lv_msg IN CHARACTER MODE.
MESSAGE lv_msg TYPE 'I'.
CLEAR: lv_lines, lv_linesc, lv_msg.

DESCRIBE TABLE lt_objectlist_nonzerolevel LINES lv_lines.
MOVE lv_lines TO lv_linesc.
CONCATENATE 'Non-zero level references found: ' lv_linesc INTO lv_msg IN CHARACTER MODE.
MESSAGE lv_msg TYPE 'I'.
CLEAR: lv_lines, lv_linesc, lv_msg.

DELETE ADJACENT DUPLICATES FROM lt_objectlist_nonzerolevel COMPARING ALL FIELDS.

DESCRIBE TABLE lt_objectlist_nonzerolevel LINES lv_lines.
MOVE lv_lines TO lv_linesc.
CONCATENATE 'Non-zero level calls remaining after unique filter: ' lv_linesc INTO lv_msg IN CHARACTER MODE.
MESSAGE lv_msg TYPE 'I'.

SORT lt_objectlist_ref_ok BY type name.
SORT lt_objectlist_banned BY type name.

DELETE ADJACENT DUPLICATES FROM lt_objectlist_ref_ok COMPARING ALL FIELDS.
DELETE ADJACENT DUPLICATES FROM lt_objectlist_banned COMPARING ALL FIELDS.

*FOURTH - now make sure OKs don't have some from banned.
MESSAGE 'Start fourth phase -  recheck intersections' TYPE 'I'.

LOOP AT lt_objectlist_ref_ok INTO ls_objectlist.
  READ TABLE lt_objectlist_banned WITH KEY pgmid = 'R3TR' type = ls_objectlist-type name = ls_objectlist-name TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    DELETE lt_objectlist_ref_ok.
  ENDIF.
ENDLOOP.


*now let's display data
MESSAGE 'Start fifth phase -  prepare display' TYPE 'I'.

IF sy-batch <> 'X'.
  DATA alv TYPE REF TO cl_salv_table.
  DATA message TYPE REF TO cx_salv_msg.
  DATA not_found TYPE REF TO cx_salv_not_found.
  DATA layout_settings TYPE REF TO cl_salv_layout.
  DATA layout_key      TYPE salv_s_layout_key.
  DATA functions TYPE REF TO cl_salv_functions_list.
  DATA columns TYPE REF TO cl_salv_columns_table.
  DATA column  TYPE REF TO cl_salv_column.
  DATA display_settings TYPE REF TO cl_salv_display_settings.
  DATA lr_events  TYPE REF TO cl_salv_events_table.
  "DATA gr_events TYPE REF TO lcl_handle_events.
  DATA: struct_descr TYPE REF TO cl_abap_structdescr.
  TRY.
      CALL METHOD cl_salv_table=>factory
*        EXPORTING
*         list_display   = IF_SALV_C_BOOL_SAP=>FALSE
*          r_container    = g_top_container
*         container_name =
        IMPORTING
          r_salv_table = alv
        CHANGING
          t_table      = lt_objectlist_ref_ok.
    CATCH cx_salv_msg INTO message.
      " error handling
  ENDTRY.

  layout_settings = alv->get_layout( ).
  layout_key-report = sy-repid.
  layout_settings->set_key( layout_key ).
  layout_settings->set_save_restriction( if_salv_c_layout=>restrict_none ).
  functions = alv->get_functions( ).
  functions->set_all( ).
  display_settings = alv->get_display_settings( ).
  display_settings->set_striped_pattern( if_salv_c_bool_sap=>true ).
  display_settings->set_list_header( 'Identified SubObjects' ).
  columns = alv->get_columns( ).
  columns->set_optimize( ).

  alv->display( ).

ELSE.
  CHECK lt_objectlist_ref_ok IS NOT INITIAL.
  SELECT * FROM agsccl_object INTO TABLE @DATA(lt_temp2) FOR ALL ENTRIES IN @lt_objectlist_ref_ok
    WHERE system_id = @p_system
    AND   obj_type = @lt_objectlist_ref_ok-type
    AND   obj_name = @lt_objectlist_ref_ok-name.

  LOOP AT lt_temp2 INTO DATA(ls_temp2).
    ls_temp2-zdelsubobj = p_fval.
    ls_temp2-lib_changed_on = sy-datum.
    ls_temp2-lib_changed_by = sy-uname.
    MODIFY lt_temp2 FROM ls_temp2.
  ENDLOOP.

  MODIFY agsccl_object FROM TABLE lt_temp2.

ENDIF.
*&---------------------------------------------------------------------*
*&      Form  SEARCH_REFOBJECTS_FROM_MAIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM search_refobjects_from_main .
  CLEAR g_th_sfc.

* Main object Type
  CLEAR g_s_sfc.
* --- name of characteristic
  g_s_sfc-chanm    = '0SM_CCLCTYP'.
* --- name of corresponding column in G_T_DATA
  g_s_sfc-chaalias = '0SM_CCLCTYP'.
* --- no ORDER-BY
  g_s_sfc-orderby  = 0.
* --- include into list of characteristics
  INSERT g_s_sfc INTO TABLE g_th_sfc.

* Main object name
  CLEAR g_s_sfc.
* --- name of characteristic
  g_s_sfc-chanm    = '0SM_CCLCNAM'.
* --- name of corresponding column in G_T_DATA
  g_s_sfc-chaalias = '0SM_CCLCNAM'.
* --- no ORDER-BY
  g_s_sfc-orderby  = 0.
* --- include into list of characteristics
  INSERT g_s_sfc INTO TABLE g_th_sfc.

* Referenced object type
  CLEAR g_s_sfc.
* --- name of characteristic
  g_s_sfc-chanm    = '0SM_CCLTYP'.
* --- name of corresponding column in G_T_DATA
  g_s_sfc-chaalias = '0SM_CCLTYP'.
* --- no ORDER-BY
  g_s_sfc-orderby  = 0.
* --- include into list of characteristics
  INSERT g_s_sfc INTO TABLE g_th_sfc.


* Referenced object name
  CLEAR g_s_sfc.
* --- name of characteristic
  g_s_sfc-chanm    = '0SM_CCLOBJ'.
* --- name of corresponding column in G_T_DATA
  g_s_sfc-chaalias = '0SM_CCLOBJ'.
* --- no ORDER-BY
  g_s_sfc-orderby  = 1.
* --- include into list of characteristics
  INSERT g_s_sfc INTO TABLE g_th_sfc.


* Caller (environment) object type
  CLEAR g_s_sfc.
* --- name of characteristic
  g_s_sfc-chanm    = '0SM_CCLCTY'.
* --- name of corresponding column in G_T_DATA
  g_s_sfc-chaalias = '0SM_CCLCTY'.
* --- no ORDER-BY
  g_s_sfc-orderby  = 0.
* --- include into list of characteristics
  INSERT g_s_sfc INTO TABLE g_th_sfc.


* Caller (environment) object name
  CLEAR g_s_sfc.
* --- name of characteristic
  g_s_sfc-chanm    = '0SM_CCLCOBJ'.
* --- name of corresponding column in G_T_DATA
  g_s_sfc-chaalias = '0SM_CCLCOBJ'.
* --- no ORDER-BY
  g_s_sfc-orderby  = 1.
* --- include into list of characteristics
  INSERT g_s_sfc INTO TABLE g_th_sfc.

* These are the restrictions:
  CLEAR g_t_range.
* PGQ system only
  CLEAR g_s_range.
* --- name of the characteristic
  g_s_range-chanm    = '0SMD_LSID'.
* --- including or excluding condition ?
  g_s_range-sign     = rs_c_range_sign-including.
* --- comparison operator
  g_s_range-compop   = rs_c_range_opt-equal.
* --- low value
  g_s_range-low      = p_system.
* --- include into list of restrictions
  APPEND g_s_range TO g_t_range.




  LOOP AT lt_objectlist_marked ASSIGNING <fs_objectlist> WHERE type = p_mtype.
    CLEAR g_s_range.
    g_s_range-chanm    = '0SM_CCLCNAM'.
    g_s_range-sign     = rs_c_range_sign-including.
    g_s_range-compop   = rs_c_range_opt-equal.
    g_s_range-low      = <fs_objectlist>-name.
    g_s_range-high     = <fs_objectlist>-name.
    APPEND g_s_range TO g_t_range.
  ENDLOOP.

* Find only for certain main object types.
  CLEAR g_s_range.
  g_s_range-chanm    = '0SM_CCLCTYP'.
  g_s_range-sign     = rs_c_range_sign-including.
  g_s_range-compop   = rs_c_range_opt-equal.
  g_s_range-low      = p_mtype.
  g_s_range-high     = p_mtype.
  APPEND g_s_range TO g_t_range.

** Find only for certain main object types.
*  CLEAR g_s_range.
*  g_s_range-chanm    = '0SM_CCLCTYP'.
*  g_s_range-sign     = rs_c_range_sign-including.
*  g_s_range-compop   = rs_c_range_opt-equal.
*  g_s_range-low      = 'TRAN'.
*  g_s_range-high     = 'TRAN'.
*  APPEND g_s_range TO g_t_range.

* Find only certain sub object types.
  CLEAR g_s_range.
* --- name of the characteristic
  g_s_range-chanm    = '0SM_CCLTYP'.
* --- including or excluding condition ?
  g_s_range-sign     = rs_c_range_sign-including.
* --- comparison operator
  g_s_range-compop   = rs_c_range_opt-equal.
* --- low value
  g_s_range-low      = p_rtype.
* --- high value
  g_s_range-high     = p_rtype.
* --- include into list of restrictions
  APPEND g_s_range TO g_t_range.





  WHILE g_end_of_data = rs_c_false.

    CALL FUNCTION 'RSDRI_INFOPROV_READ'
      EXPORTING
        i_infoprov             = '0SM_CCREF'
        i_th_sfc               = g_th_sfc
        i_th_sfk               = g_th_sfk
        i_t_range              = g_t_range
        i_reference_date       = sy-datum
        i_save_in_table        = rs_c_false
        i_save_in_file         = rs_c_false
        i_packagesize          = 10000000
 "      i_authority_check      = rsdrc_c_authchk-read
      IMPORTING
        e_t_data               = g_t_data
        e_end_of_data          = g_end_of_data
      CHANGING
        c_first_call           = g_first_call
      EXCEPTIONS
        illegal_input          = 1
        illegal_input_sfc      = 2
        illegal_input_sfk      = 3
        illegal_input_range    = 4
        illegal_input_tablesel = 5
        no_authorization       = 6
        illegal_download       = 8
        illegal_tablename      = 9
        OTHERS                 = 11.

    IF sy-subrc <> 0.
      MESSAGE 'BW cubes was not read successfully' TYPE 'E'.
    ELSE.
      CLEAR: lv_lines, lv_linesc, lv_msg.
      DESCRIBE TABLE g_t_data LINES lv_lines.
      MOVE lv_lines TO lv_linesc.
      CONCATENATE 'Referenced objects fetched from BW: ' lv_linesc INTO lv_msg IN CHARACTER MODE.
      MESSAGE lv_msg TYPE 'S'.
    ENDIF.


  ENDWHILE.
ENDFORM.                    " SEARCH_REFOBJECTS_FROM_MAIN
*&---------------------------------------------------------------------*
*&      Form  SEARCH_ALLMAIN_FROM_REFOBJECTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM search_allmain_from_refobjects .
  CLEAR g_th_sfc.

* Main object Type
  CLEAR g_s_sfc.
  g_s_sfc-chanm    = '0SM_CCLCTYP'.
  g_s_sfc-chaalias = '0SM_CCLCTYP'.
  g_s_sfc-orderby  = 0.
  INSERT g_s_sfc INTO TABLE g_th_sfc.

* Main object name
  CLEAR g_s_sfc.
  g_s_sfc-chanm    = '0SM_CCLCNAM'.
  g_s_sfc-chaalias = '0SM_CCLCNAM'.
  g_s_sfc-orderby  = 0.
  INSERT g_s_sfc INTO TABLE g_th_sfc.

* Referenced object type
  CLEAR g_s_sfc.
  g_s_sfc-chanm    = '0SM_CCLTYP'.
  g_s_sfc-chaalias = '0SM_CCLTYP'.
  g_s_sfc-orderby  = 0.
  INSERT g_s_sfc INTO TABLE g_th_sfc.


* Referenced object name
  CLEAR g_s_sfc.
  g_s_sfc-chanm    = '0SM_CCLOBJ'.
  g_s_sfc-chaalias = '0SM_CCLOBJ'.
  g_s_sfc-orderby  = 1.
  INSERT g_s_sfc INTO TABLE g_th_sfc.

* Caller object name
  CLEAR g_s_sfc.
  g_s_sfc-chanm    = '0SM_CCLCTY'.
  g_s_sfc-chaalias = '0SM_CCLCTY'.
  g_s_sfc-orderby  = 0.
  INSERT g_s_sfc INTO TABLE g_th_sfc.

* Caller object name
  CLEAR g_s_sfc.
  g_s_sfc-chanm    = '0SM_CCLCOBJ'.
  g_s_sfc-chaalias = '0SM_CCLCOBJ'.
  g_s_sfc-orderby  = 0.
  INSERT g_s_sfc INTO TABLE g_th_sfc.

* These are the restrictions:
  CLEAR g_t_range.
  CLEAR g_s_range.
  g_s_range-chanm    = '0SMD_LSID'.
  g_s_range-sign     = rs_c_range_sign-including.
  g_s_range-compop   = rs_c_range_opt-equal.
  g_s_range-low      = p_system.
  APPEND g_s_range TO g_t_range.

* Use only certain sub object types.
  CLEAR g_s_range.
  g_s_range-chanm    = '0SM_CCLTYP'.
  g_s_range-sign     = rs_c_range_sign-including.
  g_s_range-compop   = rs_c_range_opt-equal.
  g_s_range-low      = p_rtype.
  APPEND g_s_range TO g_t_range.


* Find only for certain main object types PROG.
*IF p_mtype = 'INCL'.
*  CLEAR g_s_range.
*  g_s_range-chanm    = '0SM_CCLCTYP'.
*  g_s_range-sign     = rs_c_range_sign-including.
*  g_s_range-compop   = rs_c_range_opt-equal.
*  g_s_range-low      = p_mtype.
*  g_s_range-high     = p_mtype.
*  APPEND g_s_range TO g_t_range.
*ENDIF.

* Find main objects for certain sub object.
  CLEAR g_s_range.
  LOOP AT lt_objectlist_includes ASSIGNING <fs_objectlist>.
    g_s_range-chanm    = '0SM_CCLOBJ'.
    g_s_range-sign     = rs_c_range_sign-including.
    g_s_range-compop   = rs_c_range_opt-equal.
    g_s_range-low      = <fs_objectlist>-name.
    APPEND g_s_range TO g_t_range.
  ENDLOOP.


  REFRESH: g_t_data.
  CLEAR: g_t_data, g_end_of_data.

  WHILE g_end_of_data = rs_c_false.

    CALL FUNCTION 'RSDRI_INFOPROV_READ'
      EXPORTING
        i_infoprov             = '0SM_CCREF'
        i_th_sfc               = g_th_sfc
        i_th_sfk               = g_th_sfk
        i_t_range              = g_t_range
        i_reference_date       = sy-datum
        i_save_in_table        = rs_c_false
        i_save_in_file         = rs_c_false
        i_packagesize          = 10000000
 "      i_authority_check      = rsdrc_c_authchk-read
      IMPORTING
        e_t_data               = g_t_data
        e_end_of_data          = g_end_of_data
      CHANGING
        c_first_call           = g_first_call
      EXCEPTIONS
        illegal_input          = 1
        illegal_input_sfc      = 2
        illegal_input_sfk      = 3
        illegal_input_range    = 4
        illegal_input_tablesel = 5
        no_authorization       = 6
        illegal_download       = 8
        illegal_tablename      = 9
        OTHERS                 = 11.

    IF sy-subrc <> 0.
      MESSAGE 'BW cubes was not read successfully' TYPE 'E'.
    ELSE.
      CLEAR: lv_lines, lv_linesc, lv_msg.
      DESCRIBE TABLE g_t_data LINES lv_lines.
      MOVE lv_lines TO lv_linesc.
      CONCATENATE 'Main objects for Includes fetched from BW: ' lv_linesc INTO lv_msg IN CHARACTER MODE.
      MESSAGE lv_msg TYPE 'I'.
    ENDIF.


  ENDWHILE.
ENDFORM.                    " SEARCH_ALLMAIN_FROM_REFOBJECTS


FORM get_attr    CHANGING   p_attr TYPE string.

  DATA: BEGIN OF ls_attr,
          attrib_name TYPE ags_cc_attribute_name,
        END OF ls_attr.

  DATA: lt_attr        LIKE TABLE OF ls_attr.
  DATA: value_tab     LIKE lt_attr WITH HEADER LINE,
        return_tab    LIKE ddshretval OCCURS 0 WITH HEADER LINE,
        value         LIKE help_info-fldvalue,
*       index like sy-tabix,
        persvaluelist TYPE ddshpvkey.


*Get attributes
  SELECT attrib_name
    FROM agsccl_attr_meta
    INTO CORRESPONDING FIELDS OF TABLE lt_attr
    WHERE db_tab = 'AGSCCL_OBJECT' AND
    attrib_name <> ''.
  SORT lt_attr.
  DELETE ADJACENT DUPLICATES FROM lt_attr.

*Open F4 Dialog
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE  = ' '
      retfield        = 'ATTRIB_NAME'
      pvalkey         = persvaluelist
*     DYNPPROG        = ' '
*     DYNPNR          = ' '
*     DYNPROFIELD     = ' '
*     STEPL           = 0
      window_title    = 'Attributes'
      value           = value
      value_org       = 'S'
      multiple_choice = ' '
*     DISPLAY         = 'Selected'
*     CALLBACK_PROGRAM = ' '
*     CALLBACK_FORM   = ' '
    TABLES
      value_tab       = lt_attr
*     FIELD_TAB       =
      return_tab      = return_tab
*     DYNPFLD_MAPPING =
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  READ TABLE return_tab INDEX 1.
  IF sy-subrc EQ 0.
    p_attr = return_tab-fieldval.
  ENDIF.

ENDFORM.



FORM get_obj_type    CHANGING   p_attr TYPE string.

  DATA: BEGIN OF ls_attr,
          obj_type TYPE ags_cc_attribute_name,
        END OF ls_attr.

  DATA: lt_attr        LIKE TABLE OF ls_attr.
  DATA: value_tab     LIKE lt_attr WITH HEADER LINE,
        return_tab    LIKE ddshretval OCCURS 0 WITH HEADER LINE,
        value         LIKE help_info-fldvalue,
*       index like sy-tabix,
        persvaluelist TYPE ddshpvkey.


*Get attributes
  SELECT obj_type
    FROM agsccl_object
    INTO CORRESPONDING FIELDS OF TABLE lt_attr.
  SORT lt_attr.
  DELETE ADJACENT DUPLICATES FROM lt_attr.

*Open F4 Dialog
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE  = ' '
      retfield        = 'OBJ_NAME'
      pvalkey         = persvaluelist
*     DYNPPROG        = ' '
*     DYNPNR          = ' '
*     DYNPROFIELD     = ' '
*     STEPL           = 0
      window_title    = 'Attributes'
      value           = value
      value_org       = 'S'
      multiple_choice = ' '
*     DISPLAY         = 'Selected'
*     CALLBACK_PROGRAM = ' '
*     CALLBACK_FORM   = ' '
    TABLES
      value_tab       = lt_attr
*     FIELD_TAB       =
      return_tab      = return_tab
*     DYNPFLD_MAPPING =
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  READ TABLE return_tab INDEX 1.
  IF sy-subrc EQ 0.
    p_attr = return_tab-fieldval.
  ENDIF.

ENDFORM.
