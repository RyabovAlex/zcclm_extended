*&---------------------------------------------------------------------*
*& Report  ZCCLM_MASS_UPDATE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZCCLM_MASS_UPDATE.

TYPES: BEGIN OF w_sid,
         sid TYPE sysysid,
       END OF w_sid.
TYPES: t_sid TYPE STANDARD TABLE OF w_sid.
DATA: it_tab   TYPE filetable,
      ls_tab   LIKE LINE OF it_tab,
      gd_subrc TYPE i.
DATA: sel_sys_low TYPE string_table,
      tmp_sys_id  TYPE string,
      sv_decis    TYPE string.
DATA fname TYPE string.
DATA it_new_objects TYPE agsccl_object_t.
DATA it_old_objects TYPE agsccl_object_t.
DATA it_fail_objects TYPE agsccl_object_t.
TABLES: agsccl_attr_meta,
        agsccl_system,
        agsccl_object.

DATA: lt_sid TYPE TABLE OF agsccl_system.
DATA: ls_sid LIKE LINE OF lt_sid.

DATA: data_ref      TYPE REF TO data,
      lv_where(255) TYPE c,
      where_tab     LIKE TABLE OF lv_where.

SELECTION-SCREEN BEGIN OF BLOCK 0 WITH FRAME.
SELECTION-SCREEN COMMENT /1(50) comm1.
SELECTION-SCREEN COMMENT /1(79) comm2.
SELECTION-SCREEN COMMENT /1(79) comm3.
SELECTION-SCREEN END OF BLOCK 0.

SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE txt1.
PARAMETERS:  p_inp1 RADIOBUTTON GROUP  inp USER-COMMAND com2.                     "Excel upload
PARAMETERS:  p_file LIKE rlgrap-filename DEFAULT 'Filename.xlsx'  MODIF ID in1.   "File Name
PARAMETERS:  p_inp2 RADIOBUTTON GROUP  inp.                                       "Object list
SELECT-OPTIONS: s_objnam FOR  agsccl_object-obj_name NO INTERVALS DEFAULT 'Z4' MODIF ID in2.
PARAMETERS:     p_objtyp TYPE agsccl_object-obj_type              DEFAULT 'PROG' MODIF ID in2.
PARAMETERS:  p_inp3 RADIOBUTTON GROUP  inp DEFAULT 'X' .         "Attributes
SELECTION-SCREEN END OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE txt2.
SELECT-OPTIONS p_attrn FOR agsccl_attr_meta-attrib_name NO-EXTENSION NO INTERVALS DEFAULT 'Z_FISCHER'." Attribute Name.
PARAMETERS:    p_attrv(15) TYPE c                                                DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK c1 WITH FRAME TITLE txt3.
PARAMETERS: p_rad1 RADIOBUTTON GROUP rad  USER-COMMAND com1,   "all
            p_rad2 RADIOBUTTON GROUP  rad DEFAULT 'X'.            "certain project
SELECT-OPTIONS p_sid FOR agsccl_system-system_id NO-EXTENSION NO INTERVALS DEFAULT 'FBT' MODIF ID sc1.   " certain project
SELECTION-SCREEN END OF BLOCK c1.

SELECTION-SCREEN BEGIN OF BLOCK d1 WITH FRAME TITLE text-004.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
SELECTION-SCREEN COMMENT 1(79) text-030.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
SELECTION-SCREEN COMMENT 1(79) text-031.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
SELECTION-SCREEN COMMENT 1(79) text-032.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK d1.


INITIALIZATION.

***********************************************************************
* AT SELECTION-SCREEN OUTPUT
***********************************************************************

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM get_filename.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_attrn-low.
  PERFORM get_attr CHANGING sv_decis.
  p_attrn-low = sv_decis.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_sid-low.
  CLEAR sel_sys_low.
  PERFORM get_systems   TABLES   sel_sys_low
                        USING    tmp_sys_id.
  READ TABLE sel_sys_low INDEX 1 INTO p_sid-low.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'SC1' AND p_rad1 EQ 'X'.
      screen-active = '0'.
      MODIFY SCREEN.
      CONTINUE.
    ELSEIF screen-group1 = 'IN1' AND p_inp2 EQ 'X'.
      screen-active = '0'.
      MODIFY SCREEN.
      CONTINUE.
    ELSEIF screen-group1 = 'IN2' AND p_inp1 EQ 'X'.
      screen-active = '0'.
      MODIFY SCREEN.
      CONTINUE.
    ELSEIF p_inp3 EQ 'X' AND ( screen-group1 = 'IN2' OR screen-group1 = 'IN1').
      screen-active = '0'.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  comm1 = 'CCLM Mass update program'.
  comm2 = 'This program allows to change any attribute of AGSCCL_OBJECT table'.
  comm3 = ''.

  txt1 = 'Please define the object list for processing'.
  txt2 = 'Define attributes for precise selection'.
  txt3 = 'For which system update the attribute?'.


************************************************************************
*** START-OF-SELECTION
************************************************************************

START-OF-SELECTION.

  IF p_file IS INITIAL AND s_objnam IS INITIAL AND p_objtyp IS INITIAL AND p_attrn IS INITIAL AND p_attrv IS INITIAL.
    MESSAGE 'Please define at least any attribute' TYPE 'E'.
  ENDIF.

  IF p_rad1 = 'X'. "update for all system
    SELECT system_id FROM agsccl_system INTO TABLE lt_sid.
    SORT lt_sid BY system_id.
    DELETE ADJACENT DUPLICATES FROM lt_sid COMPARING system_id.
  ELSEIF p_rad1 <> 'X' AND p_sid-low IS NOT INITIAL. "system selected
    MOVE p_sid-low TO ls_sid-system_id.
    APPEND ls_sid TO lt_sid.
  ENDIF.

  LOOP AT lt_sid INTO ls_sid.
    IF sy-tabix = 1.
      CONCATENATE 'SYSTEM_ID = ''' ls_sid-system_id '''' INTO lv_where.
      APPEND lv_where TO where_tab.
    ELSE.
      lv_where = 'OR'.
      APPEND lv_where TO where_tab.
      CONCATENATE 'SYSTEM_ID = ''' ls_sid-system_id '''' INTO lv_where.
      APPEND lv_where TO where_tab.
    ENDIF.
  ENDLOOP.

  IF p_inp1 = 'X'. "Use Excel as input
* Parse excel to determine changes in attributes and provide internal tables with old and new objects fields
*    CALL FUNCTION 'ZCCLM_UPD_ATTR_XLS_FM'
*      EXPORTING
*        filename       = fname
*        attr           = p_attrn-low
*        system_id      = lt_sid
*      CHANGING
*        it_new_objects = it_new_objects
*        it_old_objects = it_old_objects
*        it_fail_objects = it_fail_objects.
  ELSEIF p_inp2 = 'X'. "Use File selection
    IF s_objnam IS NOT INITIAL AND s_objnam <> '*'.
        lv_where = 'AND'.
        APPEND lv_where TO where_tab.
      LOOP AT s_objnam ASSIGNING FIELD-SYMBOL(<fs_objnam>).
        IF sy-tabix = 1.
        CONCATENATE '(' ' OBJ_NAME = ''' <fs_objnam>-low '''' INTO lv_where.
        else.
          CONCATENATE lv_where ' OR OBJ_NAME = ''' <fs_objnam>-low '''' INTO lv_where.
        ENDIF.
      ENDLOOP.
      CONCATENATE lv_where ' )' into lv_where.
      APPEND lv_where TO where_tab.
      IF p_objtyp IS NOT INITIAL.
        lv_where = 'AND'.
        APPEND lv_where TO where_tab.
        CONCATENATE 'OBJ_TYPE = ''' p_objtyp '''' INTO lv_where.
        APPEND lv_where TO where_tab.
      ENDIF.


    ENDIF.
  ELSEIF p_inp3 = 'X'. "Use attributes only
    "use attribute selection only
    "no logic needed
  ENDIF.

  "Perform filterings and attributes selection
  IF p_attrn is not INITIAL and p_attrv is not initial.
      lv_where = 'AND'.
      APPEND lv_where TO where_tab.
      CONCATENATE p_attrn-low ' = ''' p_attrv '''' INTO lv_where.
      APPEND lv_where TO where_tab.
  ENDIF.

  "perform main select
        SELECT * FROM agsccl_object INTO TABLE it_old_objects WHERE (where_tab).


* Display changes in ALV and ask for commit
  PERFORM alv_display.

* Commit changes to database

END-OF-SELECTION.


FORM get_filename .
  REFRESH: it_tab.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title     = 'Select File'
      default_filename = '*.xls'
      multiselection   = ' '
    CHANGING
      file_table       = it_tab
      rc               = gd_subrc.
  READ TABLE it_tab INTO ls_tab INDEX 1.
  p_file = ls_tab-filename.

  MOVE p_file TO fname.

ENDFORM.                    " GET_FILENAME


FORM get_systems  TABLES   p_sel_sys TYPE string_table
                  USING    p_tmp_sys_id  TYPE string.

  DATA: BEGIN OF ls_sid,
          system_id TYPE system_id,
        END OF ls_sid.
  DATA: lt_sid        LIKE TABLE OF ls_sid.
  DATA: value_tab     LIKE lt_sid WITH HEADER LINE,
        return_tab    LIKE ddshretval OCCURS 0 WITH HEADER LINE,
        value         LIKE help_info-fldvalue,
*       index like sy-tabix,
        persvaluelist TYPE ddshpvkey.


*Get systems
  SELECT system_id FROM agsccl_system INTO CORRESPONDING FIELDS OF TABLE lt_sid.
  SORT lt_sid.
  DELETE ADJACENT DUPLICATES FROM lt_sid.

*Open F4 Dialog
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE  = ' '
      retfield        = 'SYSTEM_ID'
      pvalkey         = persvaluelist
*     DYNPPROG        = ' '
*     DYNPNR          = ' '
*     DYNPROFIELD     = ' '
*     STEPL           = 0
      window_title    = 'CCM related Systems'
      value           = value
      value_org       = 'S'
      multiple_choice = 'X'
*     DISPLAY         = 'Selected'
*     CALLBACK_PROGRAM = ' '
*     CALLBACK_FORM   = ' '
    TABLES
      value_tab       = lt_sid
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

  CLEAR p_sel_sys.
  READ TABLE return_tab INDEX 1.
  LOOP AT return_tab.
    p_tmp_sys_id = return_tab-fieldval.
    APPEND p_tmp_sys_id TO p_sel_sys.
  ENDLOOP.

ENDFORM.                    " GET_SYSTEMS


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


  IF return_tab-fieldval CP 'Z*'.
    p_attr = return_tab-fieldval.
  ELSEIF return_tab-fieldval IS NOT INITIAL.
    DATA: lv_answer TYPE c.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar       = 'Are you sure?'
*       DIAGNOSE_OBJECT             = ' '
        text_question  = 'You are trying to update Standard SAP Attribute. Z-Attributes start with Z*. Continue?'
        text_button_1  = 'YES'
*       ICON_BUTTON_1  = ' '
        text_button_2  = 'NO'
*       ICON_BUTTON_2  = ' '
        default_button = '2'
*       DISPLAY_CANCEL_BUTTON       = 'X'
*       USERDEFINED_F1_HELP         = ' '
*       START_COLUMN   = 25
*       START_ROW      = 6
*       POPUP_TYPE     =
*       IV_QUICKINFO_BUTTON_1       = ' '
*       IV_QUICKINFO_BUTTON_2       = ' '
      IMPORTING
        answer         = lv_answer
*                  TABLES
*       PARAMETER      =
*                  EXCEPTIONS
*       TEXT_NOT_FOUND = 1
*       OTHERS         = 2
      .

    IF sy-subrc EQ 0 AND lv_answer CP '1'.
      p_attr = return_tab-fieldval.
    ENDIF.

  ENDIF.

ENDFORM.

FORM alv_display.
  CALL SCREEN 0100.
ENDFORM.

FORM commit.
  UPDATE agsccl_object FROM TABLE it_new_objects.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED.
  MESSAGE 'Check your data in AGSCCL_TABLE!' TYPE 'I'.
  EXIT.
ENDFORM.

MODULE status_0100 OUTPUT.

  DATA alv TYPE REF TO cl_salv_table.
  DATA alv2 TYPE REF TO cl_salv_table.
  DATA message TYPE REF TO cx_salv_msg.
  DATA not_found TYPE REF TO cx_salv_not_found.
  DATA layout_settings TYPE REF TO cl_salv_layout.
  DATA layout_key      TYPE salv_s_layout_key.
  DATA functions TYPE REF TO cl_salv_functions_list.
  DATA columns TYPE REF TO cl_salv_columns_table.
  DATA columns2 TYPE REF TO cl_salv_columns_table.
  DATA column  TYPE REF TO cl_salv_column.
  DATA column2  TYPE REF TO cl_salv_column.
  DATA display_settings TYPE REF TO cl_salv_display_settings.
  DATA: ls_object     TYPE agsccl_object,
        ls_object_new TYPE agsccl_object.
  DATA: struct_descr TYPE REF TO cl_abap_structdescr,
        wa_comp      TYPE abap_compdescr,
        lv_colname   TYPE lvc_fname.
  DATA: l_columname   TYPE lvc_fname,
        l_long_text   TYPE scrtext_l,
        l_medium_text TYPE scrtext_m,
        l_short_text  TYPE scrtext_s.
  DATA ind TYPE i.

  TYPES: BEGIN OF t_chng,
           sid     TYPE system_id,
           type    TYPE c LENGTH 4,
           name    TYPE c LENGTH 100,
           old_val TYPE c LENGTH 80,
           new_val TYPE c LENGTH 80,
         END OF t_chng.
  DATA ls_change TYPE t_chng.
  DATA ls_fail TYPE t_chng.
  DATA it_changes TYPE STANDARD TABLE OF t_chng.
  DATA it_fails TYPE STANDARD TABLE OF t_chng.


  FIELD-SYMBOLS: <comp_descr> LIKE LINE OF cl_abap_structdescr=>components.
  DATA: lr_selections TYPE REF TO cl_salv_selections.
  DATA: g_custom_container   TYPE REF TO cl_gui_custom_container, "custom container
        g_splitter_container TYPE REF TO cl_gui_splitter_container, "splitter container
        g_top_container      TYPE REF TO cl_gui_container, "top container
        g_bottom_container   TYPE REF TO cl_gui_container, "bottom one
        g_display            TYPE REF TO cl_salv_display_settings. " set display pattern
  "create custom container placed in CUSTOM AREA defined on screen

  READ TABLE it_new_objects INDEX 1 INTO ls_object.
  TRY.
      struct_descr ?= cl_abap_typedescr=>describe_by_data( ls_object ).
    CATCH cx_sy_move_cast_error.
      RETURN.
  ENDTRY.
  FIELD-SYMBOLS: <fs_decis> TYPE any.
  IF sv_decis IS INITIAL.
    ASSIGN p_attrn-low TO <fs_decis>.
  ELSEIF sv_decis IS NOT INITIAL.
    ASSIGN sv_decis TO <fs_decis>.
  ENDIF.
  LOOP AT struct_descr->components INTO wa_comp.
    IF wa_comp-name EQ <fs_decis>.
      ind = sy-tabix.
      DATA(t_newtype) = struct_descr->get_component_type( <fs_decis> ).
      EXIT.
    ENDIF.
  ENDLOOP.

  IF alv IS NOT INITIAL.
    alv->refresh( ).
  ELSEIF g_custom_container IS NOT INITIAL.
    g_custom_container->free( ).
  ELSEIF g_splitter_container IS NOT INITIAL.
    g_splitter_container->free( ).
  ENDIF.

  CREATE OBJECT g_custom_container
    EXPORTING
      container_name              = 'CONTAINER'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  "now split the container into two independent containers
  CREATE OBJECT g_splitter_container
    EXPORTING
      parent            = g_custom_container
      rows              = 2
      columns           = 1
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  "get top container
  CALL METHOD g_splitter_container->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = g_top_container.

  "get top container
  CALL METHOD g_splitter_container->get_container
    EXPORTING
      row       = 2
      column    = 1
    RECEIVING
      container = g_bottom_container.
  TRY.

      LOOP AT it_old_objects INTO ls_object.
        FIELD-SYMBOLS: <fs> TYPE any.
        CLEAR ls_change.
        ls_change-sid = ls_object-system_id.
        ls_change-type = ls_object-obj_type.
        ls_change-name = ls_object-obj_name.
        READ TABLE it_new_objects INTO ls_object_new INDEX sy-tabix.  ""here
        ASSIGN COMPONENT ind OF STRUCTURE ls_object TO <fs>.
        MOVE <fs> TO ls_change-old_val.
        ASSIGN COMPONENT ind OF STRUCTURE ls_object_new TO <fs>.
        MOVE <fs> TO ls_change-new_val.
        APPEND ls_change TO it_changes.
      ENDLOOP.

      CALL METHOD cl_salv_table=>factory
        EXPORTING
*         list_display = IF_SALV_C_BOOL_SAP=>FALSE
          r_container  = g_top_container
*         container_name =
        IMPORTING
          r_salv_table = alv
        CHANGING
          t_table      = it_changes.
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
  display_settings->set_list_header( 'Objects affected by changes' ).
  columns = alv->get_columns( ).
  columns->set_optimize( ).

  lr_selections = alv->get_selections( ).
  lr_selections->set_selection_mode( if_salv_c_selection_mode=>none  ).

  struct_descr ?= cl_abap_typedescr=>describe_by_data( ls_change ).

  LOOP AT struct_descr->components ASSIGNING <comp_descr>.
    lv_colname = <comp_descr>-name.
    column ?= columns->get_column( lv_colname ).
    l_short_text = l_medium_text = l_long_text = <comp_descr>-name.
    column->set_long_text(  l_long_text ).
    column->set_medium_text( l_medium_text ).
    column->set_short_text( l_short_text ).
  ENDLOOP.

  alv->display( ).

  IF alv2 IS NOT INITIAL.
    alv2->refresh( ).
  ENDIF.
  CLEAR: ls_object.
  LOOP AT it_fail_objects INTO ls_object.
    CLEAR ls_fail.
    ls_fail-sid = ls_object-system_id.
    ls_fail-type = ls_object-obj_type.
    ls_fail-name = ls_object-obj_name.
*        READ TABLE it_fail_objects INTO ls_object_fail INDEX sy-tabix.  ""here
*        ASSIGN COMPONENT ind OF STRUCTURE ls_object TO <fs>.
*        MOVE <fs> TO ls_change-old_val.
*        ASSIGN COMPONENT ind OF STRUCTURE ls_object_new TO <fs>.
*        MOVE <fs> TO ls_change-new_val.
    APPEND ls_fail TO it_fails.
  ENDLOOP.

  TRY.
      CALL METHOD cl_salv_table=>factory
        EXPORTING
*         list_display = IF_SALV_C_BOOL_SAP=>FALSE
          r_container  = g_bottom_container
*         container_name =
        IMPORTING
          r_salv_table = alv2
        CHANGING
          t_table      = it_fails.

    CATCH cx_salv_msg INTO message.
      " error handling
  ENDTRY.

  layout_settings = alv2->get_layout( ).
  layout_key-report = 'report2'.
  layout_settings->set_key( layout_key ).
  layout_settings->set_save_restriction( if_salv_c_layout=>restrict_none ).
  functions = alv2->get_functions( ).
  functions->set_all( ).
  display_settings = alv2->get_display_settings( ).
  display_settings->set_striped_pattern( if_salv_c_bool_sap=>true ).
  display_settings->set_list_header( 'Objects not found' ).
  columns = alv2->get_columns( ).
  columns->set_optimize( ).
  lr_selections = alv2->get_selections( ).
  lr_selections->set_selection_mode( if_salv_c_selection_mode=>none ).

  struct_descr ?= cl_abap_typedescr=>describe_by_data( ls_change ).

  LOOP AT struct_descr->components ASSIGNING <comp_descr>.
    lv_colname = <comp_descr>-name.
    column ?= columns->get_column( lv_colname ).
    l_short_text = l_medium_text = l_long_text = <comp_descr>-name.
    column->set_long_text(  l_long_text ).
    column->set_medium_text( l_medium_text ).
    column->set_short_text( l_short_text ).
  ENDLOOP.


  alv2->display( ).
ENDMODULE.

MODULE user_command_0100 INPUT.
  DATA: ok_code TYPE sy-ucomm.
  ok_code = sy-ucomm.
  IF ok_code = 'BACK' OR ok_code = 'EXIT' OR ok_code = 'CANCEL' OR
    ok_code = 'B' OR ok_code = 'E' OR ok_code = 'C'.
    LEAVE PROGRAM.
  ENDIF.
  IF ok_code = 'ONLI'.
    PERFORM commit.
  ENDIF.
  CLEAR ok_code.
ENDMODULE.
