*&---------------------------------------------------------------------*
*& Report ZDBTABLE_UPDN
*& Program to mass upload/download data in the database table.
*&---------------------------------------------------------------------*
REPORT zsm30massimport.
TABLES: sscrfields.

CONSTANTS :
  BEGIN OF gc,
    sflight          TYPE rsrd1-tbma_val VALUE 'SFLIGHT',
    sbook            TYPE rsrd1-tbma_val VALUE 'SBOOK',
*    Z_TMG_table TYPE rsrd1-tbma_val VALUE 'Z_TMG_table',

    import_file      TYPE sscrfields-ucomm VALUE 'FC05',
    import_clipboard TYPE sscrfields-ucomm VALUE 'FC03',
    export_file      TYPE sscrfields-ucomm VALUE 'FC04',

  END OF gc.

TYPES: BEGIN OF _text,
         line TYPE c LENGTH 1000,
       END OF _text,
       _text_tab TYPE STANDARD TABLE OF _text WITH DEFAULT KEY.

DATA :
  doc_container TYPE REF TO cl_gui_docking_container.

FIELD-SYMBOLS:
  <ft_tab_key>  TYPE ANY TABLE,
  <ft_line_key> TYPE data,
  <ft_tab>      TYPE STANDARD TABLE.

*— Declaration for factory ALV
DATA: salv TYPE REF TO cl_salv_table.

*— Selection Screen
PARAMETERS: ptable TYPE rsrd1-tbma_val AS LISTBOX VISIBLE LENGTH 40 USER-COMMAND ptab.
SELECTION-SCREEN FUNCTION KEY 3.
SELECTION-SCREEN FUNCTION KEY 4.
SELECTION-SCREEN FUNCTION KEY 5.

INITIALIZATION.
  PERFORM f_init.

*— Validate table name
AT SELECTION-SCREEN ON ptable.
  PERFORM f_validate_table.

AT SELECTION-SCREEN.

  CASE sscrfields-ucomm.
    WHEN gc-import_file OR gc-import_clipboard.
      CHECK ptable IS NOT INITIAL.
      PERFORM f_call_sm30.
    WHEN gc-export_file." 'FC04'.
      CHECK ptable IS NOT INITIAL.
      PERFORM f_export_to_pc.
    WHEN 'PTAB'.
*      CHECK ptable IS NOT INITIAL.
      PERFORM f_build_container.
  ENDCASE.

FORM f_init.
  DATA : li_list    TYPE vrm_values,
         lt_exclude TYPE TABLE OF rsexfcode.

  lt_exclude = VALUE #(
    ( fcode = 'PRIN' )  "Execute and Print.
    ( fcode = 'ONLI' )  "Execute.
    ( fcode = 'SJOB' )  "Execute in Background
    ( fcode = 'VDEL' )  "Variant Delete
    ( fcode = 'SPOS' )  "Variant Save
    ( fcode = 'GET'  )   "Get...
    ( fcode = 'VSHO' )  "Display...
    ( fcode = 'VDEL' )  "Delete...
    ( fcode = 'SPOS' )  "Save as Variant...
    ( fcode = 'LVUV' ) ). "User Variables...

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
    TABLES
      p_exclude = lt_exclude.

  DATA functxt TYPE smp_dyntxt.

  functxt-icon_id = icon_import.
  functxt-quickinfo = 'Import from file'.
  functxt-icon_text = 'Import from file'.
  sscrfields-functxt_05 = functxt.

  functxt-icon_id = icon_export.
  functxt-quickinfo = 'Export to file'.
  functxt-icon_text = 'Export to file'.
  sscrfields-functxt_04 = functxt.


  functxt-icon_id = icon_system_local_paste.
  functxt-quickinfo = 'Import from Clipboard'.
  functxt-icon_text = 'Import from Clipboard'.
  sscrfields-functxt_03 = functxt.

*DB table list
  li_list = VALUE #(
       ( key = gc-sbook text = 'SBOOK:Single Flight Booking' )
       ( key = gc-sflight text = 'SFLIGHT:Flight' ) ).
  ptable = gc-sflight.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'PTABLE'
      values          = li_list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  PERFORM f_build_container.

ENDFORM.

FORM import_file CHANGING import_data_csv TYPE _text_tab.
  DATA:
    lv_filename TYPE string,
    lt_files    TYPE filetable,
    rc          TYPE i.

  lv_filename = zcl_file_utility=>pc_file_open_dialog( ).

  IF lv_filename IS NOT INITIAL.

    cl_gui_frontend_services=>gui_upload(
       EXPORTING
         filename        = lv_filename
         filetype        = 'ASC'
*         has_field_separator     = 'X'
       CHANGING
         data_tab        = import_data_csv
       EXCEPTIONS
         file_open_error = 1
         file_read_error         = 2
         OTHERS                  = 3 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_call_sm30.
  DATA:
    import_data_csv TYPE _text_tab.

  DATA(import_table) = CONV tabname( ptable ).

  IF sscrfields-ucomm = gc-import_file.
    PERFORM import_file CHANGING import_data_csv. "TYPE _text_tab
    DATA(delimiter) = cl_abap_char_utilities=>horizontal_tab." ';'.
  ELSE.
    cl_gui_frontend_services=>clipboard_import(
      IMPORTING
        data  = import_data_csv ).
    delimiter = cl_abap_char_utilities=>horizontal_tab.
  ENDIF.

  FIELD-SYMBOLS <import_data_line> TYPE any.
  FIELD-SYMBOLS <import_data_tab> TYPE table.

  DATA import_data_table_ref TYPE REF TO data.
  DATA import_data_struc_ref TYPE REF TO data.
  DATA(import_data_struc) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( ptable ) ).
  DATA(vimflagtab_struc) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( 'VIMFLAGTAB' ) ).

  DATA(maint_struc_components) = import_data_struc->get_components( ).

  APPEND LINES OF vimflagtab_struc->get_components( ) TO maint_struc_components.

  DATA(import_maint_struc) = cl_abap_structdescr=>create( maint_struc_components ).

  DATA(import_data_table) = cl_abap_tabledescr=>create( p_line_type  = import_maint_struc ).

  CREATE DATA import_data_struc_ref TYPE HANDLE import_maint_struc.
  ASSIGN import_data_struc_ref->* TO <import_data_line>.

  CREATE DATA import_data_table_ref TYPE HANDLE import_data_table.
  ASSIGN import_data_table_ref->* TO <import_data_tab>.

  DATA tab_key TYPE REF TO data.
  FIELD-SYMBOLS <fs_tab_key> TYPE any.

  CREATE DATA tab_key TYPE (ptable).
  ASSIGN tab_key->* TO <fs_tab_key>.

  LOOP AT import_data_csv INTO DATA(csv_line).
    CLEAR <import_data_line>.
    SPLIT csv_line AT delimiter INTO TABLE DATA(import_data_values).
    LOOP AT import_data_values INTO DATA(value).
      ASSIGN COMPONENT sy-tabix OF STRUCTURE <import_data_line> TO FIELD-SYMBOL(<field>).
      <field> = value.

      ASSIGN COMPONENT sy-tabix OF STRUCTURE <fs_tab_key> TO FIELD-SYMBOL(<field_key>).
      <field_key> = value.

    ENDLOOP.
    ASSIGN COMPONENT 'ACTION' OF STRUCTURE <import_data_line> TO FIELD-SYMBOL(<action>).

    <ft_line_key> = <import_data_line>.

    READ TABLE <ft_tab_key> ASSIGNING FIELD-SYMBOL(<fs_line>) WITH KEY ('KEY') = <ft_line_key>.
    IF <fs_line> IS ASSIGNED.
      "U = Updated entry
      <action> = 'U'.
    ELSE.
      "N = New entry
      <action> = 'N'.
    ENDIF.
    UNASSIGN <fs_line>.

    APPEND <import_data_line> TO <import_data_tab>.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM <import_data_tab> COMPARING ALL FIELDS.

  CALL FUNCTION 'VIEW_MAINTENANCE_GIVEN_DATA'
    EXPORTING
      action                       = 'U'
      view_name                    = import_table
    TABLES
      data                         = <import_data_tab>
    EXCEPTIONS
      client_reference             = 1              " View is tied to another client
      foreign_lock                 = 2              " View/Table is locked by another user
      invalid_action               = 3              " ACTION contains invalid values
      no_clientindependent_auth    = 4              " no authorization for maintaining client-independent tables/v
      no_database_function         = 5              " Fct. mod. for data capture/disposal is missing
      no_show_auth                 = 6              " no display authorization
      no_tvdir_entry               = 7              " View/table is not entered in TVDIR
      no_upd_auth                  = 8              " no maintenance or display authorization
      only_show_allowed            = 9              " Display, but not maintain authorization
      system_failure               = 10             " System locking error
      unknown_field_in_dba_sellist = 11             " Selection table contains unknown field
      view_not_found               = 12             " View/table not found in DDIC
      OTHERS                       = 13.
  IF sy-subrc <> 0 .
    IF sy-msgid IS NOT INITIAL AND
       sy-msgty IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      MESSAGE |Error: { sy-subrc }| TYPE 'I'.
    ENDIF.
  ENDIF.

  PERFORM f_build_container.

ENDFORM.

FORM f_build_container.
*— Declarations for dynamic data
  DATA gt_data TYPE REF TO data.

  CLEAR: salv.

  IF doc_container IS NOT INITIAL.
*    CLEAR doc_container.
    doc_container->free(
      EXCEPTIONS
        cntl_error        = 1                " CNTL_ERROR
        cntl_system_error = 2                " CNTL_SYSTEM_ERROR
        OTHERS            = 3 ).
    IF sy-subrc <> 0 AND
      sy-msgid IS NOT INITIAL AND
      sy-msgty IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

  CHECK ptable IS NOT INITIAL.

*— Create dynamic internal table
  CREATE DATA gt_data TYPE TABLE OF (ptable).
  ASSIGN gt_data->* TO <ft_tab>.

  SELECT SINGLE tabclass
  FROM dd02l INTO @DATA(lv_tabname)
  WHERE tabname = @ptable. "AND tabclass = 'TRANSP'.

  IF lv_tabname = 'VIEW'.
    PERFORM get_data_mv.
  ELSE.
    SELECT *
      FROM (ptable) INTO TABLE <ft_tab>.
  ENDIF.

**********************************************************************
  " create a reference variable with structure of DB table
  DATA: lr_data TYPE REF TO data.
  CREATE DATA lr_data TYPE (ptable) .
  " assign the data reference to a field symbol
  ASSIGN lr_data->* TO FIELD-SYMBOL(<fs_struc1>).
  " Get the list of fields from the database table name
  DATA(lo_struct_descr1) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( <fs_struc1>  ) ).
  DATA(lt_fieldlist1) = lo_struct_descr1->get_ddic_field_list( ).
  " Get a list of key fields from the list of all fields
  " Build Range Table for Key Field

  DATA lt_key_fields_range TYPE RANGE OF fdname.

  lt_key_fields_range = VALUE #( FOR ls_fieldlist1 IN lt_fieldlist1 WHERE ( keyflag = abap_true )
                          sign   = 'I' option = 'EQ'
                         ( low    = CONV fdname( ls_fieldlist1-fieldname ) ) ).


* From the list of all components get the key components
  DATA(lt_components) = lo_struct_descr1->get_components( ).
  DATA(lt_key_compnents) = lt_components.

  DELETE lt_key_compnents WHERE NOT name IN lt_key_fields_range.
  DELETE lt_components WHERE name IN lt_key_fields_range.

  " Create an internal table with all the fields in the DB along with another field which points to the key field
  TRY.
      DATA(lo_key_fields_struct) = cl_abap_structdescr=>create( lt_key_compnents ).
      INSERT INITIAL LINE INTO lt_components ASSIGNING FIELD-SYMBOL(<fs_components>) INDEX 1.
      <fs_components>-name = 'KEY'.
      <fs_components>-as_include = abap_true.
      <fs_components>-type = lo_key_fields_struct.

      DATA(lo_table_data_struct) = cl_abap_structdescr=>create( lt_components ).
      DATA(lo_table_data_table) = cl_abap_tabledescr=>create( lo_table_data_struct ).
    CATCH cx_sy_struct_creation.  "
  ENDTRY.

  " build internal table 1 <ft_t1> which has the key field record
  DATA lo_t_data1          TYPE REF TO data.
  CREATE DATA lo_t_data1 TYPE HANDLE lo_table_data_table.
  ASSIGN lo_t_data1->* TO <ft_tab_key>.
  <ft_tab_key> = <ft_tab>.

  FIELD-SYMBOLS: <fs_key_stru> TYPE data.
  DATA lo_data_key TYPE REF TO data.
  CREATE DATA lo_data_key TYPE HANDLE lo_key_fields_struct.
  ASSIGN lo_data_key->* TO <ft_line_key>.

**********************************************************************

  CREATE OBJECT doc_container
    EXPORTING
      repid = sy-repid
      dynnr = sy-dynnr
      side  = doc_container->dock_at_bottom
*     extension = 325
      ratio = 95.

  IF salv IS INITIAL.
    TRY.
*— Create Instance
        CALL METHOD cl_salv_table=>factory
          EXPORTING
            r_container    = doc_container
            container_name = 'CONTAINER'
          IMPORTING
            r_salv_table   = salv
          CHANGING
            t_table        = <ft_tab>.
      CATCH cx_salv_msg.                                "#EC NO_HANDLER
    ENDTRY.

    salv->get_display_settings( )->set_list_header( |{ lines( <ft_tab> ) }| & | Records from table | & |{ ptable }| ).
    salv->get_columns( )->set_optimize( abap_true ).
    salv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ). "Allow single row Selection"

*— Display ALV \Output
    salv->display( ).
  ENDIF.
ENDFORM.

FORM get_data_mv.
  DATA lv_view_name TYPE dd02v-tabname.

  lv_view_name = ptable.

  CALL FUNCTION 'VIEW_GET_DATA'
    EXPORTING
      view_name              = lv_view_name      " Name of the View/Table to be Edited
      with_authority_check   = abap_true
    TABLES
*     dba_sellist            =                  " Database Access Selection Conditions
      data                   = <ft_tab>
*    CHANGING
*     org_crit_inst          =                  " Organizational criterion
    EXCEPTIONS
      no_viewmaint_tool      = 1
      no_authority           = 2
      no_auth_for_sel        = 3
      data_access_restricted = 4
      no_functiongroup       = 5
      OTHERS                 = 6.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

FORM f_export_to_pc.
  DATA:
    filename          TYPE string.

*  IF <ft_tab> IS NOT INITIAL.
  PERFORM pickup_path_file CHANGING filename.
  IF filename IS NOT INITIAL.
    filename = filename && '\' && ptable && sy-sysid && sy-datum && '.CSV'.

* Get trailing blank
    cl_gui_frontend_services=>gui_download(
         EXPORTING filename     = filename
                   filetype     = 'ASC'
                   write_field_separator = 'X'
         CHANGING  data_tab     = <ft_tab> ).


  ENDIF.
*  ENDIF.

ENDFORM.

FORM pickup_path_file CHANGING filepath.
  cl_gui_frontend_services=>directory_browse(
     EXPORTING window_title    = 'Browse Path to download'
               initial_folder = 'C:\temp1'
     CHANGING selected_folder = filepath ).

ENDFORM.

FORM f_validate_table.
  IF  ptable IS INITIAL.
    MESSAGE 'Select table Name to be uploaded.' TYPE 'S'.
  ELSE.

*— Upload only Tables in customer namespace
    IF ptable+0(1) NE 'Z' AND ptable+0(1) NE 'Y'.
      MESSAGE 'Only tables in customer namespace can be uploaded.' TYPE 'S'. "'E'.
    ENDIF.

*— Only transparent tables can be uploaded
    SELECT SINGLE tabname
    FROM dd02l INTO @DATA(lv_tabname)
    WHERE tabname = @ptable AND
          tabclass = 'TRANSP' OR
          tabclass = 'VIEW'.
    IF sy-subrc NE 0.
      MESSAGE 'Only transparent tables can be uploaded.' TYPE 'S'. "'E'.
    ENDIF.
  ENDIF.
ENDFORM.
