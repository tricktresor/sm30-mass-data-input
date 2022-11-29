REPORT ztrcktrsr_sm30_given_data.

TYPES: BEGIN OF _text,
         line TYPE c LENGTH 1000,
       END OF _text,
       _text_tab TYPE STANDARD TABLE OF _text WITH DEFAULT KEY.

PARAMETERS p_demo RADIOBUTTON GROUP a DEFAULT 'X'.
PARAMETERS p_clpb RADIOBUTTON GROUP a.


START-OF-SELECTION.

  DATA(import_table) = CONV tabname( 'ZMVDIMP' ).
  IF p_demo = abap_true.
    DATA(import_data_csv) = VALUE _text_tab(
        ( line = '100;123;1000;6600' )
        ( line = '100;333;1000;6600' )
        ( line = '100;56;3000;2200' )
        ).
    DATA(delimiter) = ';'.
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
  DATA(import_data_struc) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( 'ZMVDIMP' ) ).
  DATA(vimflagtab_struc) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( 'VIMFLAGTAB' ) ).

  DATA(maint_struc_components) = import_data_struc->get_components( ).
  APPEND LINES OF vimflagtab_struc->get_components( ) TO maint_struc_components.

  DATA(import_maint_struc) = cl_abap_structdescr=>create( maint_struc_components ).


  DATA(import_data_table) = cl_abap_tabledescr=>create( p_line_type  = import_maint_struc ).

  CREATE DATA import_data_struc_ref TYPE HANDLE import_maint_struc.
  ASSIGN import_data_struc_ref->* TO <import_data_line>.


  CREATE DATA import_data_table_ref TYPE HANDLE import_data_table.
  ASSIGN import_data_table_ref->* TO <import_data_tab>.


  LOOP AT import_data_csv INTO DATA(csv_line).
    CLEAR <import_data_line>.
    SPLIT csv_line AT delimiter INTO TABLE DATA(import_data_values).
    LOOP AT import_data_values INTO DATA(value).
      ASSIGN COMPONENT sy-tabix OF STRUCTURE <import_data_line> TO FIELD-SYMBOL(<field>).
      <field> = value.
    ENDLOOP.
    ASSIGN COMPONENT 'ACTION' OF STRUCTURE <import_data_line> TO FIELD-SYMBOL(<action>).
    <action> = 'N'.
    APPEND <import_data_line> TO <import_data_tab>.
  ENDLOOP.


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
  IF sy-subrc <> 0.
    MESSAGE |Error: { sy-subrc }| TYPE 'I'.
  ENDIF.
