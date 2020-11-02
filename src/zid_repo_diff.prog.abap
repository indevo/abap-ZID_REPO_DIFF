*---------------------------------------------------------------------*
*                                                                     *
* Report  ZID_REPO_DIFF                                               *
*                                                                     *
*---------------------------------------------------------------------*
*                                                                     *
*  Remote comparison of repository objects                            *
*                                                                     *
*  Author: Jacek Kopcinski (INDEVO.PL)                                *
*  LinkedIn: pl.linkedin.com/in/jkopcinski                            *
*  Date: 03.07.2017                                                   *
*                                                                     *
* See FM TRINT_COMP_VERSION_EDITOR                                    *
*                                                                     *
*---------------------------------------------------------------------*

REPORT  zid_repo_diff.

"$. Region DEFINITIONS (CLASSES, TYPES, GLOBAL VARS)
CLASS gcl_obj_checker DEFINITION DEFERRED.
*----------------------------------------------------------------------*
* TABLES                                                               *
*----------------------------------------------------------------------*

TABLES:
  sci_dynp, sfpverscform.

*----------------------------------------------------------------------*
* TYPES                                                                *
*----------------------------------------------------------------------*
TYPE-POOLS:
  svrs2.

TYPES:

  ygv_repo_obj_status TYPE i,

BEGIN OF ygs_object,
  pgmid TYPE e071-pgmid,
  object TYPE e071-object,
  obj_name TYPE e071-obj_name,
  devclass TYPE tadir-devclass,
  author TYPE tadir-author,
  parent_object TYPE e071-object,
  parent_obj_name TYPE e071-obj_name,
END OF ygs_object,

BEGIN OF ygs_free_obj,
  type TYPE sci_trobj,
  range TYPE scit_objn,
END OF ygs_free_obj,
ygts_free_objs TYPE SORTED TABLE OF ygs_free_obj WITH UNIQUE KEY type,

BEGIN OF ygs_outtab.
        INCLUDE TYPE ygs_object.
TYPES:
  status TYPE ygv_repo_obj_status,
  status_icon TYPE icon_l2,
  status_txt TYPE char20,
  log_link TYPE text10,
  checker TYPE REF TO gcl_obj_checker,
END OF ygs_outtab,

  ygt_request_task TYPE STANDARD TABLE OF e070,
  ygt_tr_objects TYPE STANDARD TABLE OF e071,
  ygt_objects TYPE STANDARD TABLE OF ygs_object,
  ygt_outtab TYPE STANDARD TABLE OF ygs_outtab.

*----------------------------------------------------------------------*
* CLASS DEFINITIONS                                                    *
*----------------------------------------------------------------------*
CLASS gcl_system DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS:
      get_system
        IMPORTING iv_name TYPE tmssysnam
        RETURNING value(oo_sys) TYPE REF TO gcl_system,
      class_constructor.
    METHODS:
      get_name RETURNING value(ov_name) TYPE tmssysnam,
      get_rfc_dest RETURNING value(ov_dest) TYPE rfcdest.
  PRIVATE SECTION.
    CLASS-DATA:
      lt_systems TYPE tmscsyslst_typ.
    DATA:
      lv_name TYPE tmssysnam,
      lv_rfc_dest TYPE rfc_dest.
ENDCLASS.                    "gcl_system DEFINITION

*----------------------------------------------------------------------*
*       CLASS gcl_logger DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_logger DEFINITION.
  PUBLIC SECTION.
    METHODS:
      show_log,
      add_info IMPORTING iv_msg TYPE clike,
      add_warning IMPORTING iv_msg TYPE clike,
      add_error IMPORTING iv_msg TYPE clike.

  PRIVATE SECTION.
    TYPE-POOLS:
      slis, icon.
    TYPES:
      BEGIN OF yls_log,
        type TYPE bapiret2-type,
        icon TYPE balimsgty,
        message TYPE baltmsg,
      END OF yls_log,
      ylt_log TYPE STANDARD TABLE OF yls_log WITH DEFAULT KEY.

    DATA:
      lt_log TYPE ylt_log.

    METHODS:
      add_log
        IMPORTING iv_type TYPE msgty iv_msg TYPE clike,
      get_icon
       IMPORTING iv_type TYPE msgty
       RETURNING value(ov_icon) TYPE balimsgty,
      prepare_fieldcat CHANGING ct_fieldcat TYPE slis_t_fieldcat_alv.

ENDCLASS.                    "gcl_logger DEFINITION
*----------------------------------------------------------------------*
*       CLASS gcl_obj_checker DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_obj_checker DEFINITION ABSTRACT.
  PUBLIC SECTION.
    CONSTANTS:
      aco_status_unknown TYPE i VALUE 0,
      aco_status_equal TYPE i VALUE 1,
      aco_status_missing TYPE i VALUE 2,
      aco_status_changed TYPE i VALUE 3.
    CLASS-METHODS:
      get_status_txt
        IMPORTING iv_status TYPE ygv_repo_obj_status
        RETURNING value(ov_txt) TYPE string.
    METHODS:
      constructor,
      check RETURNING value(ov_status) TYPE ygv_repo_obj_status,
      show_object,
      show_parent_object,
      show_diff ABSTRACT,
      show_log,
      set_object IMPORTING is_obj TYPE ygs_object,
      set_target_sys IMPORTING io_sys TYPE REF TO gcl_system,
      get_target_sys RETURNING value(oo_sys) TYPE REF TO gcl_system,
      clone ABSTRACT RETURNING value(oo_checker) TYPE REF TO gcl_obj_checker.
  PROTECTED SECTION.
    DATA:
      ao_logger TYPE REF TO gcl_logger,
      as_obj TYPE ygs_object,
      as_obj_loc TYPE svrs2_versionable_object,
      as_obj_rem TYPE svrs2_versionable_object,
      as_delta TYPE  svrs2_xversionable_object,
      ao_target_sys TYPE REF TO gcl_system.
    METHODS:
      read_object
        IMPORTING
          iv_remote TYPE boolean OPTIONAL
        EXPORTING
          ov_missing TYPE boolean
          os_obj TYPE svrs2_versionable_object,
      clear_technical_fields
        IMPORTING it_tech_fields TYPE fieldname_tab
        CHANGING ct_tab TYPE STANDARD TABLE,
      clear_irrelevant_data
        CHANGING os_obj TYPE svrs2_versionable_object,
      replace_texts
        CHANGING os_obj TYPE svrs2_versionable_object,
      evaluate_delta ABSTRACT
        RETURNING value(ov_status) TYPE ygv_repo_obj_status,
      get_tab_change_info
        IMPORTING it_loc TYPE STANDARD TABLE
          it_rem TYPE STANDARD TABLE
        RETURNING value(rv_info) TYPE string,
      get_struct_change_info
        IMPORTING is_loc TYPE any
          is_rem TYPE any
        RETURNING value(rv_info) TYPE string.

ENDCLASS.                    "gcl_obj_checker DEFINITION
*----------------------------------------------------------------------*
*       CLASS gcl_dflt_checker DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_dflt_checker DEFINITION
  INHERITING FROM gcl_obj_checker.

  PUBLIC SECTION.
    METHODS:
      clone REDEFINITION,
      check REDEFINITION,
      show_diff REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      evaluate_delta REDEFINITION.
ENDCLASS.                    "gcl_dflt_checker DEFINITION
*----------------------------------------------------------------------*
*       CLASS gcl_code_checker DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_code_checker DEFINITION
  INHERITING FROM gcl_obj_checker ABSTRACT.

  PUBLIC SECTION.
    METHODS:
      show_diff REDEFINITION,
      set_ignore_comments IMPORTING iv_value TYPE boolean,
      set_ignore_indents IMPORTING iv_value TYPE boolean,
      set_ignore_empty_lines IMPORTING iv_value TYPE boolean,
      get_ignore_comments RETURNING value(ov_value) TYPE boolean,
      get_ignore_indents RETURNING value(ov_value) TYPE boolean,
      get_ignore_empty_lines RETURNING value(ov_value) TYPE boolean.

  PROTECTED SECTION.
    METHODS:
      normalize_code CHANGING ot_code TYPE abaptxt255_tab,
      remove_empty_lines CHANGING ot_code TYPE abaptxt255_tab.
    DATA:
      lv_ignore_indents TYPE boolean,
      lv_ignore_comments TYPE boolean,
      lv_ignore_empty_lines TYPE boolean.

ENDCLASS.                    "gcl_code_checker  DEFINITIO
*----------------------------------------------------------------------*
*       CLASS gcl_generic_code_checker DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_generic_code_checker DEFINITION
  INHERITING FROM gcl_code_checker.

  PUBLIC SECTION.
    METHODS:
      clone REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      evaluate_delta REDEFINITION,
      clear_irrelevant_data REDEFINITION.

ENDCLASS.                    "gcl_generic_code_checker DEFINITION

*----------------------------------------------------------------------*
*       CLASS gcl_class_section_checker  ABSTRACT DEFIN
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_class_section_checker DEFINITION ABSTRACT
  INHERITING FROM gcl_code_checker.

  PROTECTED SECTION.
    TYPES:
      ylt_methods TYPE STANDARD TABLE OF vseomethod,
      ylt_attributes TYPE STANDARD TABLE OF vseoattrib,
      ylt_parameters TYPE STANDARD TABLE OF vseoparam,
      ylt_excep TYPE STANDARD TABLE OF vseoexcep,
      ylt_redef TYPE STANDARD TABLE OF seoredef,
      ylt_types TYPE STANDARD TABLE OF vseotype_vrs.

    METHODS:
      clear_irrelevant_meth_data
        CHANGING ct_meth TYPE ylt_methods,
      clear_irrelevant_attr_data
        CHANGING ct_attr TYPE ylt_attributes,
      clear_irrelevant_param_data
        CHANGING ct_param TYPE ylt_parameters,
      clear_irrelevant_type_data
        CHANGING ct_type TYPE ylt_types,
      clear_irrelevant_redef_data
      CHANGING ct_redef TYPE ylt_redef,
      clear_irrelevant_excep_data
      CHANGING ct_excep TYPE ylt_excep.

ENDCLASS.                    "gcl_class_section_checker  ABSTRACT DEFIN
*----------------------------------------------------------------------*
*       CLASS gcl_cpub_checker  DEFINITIO
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_cpub_checker DEFINITION
  INHERITING FROM gcl_class_section_checker.

  PUBLIC SECTION.
    METHODS:
      clone REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      clear_irrelevant_data REDEFINITION,
      evaluate_delta REDEFINITION.

ENDCLASS.                    "gcl_cpub_checker  DEFINITIO
*----------------------------------------------------------------------*
*       CLASS gcl_cpro_checker  DEFINITIO
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_cpro_checker DEFINITION
  INHERITING FROM gcl_class_section_checker.

  PUBLIC SECTION.
    METHODS:
      clone REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      clear_irrelevant_data REDEFINITION,
      evaluate_delta REDEFINITION.

ENDCLASS.                    "gcl_cpro_checker  DEFINITIO
*----------------------------------------------------------------------*
*       CLASS gcl_cpri_checker  DEFINITIO
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_cpri_checker DEFINITION
  INHERITING FROM gcl_class_section_checker.

  PUBLIC SECTION.
    METHODS:
      clone REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      clear_irrelevant_data REDEFINITION,
      evaluate_delta REDEFINITION.

ENDCLASS.                    "gcl_cpri_checker  DEFINITIO
*----------------------------------------------------------------------*
*       CLASS gcl_clsd_checker  DEFINITIO
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_clsd_checker DEFINITION
  INHERITING FROM gcl_code_checker.

  PUBLIC SECTION.
    METHODS:
      clone REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      clear_irrelevant_data REDEFINITION,
      evaluate_delta REDEFINITION.

ENDCLASS.                    "gcl_clsd_checker  DEFINITIO
*----------------------------------------------------------------------*
*       CLASS gcl_intf_checker  DEFINITIO
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_intf_checker DEFINITION
  INHERITING FROM gcl_code_checker.

  PUBLIC SECTION.
    METHODS:
      clone REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      clear_irrelevant_data REDEFINITION,
      evaluate_delta REDEFINITION.

ENDCLASS.                    "gcl_intf_checker  DEFINITIO
*----------------------------------------------------------------------*
*       CLASS gcl_func_checker  DEFINITIO
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_func_checker DEFINITION
  INHERITING FROM gcl_generic_code_checker.

  PUBLIC SECTION.
    METHODS:
      clone REDEFINITION,
      show_diff REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      evaluate_delta REDEFINITION.

ENDCLASS.                    "gcl_func_checker  DEFINITIO
*----------------------------------------------------------------------*
*       CLASS gcl_rept_checker DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_rept_checker DEFINITION
  INHERITING FROM gcl_obj_checker.

  PUBLIC SECTION.
    METHODS:
      clone REDEFINITION,
      show_diff REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      clear_irrelevant_data REDEFINITION,
      evaluate_delta REDEFINITION.

ENDCLASS.                    "gcl_rept_checker DEFINITION

*----------------------------------------------------------------------*
*       CLASS gcl_dynp_checker  DEFINITIO
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_dynp_checker DEFINITION
  INHERITING FROM gcl_obj_checker.

  PUBLIC SECTION.
    METHODS:
      clone REDEFINITION,
      show_diff REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      clear_irrelevant_data REDEFINITION,
      evaluate_delta REDEFINITION.

ENDCLASS.                    "gcl_dynp_checker  DEFINITIO
*----------------------------------------------------------------------*
*       CLASS gcl_cuad_checker  DEFINITIO
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_cuad_checker DEFINITION
  INHERITING FROM gcl_obj_checker.

  PUBLIC SECTION.
    METHODS:
      clone REDEFINITION,
      show_diff REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      clear_irrelevant_data REDEFINITION,
      evaluate_delta REDEFINITION.

ENDCLASS.                    "gcl_cuad_checker  DEFINITIO
*----------------------------------------------------------------------*
*       CLASS gcl_tabt_checker  DEFINITIO
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_tabt_checker DEFINITION
  INHERITING FROM gcl_obj_checker.

  PUBLIC SECTION.
    METHODS:
      clone REDEFINITION,
      show_diff REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      clear_irrelevant_data REDEFINITION,
      evaluate_delta REDEFINITION.

ENDCLASS.                    "gcl_tabt_checker  DEFINITIO
*----------------------------------------------------------------------*
*       CLASS gcl_dted_checker  DEFINITIO
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_dted_checker DEFINITION
  INHERITING FROM gcl_obj_checker.

  PUBLIC SECTION.
    METHODS:
      clone REDEFINITION,
      show_diff REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      clear_irrelevant_data REDEFINITION,
      evaluate_delta REDEFINITION.

ENDCLASS.                    "gcl_dted_checker  DEFINITIO

*----------------------------------------------------------------------*
*       CLASS gcl_domd_checker  DEFINITIO
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_domd_checker DEFINITION
  INHERITING FROM gcl_obj_checker.

  PUBLIC SECTION.
    METHODS:
      clone REDEFINITION,
      show_diff REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      clear_irrelevant_data REDEFINITION,
      evaluate_delta REDEFINITION.

ENDCLASS.                    "gcl_domd_checker  DEFINITIO

*----------------------------------------------------------------------*
*       CLASS gcl_vied_checker  DEFINITIO
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_vied_checker DEFINITION
  INHERITING FROM gcl_obj_checker.

  PUBLIC SECTION.
    METHODS:
      clone REDEFINITION,
      show_diff REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      clear_irrelevant_data REDEFINITION,
      evaluate_delta REDEFINITION.
ENDCLASS.                    "gcl_vied_checker  DEFINITIO

*----------------------------------------------------------------------*
*       CLASS gcl_ttyd_checker  DEFINITIO
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_ttyd_checker DEFINITION
  INHERITING FROM gcl_obj_checker.

  PUBLIC SECTION.
    METHODS:
      clone REDEFINITION,
      show_diff REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      clear_irrelevant_data REDEFINITION,
      evaluate_delta REDEFINITION.
ENDCLASS.                    "gcl_ttyd_checker  DEFINITIO

*----------------------------------------------------------------------*
*       CLASS gcl_tabd_checker  DEFINITIO
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_tabd_checker DEFINITION
  INHERITING FROM gcl_obj_checker.

  PUBLIC SECTION.
    METHODS:
      clone REDEFINITION,
      show_diff REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      clear_irrelevant_data REDEFINITION,
      replace_texts REDEFINITION,
      evaluate_delta REDEFINITION.
ENDCLASS.                    "gcl_tabd_checker  DEFINITIO

CLASS gcl_sfpi_checker DEFINITION
  INHERITING FROM gcl_obj_checker.

  PUBLIC SECTION.
    METHODS:
      clone REDEFINITION,
      show_diff REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      clear_irrelevant_data REDEFINITION,
      evaluate_delta REDEFINITION.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS gcl_checker_manager DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_checker_manager DEFINITION.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF yls_obj_checker,
        object TYPE tadir-object,
        prototype TYPE REF TO gcl_obj_checker,
      END OF yls_obj_checker,
      ylt_obj_checker TYPE HASHED TABLE OF yls_obj_checker
        WITH UNIQUE KEY object.

    METHODS:
      register_checker
        IMPORTING
          iv_object TYPE tadir-object
          io_prototype TYPE REF TO gcl_obj_checker,
      get_checker_instance
        IMPORTING
          iv_object TYPE tadir-object
        RETURNING value(oo_checker) TYPE REF TO gcl_obj_checker,
      get_registered_types
        RETURNING value(ot_types) TYPE devtyrange.

  PRIVATE SECTION.
    DATA:
      at_obj_checker TYPE ylt_obj_checker.

ENDCLASS.                    "gcl_checker_manager DEFINITION


*----------------------------------------------------------------------*
*       CLASS gcl_main DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_main DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA:
      at_objects TYPE ygt_objects,
      ar_progs TYPE scit_prgnm,
      ar_fugrs TYPE scit_fugr,
      ar_pckg TYPE scit_devc,
      ar_clas TYPE scit_clas,
      ar_fpckg TYPE scit_devc,
      ar_fresp TYPE scit_resp,
      ar_tran TYPE RANGE OF trkorr,
      ar_ddics TYPE scit_tabl,
      ar_typps TYPE scit_typp,
      at_free_objs TYPE ygts_free_objs,
      av_only_diffs TYPE boolean,
      av_syst TYPE tmssysnam,
      av_ignore_indents TYPE boolean,
      av_ignore_comments TYPE boolean,
      av_ignore_empty_lines TYPE boolean,
      av_show_unsupported TYPE boolean.
    CLASS-METHODS:
      add_free_obj IMPORTING iv_type TYPE clike ir_range TYPE STANDARD TABLE,
      show_object IMPORTING iv_index TYPE i,
      show_diff IMPORTING iv_index TYPE i,
      show_log IMPORTING iv_index TYPE i,
      start.
  PRIVATE SECTION.
    TYPE-POOLS:
      slis, icon.
    CLASS-METHODS:
      show_progress
        IMPORTING iv_value TYPE i iv_max TYPE i is_obj TYPE ygs_object,
      register_checkers,
      build_object_set,
      build_dictionary_set,
      build_obj_set_for_type_group,
      build_obj_set_for_program,
      build_obj_set_for_package,
      build_obj_set_for_func_group,
      build_obj_set_for_class,
      build_obj_set_for_request,
      build_obj_set_for_free_objs,
      expand_objects,
      get_transport_tasks
        EXPORTING ot_tasks TYPE ygt_request_task,
      get_task_objects
        IMPORTING it_tasks TYPE ygt_request_task
        EXPORTING ot_tr_objects TYPE ygt_tr_objects,
      get_responsible
        IMPORTING iv_author TYPE tadir-author iv_cnam TYPE trdir-cnam iv_unam TYPE trdir-unam
        RETURNING value(ov_resp) TYPE tadir-author,
      fill_prog_responsible
        CHANGING ot_progs TYPE ygt_objects,
      check_objects,
      fill_status_icons,
      show_results,
      prepare_fieldcat CHANGING ct_fieldcat TYPE slis_t_fieldcat_alv.
    CLASS-DATA:
      ao_manager TYPE REF TO gcl_checker_manager,
      at_outtab TYPE ygt_outtab.

ENDCLASS.                    "gcl_main DEFINITION

*----------------------------------------------------------------------*
* CONSTANTS                                                            *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* FIELD SYMBOLS                                                        *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GLOBAL DATA                                                          *
*----------------------------------------------------------------------*
"$. Endregion

"$. Region CLASSES IMPLEMENTATIONS
*----------------------------------------------------------------------*
*       CLASS gcl_system IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_system IMPLEMENTATION.

  METHOD class_constructor.

    CALL FUNCTION 'TMS_CI_GET_SYSTEMLIST'
      EXPORTING
        iv_only_active = 'X'
      TABLES
        tt_syslst      = lt_systems.

  ENDMETHOD.                    "constructor

  METHOD get_system.
    DATA:
      ls_sys TYPE tmscsyslst.

    READ TABLE lt_systems INTO ls_sys
      WITH KEY sysnam = iv_name.

    CHECK sy-subrc = 0.

    CREATE OBJECT oo_sys.
    oo_sys->lv_name = iv_name.

    CONCATENATE 'TMSADM@' ls_sys-sysnam '.'
            ls_sys-domnam
            INTO oo_sys->lv_rfc_dest.

  ENDMETHOD.                    "constructor

  METHOD get_name.
    ov_name = lv_name.
  ENDMETHOD.                    "get_name

  METHOD get_rfc_dest.
    ov_dest = lv_rfc_dest.
  ENDMETHOD.                    "get_rfc_dest

ENDCLASS.                    "gcl_system IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS gcl_logger IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_logger IMPLEMENTATION.

  METHOD add_log.
    DATA:
      ls_log TYPE yls_log.

    ls_log-type = iv_type.
    ls_log-icon = get_icon( iv_type ).
    ls_log-message = iv_msg.
    APPEND ls_log TO lt_log.

  ENDMETHOD.                    "add_log

  METHOD add_info.
    add_log( iv_type = 'I' iv_msg = iv_msg ).
  ENDMETHOD.                    "add_info

  METHOD add_warning.
    add_log( iv_type = 'W' iv_msg = iv_msg ).
  ENDMETHOD.                    "add_warning

  METHOD add_error.
    add_log( iv_type = 'E' iv_msg = iv_msg ).
  ENDMETHOD.                    "add_error

  METHOD get_icon.
    CASE iv_type.
      WHEN 'E'.
        ov_icon = icon_led_red.
      WHEN 'I' OR 'S'.
        ov_icon = icon_led_green.
      WHEN 'W'.
        ov_icon = icon_led_yellow.
      WHEN OTHERS.
        ov_icon = icon_status.
    ENDCASE.
  ENDMETHOD.                    "get_icon

  METHOD show_log.
    DATA:
      ls_layout TYPE slis_layout_alv,
      lt_fieldcat TYPE slis_t_fieldcat_alv.

    ls_layout-colwidth_optimize = 'X'.
    ls_layout-zebra = 'X'.

    prepare_fieldcat( CHANGING ct_fieldcat = lt_fieldcat ).

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program = 'ZID_REPO_DIFF'
        is_layout          = ls_layout
        it_fieldcat        = lt_fieldcat
      TABLES
        t_outtab           = lt_log.

  ENDMETHOD.                    "show_log

  METHOD prepare_fieldcat.
    DATA:
      ls_fieldcat TYPE slis_fieldcat_alv.

    CLEAR ls_fieldcat.
    ls_fieldcat-ref_tabname = 'BAL_S_SHOW'.
    ls_fieldcat-ref_fieldname = 'ICON_MSGTY'.
    ls_fieldcat-fieldname = 'ICON'.
    ls_fieldcat-outputlen = 3.
    ls_fieldcat-icon = 'X'.
    APPEND ls_fieldcat TO ct_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-ref_tabname = 'BAL_S_SHOW'.
    ls_fieldcat-ref_fieldname = 'T_MSG'.
    ls_fieldcat-fieldname = 'MESSAGE'.
    APPEND ls_fieldcat TO ct_fieldcat.

  ENDMETHOD.                    "prepare_fieldcat

ENDCLASS.                    "gcl_logger IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS gcl_obj_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_obj_checker IMPLEMENTATION.

  METHOD constructor.
    CREATE OBJECT ao_logger.
  ENDMETHOD.                    "constructor

  METHOD set_object.
    as_obj = is_obj.
  ENDMETHOD.                    "set_object

  METHOD set_target_sys.
    ao_target_sys = io_sys.
  ENDMETHOD.                    "set_target_sys

  METHOD get_target_sys.
    oo_sys = ao_target_sys.
  ENDMETHOD.                    "get_target_sys

  METHOD read_object.

    CLEAR: os_obj, ov_missing.

    os_obj-objtype = as_obj-object.
    os_obj-objname = as_obj-obj_name.
    os_obj-versno  = '00000'.
    IF iv_remote = 'X'.
      os_obj-destination = ao_target_sys->get_rfc_dest( ).
    ENDIF.

    CALL FUNCTION 'SVRS_GET_VERSION'
      CHANGING
        object              = os_obj
      EXCEPTIONS
        communication_error = 1
        system_error        = 2
        no_version          = 3
        version_unreadable  = 4
        OTHERS              = 5.

    IF sy-subrc <> 0.
      ov_missing = 'X'.
    ENDIF.

  ENDMETHOD.                    "read_object

  METHOD clear_technical_fields.
    DATA:
      ls_tech_field TYPE fieldname.

    FIELD-SYMBOLS:
      <row> TYPE any,
      <field> TYPE any.

    LOOP AT it_tech_fields INTO ls_tech_field.

      LOOP AT ct_tab ASSIGNING <row>.
        ASSIGN COMPONENT ls_tech_field OF STRUCTURE <row> TO <field>.
        IF sy-subrc = 0.
          CLEAR <field>.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.                    "clear_technical_fields

  METHOD clear_irrelevant_data.

    DATA:
      lv_type TYPE c,
      lv_count TYPE i.

    FIELD-SYMBOLS:
      <ls_part> TYPE any,
      <ls_row> TYPE any,
      <lt_table> TYPE table,
      <lv_value> TYPE any.

    ASSIGN COMPONENT os_obj-objtype OF STRUCTURE os_obj TO <ls_part>.
    CHECK sy-subrc = 0.

    ASSIGN COMPONENT 'MDLOG' OF STRUCTURE <ls_part> TO <lt_table>.
    IF sy-subrc = 0.
      REFRESH <lt_table>.
    ENDIF.

    DESCRIBE FIELD <ls_part> TYPE lv_type COMPONENTS lv_count.

    DO lv_count TIMES.
      ASSIGN COMPONENT sy-index OF STRUCTURE <ls_part> TO <lt_table>.
      CHECK sy-subrc = 0.
      LOOP AT <lt_table> ASSIGNING <ls_row>.
        ASSIGN COMPONENT 'AUTHOR' OF STRUCTURE <ls_row> TO <lv_value>.
        IF sy-subrc = 0.
          CLEAR <lv_value>.
        ENDIF.
        ASSIGN COMPONENT 'CREATEDON' OF STRUCTURE <ls_row> TO <lv_value>.
        IF sy-subrc = 0.
          CLEAR <lv_value>.
        ENDIF.
        ASSIGN COMPONENT 'CHANGEDBY' OF STRUCTURE <ls_row> TO <lv_value>.
        IF sy-subrc = 0.
          CLEAR <lv_value>.
        ENDIF.
        ASSIGN COMPONENT 'CHANGEDON' OF STRUCTURE <ls_row> TO <lv_value>.
        IF sy-subrc = 0.
          CLEAR <lv_value>.
        ENDIF.
      ENDLOOP.
    ENDDO.

  ENDMETHOD.                    "clear_irrelevant_data

  METHOD replace_texts.
  ENDMETHOD.                    "replace_texts

  METHOD check.
    DATA:
      lv_missing TYPE boolean.

    read_object( IMPORTING os_obj = as_obj_loc ).
    read_object(
      EXPORTING iv_remote = 'X'
      IMPORTING os_obj = as_obj_rem
        ov_missing = lv_missing
    ).

    IF lv_missing = 'X'.
      ov_status = aco_status_missing.
      RETURN.
    ENDIF.

    clear_irrelevant_data( CHANGING os_obj = as_obj_loc ).
    clear_irrelevant_data( CHANGING os_obj = as_obj_rem ).

    replace_texts( CHANGING os_obj = as_obj_loc ).
    replace_texts( CHANGING os_obj = as_obj_rem ).

    CALL FUNCTION 'SVRS_MAKE_OBJECT_DELTA'
      EXPORTING
        obj_old = as_obj_loc
        obj_new = as_obj_rem
      CHANGING
        delta   = as_delta
      EXCEPTIONS
        OTHERS  = 2.

    ov_status = evaluate_delta( ).

  ENDMETHOD.                    "check

  METHOD get_status_txt.

    CASE iv_status.
      WHEN aco_status_equal.
        ov_txt = 'Same'.
      WHEN aco_status_missing.
        ov_txt = 'Missing'.
      WHEN aco_status_changed.
        ov_txt = 'Changed'.
      WHEN OTHERS.
        ov_txt = 'Unknown'.
    ENDCASE.

  ENDMETHOD.                    "get_status_txt

  METHOD show_log.
    ao_logger->show_log( ).
  ENDMETHOD.                    "show_log

  METHOD show_object.

    CALL FUNCTION 'TR_OBJECT_JUMP_TO_TOOL'
      EXPORTING
        iv_pgmid          = as_obj-pgmid
        iv_object         = as_obj-object
        iv_obj_name       = as_obj-obj_name
        iv_action         = 'SHOW'
      EXCEPTIONS
        jump_not_possible = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      show_parent_object( ).
    ENDIF.
  ENDMETHOD.                    "show_object

  METHOD show_parent_object.
    CALL FUNCTION 'TR_OBJECT_JUMP_TO_TOOL'
      EXPORTING
        iv_pgmid          = as_obj-pgmid
        iv_object         = as_obj-parent_object
        iv_obj_name       = as_obj-parent_obj_name
        iv_action         = 'SHOW'
      EXCEPTIONS
        jump_not_possible = 1
        OTHERS            = 2.
  ENDMETHOD.

  METHOD get_tab_change_info.
    FIELD-SYMBOLS:
      <loc_line> TYPE any,
      <rem_line> TYPE any.

    IF lines( it_loc ) <> lines( it_rem ).
      rv_info = |Different number of lines in remote and local table|.
      RETURN.
    ENDIF.

    LOOP AT it_loc ASSIGNING <loc_line>.
      READ TABLE it_rem ASSIGNING <rem_line> INDEX sy-tabix.
      ASSERT sy-subrc = 0.
      IF <loc_line> <> <rem_line>.
        rv_info = |Row { sy-tabix } : { get_struct_change_info( is_loc = <loc_line> is_rem = <rem_line> ) }|.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_struct_change_info.
    DATA:
      lo_struct_descr TYPE REF TO cl_abap_structdescr,
      lv_components_count TYPE i,
      ls_comp TYPE abap_compdescr,
      lt_components TYPE abap_compdescr_tab.

    FIELD-SYMBOLS:
      <loc_value> TYPE any,
      <rem_value> TYPE any.

    lo_struct_descr ?= cl_abap_structdescr=>describe_by_data( is_loc ).
    lt_components = lo_struct_descr->components.

    lv_components_count = lines( lt_components ).
    DO lv_components_count TIMES.
      ASSIGN COMPONENT sy-index OF STRUCTURE is_loc TO <loc_value>.
      ASSERT sy-subrc = 0.
      ASSIGN COMPONENT sy-index OF STRUCTURE is_rem TO <rem_value>.
      IF <loc_value> <> <rem_value>.
        READ TABLE lt_components INTO ls_comp INDEX sy-index.
        ASSERT sy-subrc = 0.
        rv_info = |Field { ls_comp-name }: "{ <loc_value> }" (Local) <> "{ <rem_value> }" (Remote)|.
        RETURN.
      ENDIF.
    ENDDO.
  ENDMETHOD.

ENDCLASS.                    "gcl_obj_checker IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS gcl_dflt_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_dflt_checker IMPLEMENTATION.

  METHOD clone.
    DATA:
      lo_checker TYPE REF TO gcl_dflt_checker.

    CREATE OBJECT lo_checker.
    lo_checker->ao_target_sys = ao_target_sys.
    oo_checker = lo_checker.
  ENDMETHOD.                    "clone

  METHOD check.
    ov_status = aco_status_unknown.
  ENDMETHOD.                    "check

  METHOD evaluate_delta.
    ov_status = aco_status_unknown.
  ENDMETHOD.                    "evaluate_delta

  METHOD show_diff.
    MESSAGE 'Comparison not available for this type of object' TYPE 'I'.
  ENDMETHOD.                    "show_diff

ENDCLASS.                    "gcl_dflt_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS gcl_main IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_main IMPLEMENTATION.

  METHOD register_checkers.

    DATA:
      lo_syst TYPE REF TO gcl_system,
      lo_code_checker TYPE REF TO gcl_code_checker,
      lo_checker TYPE REF TO gcl_obj_checker.

    lo_syst = gcl_system=>get_system( av_syst ).

    CREATE OBJECT ao_manager.

    CREATE OBJECT lo_code_checker TYPE gcl_generic_code_checker.
    lo_code_checker->set_target_sys( lo_syst ).
    lo_code_checker->set_ignore_comments( av_ignore_comments ).
    lo_code_checker->set_ignore_indents( av_ignore_indents ).
    lo_code_checker->set_ignore_empty_lines( av_ignore_empty_lines ).
    ao_manager->register_checker( iv_object = 'REPS' io_prototype = lo_code_checker ).
    ao_manager->register_checker( iv_object = 'CINC' io_prototype = lo_code_checker ).
    ao_manager->register_checker( iv_object = 'METH' io_prototype = lo_code_checker ).
    ao_manager->register_checker( iv_object = 'TYPD' io_prototype = lo_code_checker ).

    CREATE OBJECT lo_code_checker TYPE gcl_clsd_checker.
    lo_code_checker->set_target_sys( lo_syst ).
    lo_code_checker->set_ignore_comments( av_ignore_comments ).
    lo_code_checker->set_ignore_indents( av_ignore_indents ).
    lo_code_checker->set_ignore_empty_lines( av_ignore_empty_lines ).
    ao_manager->register_checker( iv_object = 'CLSD' io_prototype = lo_code_checker ).

    CREATE OBJECT lo_code_checker TYPE gcl_cpri_checker.
    lo_code_checker->set_target_sys( lo_syst ).
    lo_code_checker->set_ignore_comments( av_ignore_comments ).
    lo_code_checker->set_ignore_indents( av_ignore_indents ).
    lo_code_checker->set_ignore_empty_lines( av_ignore_empty_lines ).
    ao_manager->register_checker( iv_object = 'CPRI' io_prototype = lo_code_checker ).

    CREATE OBJECT lo_code_checker TYPE gcl_cpro_checker.
    lo_code_checker->set_target_sys( lo_syst ).
    lo_code_checker->set_ignore_comments( av_ignore_comments ).
    lo_code_checker->set_ignore_indents( av_ignore_indents ).
    lo_code_checker->set_ignore_empty_lines( av_ignore_empty_lines ).
    ao_manager->register_checker( iv_object = 'CPRO' io_prototype = lo_code_checker ).

    CREATE OBJECT lo_code_checker TYPE gcl_cpub_checker.
    lo_code_checker->set_target_sys( lo_syst ).
    lo_code_checker->set_ignore_comments( av_ignore_comments ).
    lo_code_checker->set_ignore_indents( av_ignore_indents ).
    lo_code_checker->set_ignore_empty_lines( av_ignore_empty_lines ).
    ao_manager->register_checker( iv_object = 'CPUB' io_prototype = lo_code_checker ).

    CREATE OBJECT lo_code_checker TYPE gcl_func_checker.
    lo_code_checker->set_target_sys( lo_syst ).
    lo_code_checker->set_ignore_comments( av_ignore_comments ).
    lo_code_checker->set_ignore_indents( av_ignore_indents ).
    lo_code_checker->set_ignore_empty_lines( av_ignore_empty_lines ).
    ao_manager->register_checker( iv_object = 'FUNC' io_prototype = lo_code_checker ).

    CREATE OBJECT lo_checker TYPE gcl_rept_checker.
    lo_checker->set_target_sys( lo_syst ).
    ao_manager->register_checker( iv_object = 'REPT' io_prototype = lo_checker ).

    CREATE OBJECT lo_checker TYPE gcl_dynp_checker.
    lo_checker->set_target_sys( lo_syst ).
    ao_manager->register_checker( iv_object = 'DYNP' io_prototype = lo_checker ).

    CREATE OBJECT lo_checker TYPE gcl_cuad_checker.
    lo_checker->set_target_sys( lo_syst ).
    ao_manager->register_checker( iv_object = 'CUAD' io_prototype = lo_checker ).

    CREATE OBJECT lo_checker TYPE gcl_tabd_checker.
    lo_checker->set_target_sys( lo_syst ).
    ao_manager->register_checker( iv_object = 'TABD' io_prototype = lo_checker ).

    CREATE OBJECT lo_checker TYPE gcl_tabt_checker.
    lo_checker->set_target_sys( lo_syst ).
    ao_manager->register_checker( iv_object = 'TABT' io_prototype = lo_checker ).

    CREATE OBJECT lo_checker TYPE gcl_domd_checker.
    lo_checker->set_target_sys( lo_syst ).
    ao_manager->register_checker( iv_object = 'DOMD' io_prototype = lo_checker ).

    CREATE OBJECT lo_checker TYPE gcl_dted_checker.
    lo_checker->set_target_sys( lo_syst ).
    ao_manager->register_checker( iv_object = 'DTED' io_prototype = lo_checker ).

    CREATE OBJECT lo_checker TYPE gcl_vied_checker.
    lo_checker->set_target_sys( lo_syst ).
    ao_manager->register_checker( iv_object = 'VIED' io_prototype = lo_checker ).

    CREATE OBJECT lo_checker TYPE gcl_ttyd_checker.
    lo_checker->set_target_sys( lo_syst ).
    ao_manager->register_checker( iv_object = 'TTYD' io_prototype = lo_checker ).

    CREATE OBJECT lo_checker TYPE gcl_dflt_checker.
    lo_checker->set_target_sys( lo_syst ).
    ao_manager->register_checker( iv_object = 'DFLT' io_prototype = lo_checker ).

    CREATE OBJECT lo_checker TYPE gcl_sfpi_checker.
    lo_checker->set_target_sys( lo_syst ).
    ao_manager->register_checker( iv_object = 'SFPI' io_prototype = lo_checker ).

  ENDMETHOD.                    "register_checkers

  METHOD start.
    register_checkers( ).
    build_object_set( ).
    expand_objects( ).
    check_objects( ).
    show_results( ).
  ENDMETHOD.                    "start

  METHOD build_object_set.
    build_dictionary_set( ).
    build_obj_set_for_type_group( ).
    build_obj_set_for_program( ).
    build_obj_set_for_package( ).
    build_obj_set_for_func_group( ).
    build_obj_set_for_class( ).
    build_obj_set_for_request( ).
    build_obj_set_for_free_objs( ).
  ENDMETHOD.                    "create_object_set

  METHOD expand_objects.
    DATA:
      ls_e071 TYPE e071,
      ls_sub_obj TYPE vrso,
      ls_obj TYPE ygs_object,
      ls_ver_obj TYPE svrs2_versionable_object,
      ls_exp_obj TYPE ygs_object,
      lt_exp_objs TYPE ygt_objects,
      lt_sub_objs TYPE STANDARD TABLE OF vrso.

    LOOP AT at_objects INTO ls_obj.

      ls_e071-pgmid = ls_obj-pgmid.
      ls_e071-object = ls_obj-object.
      ls_e071-obj_name = ls_obj-obj_name.

      CALL FUNCTION 'SVRS_RESOLVE_E071_OBJ'
        EXPORTING
          e071_obj        = ls_e071
        TABLES
          obj_tab         = lt_sub_objs
        EXCEPTIONS
          not_versionable = 1
          endloop.

      LOOP AT lt_sub_objs INTO ls_sub_obj.

        CLEAR ls_ver_obj.
        ls_ver_obj-objtype = ls_sub_obj-objtype.
        ls_ver_obj-objname = ls_sub_obj-objname.
        ls_ver_obj-versno  = '00000'.

        CALL FUNCTION 'SVRS_GET_VERSION_LOCAL'
          CHANGING
            object              = ls_ver_obj
          EXCEPTIONS
            communication_error = 1
            system_error        = 2
            no_version          = 3
            version_unreadable  = 4
            OTHERS              = 5.

        CHECK sy-subrc = 0.

        CHECK av_show_unsupported = 'X' OR ls_sub_obj-objtype IN ao_manager->get_registered_types( ).
        ls_exp_obj-pgmid = 'LIMU'.
        ls_exp_obj-object = ls_sub_obj-objtype.
        ls_exp_obj-obj_name = ls_sub_obj-objname.
        ls_exp_obj-author = ls_obj-author.
        ls_exp_obj-devclass = ls_obj-devclass.
        IF ls_obj-parent_object IS INITIAL.
          ls_exp_obj-parent_object = ls_obj-object.
          ls_exp_obj-parent_obj_name = ls_obj-obj_name.
        ELSE.
          ls_exp_obj-parent_object = ls_obj-parent_object.
          ls_exp_obj-parent_obj_name = ls_obj-parent_obj_name.
        ENDIF.
        APPEND ls_exp_obj TO lt_exp_objs.
      ENDLOOP.

    ENDLOOP.

    at_objects = lt_exp_objs.

  ENDMETHOD.                    "expand_objects

  METHOD build_obj_set_for_program.

    DATA:
      ls_tadir TYPE tadir,
      lt_progs TYPE ygt_objects.

    CHECK ar_progs IS NOT INITIAL.

    SELECT pgmid object obj_name devclass author
      FROM tadir INNER JOIN trdir ON obj_name = trdir~name
      INTO TABLE lt_progs
      WHERE pgmid = 'R3TR'
        AND obj_name IN ar_progs
        AND object = 'PROG'
        AND devclass IN ar_fpckg.

    fill_prog_responsible( CHANGING ot_progs = lt_progs ).
    DELETE lt_progs WHERE author NOT IN ar_fresp.
    APPEND LINES OF lt_progs TO at_objects.

  ENDMETHOD.                    "build_progset

  METHOD build_dictionary_set.

    DATA:
      lt_dict_objs TYPE ygt_objects.

    CHECK ar_ddics IS NOT INITIAL.

    SELECT pgmid object obj_name devclass author
    FROM tadir INTO TABLE lt_dict_objs
      WHERE pgmid  = 'R3TR'
        AND object IN ('DTEL','TABL','VIEW','SQLT','TTYP')
        AND obj_name  IN ar_ddics
        AND devclass IN ar_fpckg
        AND author IN ar_fresp.

    APPEND LINES OF lt_dict_objs TO at_objects.

  ENDMETHOD.                    "build_tadirset

  METHOD build_obj_set_for_type_group.
    DATA:
      lt_type_grp_objs TYPE ygt_objects.

    CHECK ar_typps IS NOT INITIAL.

    SELECT pgmid object obj_name devclass author
    FROM tadir INTO TABLE lt_type_grp_objs
      WHERE pgmid  = 'R3TR'
        AND object = 'TYPE'
        AND obj_name  IN ar_typps
        AND devclass IN ar_fpckg
        AND author IN ar_fresp.

    APPEND LINES OF lt_type_grp_objs TO at_objects.

  ENDMETHOD.                    "build_obj_set_for_type_group

  METHOD build_obj_set_for_package.

    DATA:
      lt_pckg_objects TYPE ygt_objects.

    CHECK ar_pckg IS NOT INITIAL.

    SELECT pgmid object obj_name devclass author
      FROM tadir INTO TABLE lt_pckg_objects
      WHERE devclass IN ar_pckg
        AND author IN ar_fresp.

    APPEND LINES OF lt_pckg_objects TO at_objects.

  ENDMETHOD.                    "build_obj_set_for_func_package

  METHOD build_obj_set_for_func_group.
    DATA:
      ls_object TYPE ygs_object,
      ls_e071 TYPE e071,
      ls_sub_obj TYPE vrso,
      lt_sub_objs TYPE STANDARD TABLE OF vrso,
      ls_fugr TYPE ygs_object,
      lt_fugrs TYPE ygt_objects.

    CHECK ar_fugrs IS NOT INITIAL.

    SELECT pgmid object obj_name devclass author
    FROM tadir INTO TABLE lt_fugrs
      WHERE pgmid  = 'R3TR'
        AND object IN ('FUGR','FUGS','FUGX')
        AND obj_name  IN ar_fugrs
        AND devclass IN ar_fpckg
        AND author IN ar_fresp.

    APPEND LINES OF lt_fugrs TO at_objects.

  ENDMETHOD.                    "build_obj_set_for_func_group

  METHOD build_obj_set_for_class.
    DATA:
      ls_object TYPE ygs_object,
      ls_e071 TYPE e071,
      ls_sub_obj TYPE vrso,
      lt_sub_objs TYPE STANDARD TABLE OF vrso,
      ls_class TYPE ygs_object,
      lt_classes TYPE ygt_objects.

    CHECK ar_clas IS NOT INITIAL.

    SELECT pgmid object obj_name devclass author
    FROM tadir INTO TABLE lt_classes
      WHERE pgmid  = 'R3TR'
        AND object = 'CLAS'
        AND obj_name  IN ar_clas
        AND devclass IN ar_fpckg
        AND author IN ar_fresp.

    APPEND LINES OF lt_classes TO at_objects.

  ENDMETHOD.                    "build_obj_set_for_class

  METHOD build_obj_set_for_request.
    DATA:
      lt_tasks TYPE ygt_request_task,
      ls_tr_object TYPE e071,
      lt_tr_objects TYPE ygt_tr_objects,
      ls_object TYPE ygs_object,
      lt_objects TYPE ygt_objects,
      ls_tadir TYPE tadir.

    FIELD-SYMBOLS:
      <ls_tr_object> TYPE e071.

    get_transport_tasks( IMPORTING ot_tasks = lt_tasks ).
    get_task_objects( EXPORTING it_tasks = lt_tasks
                      IMPORTING ot_tr_objects = lt_tr_objects ).

    LOOP AT lt_tr_objects ASSIGNING <ls_tr_object>.
      CALL FUNCTION 'TR_CHECK_TYPE'
        EXPORTING
          wi_e071  = <ls_tr_object>
        IMPORTING
          we_tadir = ls_tadir.

      SELECT SINGLE * FROM tadir INTO ls_tadir
        WHERE pgmid = ls_tadir-pgmid
          AND object = ls_tadir-object
          AND obj_name = ls_tadir-obj_name.

      CHECK ls_tadir-author IN ar_fresp.

      ls_object-pgmid = <ls_tr_object>-pgmid.
      ls_object-object = <ls_tr_object>-object.
      ls_object-obj_name = <ls_tr_object>-obj_name.
      ls_object-parent_object = ls_tadir-object.
      ls_object-parent_obj_name = ls_tadir-obj_name.
      ls_object-devclass = ls_tadir-devclass.
      ls_object-author = ls_tadir-author.
      APPEND ls_object TO at_objects.

    ENDLOOP.

  ENDMETHOD.                    "build_obj_set_for_request

  METHOD build_obj_set_for_free_objs.
    DATA:
      ls_free_obj TYPE ygs_free_obj,
      lt_objs TYPE ygt_objects.

    CHECK at_free_objs IS NOT INITIAL.

    LOOP AT at_free_objs INTO ls_free_obj.
      SELECT pgmid object obj_name devclass author
      FROM tadir INTO TABLE lt_objs
        WHERE pgmid  = 'R3TR'
          AND object = ls_free_obj-type
          AND obj_name  IN ls_free_obj-range
          AND devclass IN ar_fpckg
          AND author IN ar_fresp.

      APPEND LINES OF lt_objs TO at_objects.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_transport_tasks.
    CHECK ar_tran IS NOT INITIAL.

    CLEAR ot_tasks[].

    SELECT * FROM e070
      INTO TABLE ot_tasks
      WHERE strkorr IN ar_tran OR trkorr IN ar_tran.

  ENDMETHOD.                    "get_transport_tasks

*----------------------------------------------------------------------*
* Gets objects from given task
*----------------------------------------------------------------------*
  METHOD get_task_objects.
    CHECK it_tasks[] IS NOT INITIAL.

    CLEAR ot_tr_objects[].

    SELECT * FROM e071
      INTO TABLE ot_tr_objects
      FOR ALL ENTRIES IN it_tasks
      WHERE trkorr = it_tasks-trkorr.

    SORT ot_tr_objects BY pgmid object obj_name.
    DELETE ADJACENT DUPLICATES FROM ot_tr_objects COMPARING pgmid object obj_name.

  ENDMETHOD.                    "get_task_objects

  METHOD get_responsible.

    " TADIR-Author
    IF iv_author <> space  AND
       iv_author <> 'SAP*' AND
       iv_author <> 'DDIC'.
      ov_resp = iv_author.
    ELSE.
      " TRDIR-Author
      IF iv_cnam <> space  AND
         iv_cnam <> 'SAP*' AND
         iv_cnam <> 'DDIC'.
        ov_resp = iv_cnam.
      ELSE.
        " TRDIR-Modifier
        IF iv_unam <> space.
          ov_resp = iv_unam.
        ELSE.
          ov_resp = '???'.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "get_responsible

  METHOD fill_prog_responsible.
    TYPES:
      BEGIN OF yls_prog_responsible,
        name TYPE trdir-name,
        cnam TYPE trdir-cnam,
        unam TYPE trdir-unam,
      END OF yls_prog_responsible.

    DATA:
      ls_prog_resp TYPE yls_prog_responsible,
      lt_prog_resp TYPE HASHED TABLE OF yls_prog_responsible
        WITH UNIQUE KEY name.

    FIELD-SYMBOLS:
      <ls_prog> TYPE ygs_object.

    CHECK ot_progs IS NOT INITIAL.

    SELECT name cnam unam INTO TABLE lt_prog_resp
      FROM trdir
      FOR ALL ENTRIES IN ot_progs
      WHERE name = ot_progs-obj_name(40).

    LOOP AT ot_progs ASSIGNING <ls_prog>.
      CLEAR ls_prog_resp.
      READ TABLE lt_prog_resp INTO ls_prog_resp
        WITH TABLE KEY name = <ls_prog>-obj_name.

      <ls_prog>-author = get_responsible(
          iv_author = <ls_prog>-author
          iv_cnam = ls_prog_resp-cnam
          iv_unam = ls_prog_resp-unam ).

    ENDLOOP.

  ENDMETHOD.                    "fill_prog_responsible

  METHOD show_progress.

    DATA:
      lv_text TYPE string,
      lv_percentage TYPE i.

    IF iv_max <> 0.
      lv_percentage = 100 * iv_value / iv_max.
    ELSE.
      lv_percentage = 100.
    ENDIF.

    MESSAGE s001(zid_repo)
      WITH iv_value iv_max is_obj-object is_obj-obj_name
      INTO lv_text.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_percentage
        text       = lv_text.

  ENDMETHOD.                    "show_progress

  METHOD check_objects.
    DATA:
      lv_size TYPE i,
      ls_outtab TYPE ygs_outtab,
      ls_obj TYPE ygs_object,
      lo_checker TYPE REF TO gcl_obj_checker.

    DESCRIBE TABLE at_objects LINES lv_size.

    LOOP AT at_objects INTO ls_obj.
      show_progress(
        iv_value = sy-tabix
        iv_max = lv_size
        is_obj = ls_obj ).
      MOVE-CORRESPONDING ls_obj TO ls_outtab.
      lo_checker = ao_manager->get_checker_instance( ls_obj-object ).
      lo_checker->set_object( ls_obj ).
      ls_outtab-checker = lo_checker.
      ls_outtab-status = lo_checker->check( ).
      ls_outtab-status_txt = lo_checker->get_status_txt( ls_outtab-status ).
      ls_outtab-log_link = 'More Info'.
      APPEND ls_outtab TO at_outtab.
    ENDLOOP.

    IF av_only_diffs = 'X'.
      DELETE at_outtab WHERE status = gcl_obj_checker=>aco_status_equal.
    ENDIF.

  ENDMETHOD.                    "compare_objects

  METHOD fill_status_icons.
    FIELD-SYMBOLS:
      <row> TYPE ygs_outtab.

    LOOP AT at_outtab ASSIGNING <row>.
      CASE <row>-status.
        WHEN gcl_obj_checker=>aco_status_unknown.
          <row>-status_icon = icon_light_out.
        WHEN gcl_obj_checker=>aco_status_equal.
          <row>-status_icon = icon_green_light.
        WHEN gcl_obj_checker=>aco_status_missing.
          <row>-status_icon = icon_red_light.
        WHEN gcl_obj_checker=>aco_status_changed.
          <row>-status_icon = icon_yellow_light.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.                    "fill_status_icons

  METHOD add_free_obj.
    DATA:
      ls_free_obj TYPE ygs_free_obj.

    CHECK ir_range IS NOT INITIAL.
    ls_free_obj-type = iv_type.
    ls_free_obj-range = ir_range.
    INSERT ls_free_obj INTO TABLE at_free_objs.
  ENDMETHOD.

  METHOD show_object.
    DATA:
      lo_checker TYPE REF TO gcl_obj_checker.
    FIELD-SYMBOLS:
      <row> TYPE ygs_outtab.

    READ TABLE at_outtab INDEX iv_index ASSIGNING <row>.

    lo_checker = <row>-checker.
    lo_checker->show_object( ).

  ENDMETHOD.                    "show_object

  METHOD show_diff.

    DATA:
      lo_checker TYPE REF TO gcl_obj_checker.
    FIELD-SYMBOLS:
      <row> TYPE ygs_outtab.

    READ TABLE at_outtab INDEX iv_index ASSIGNING <row>.

    lo_checker = <row>-checker.
    lo_checker->show_diff( ).

  ENDMETHOD.                    "show_diff

  METHOD show_log.

    DATA:
      lo_checker TYPE REF TO gcl_obj_checker.
    FIELD-SYMBOLS:
      <row> TYPE ygs_outtab.

    READ TABLE at_outtab INDEX iv_index ASSIGNING <row>.

    lo_checker = <row>-checker.
    lo_checker->show_log( ).

  ENDMETHOD.                    "show_diff

  METHOD show_results.
    DATA:
      ls_layout TYPE slis_layout_alv,
      lt_fieldcat TYPE slis_t_fieldcat_alv.

    ls_layout-colwidth_optimize = 'X'.
    ls_layout-zebra = 'X'.

    fill_status_icons( ).
    prepare_fieldcat( CHANGING ct_fieldcat = lt_fieldcat ).

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program      = 'ZID_REPO_DIFF'
        is_layout               = ls_layout
        it_fieldcat             = lt_fieldcat
        i_callback_user_command = 'USER_COMMAND'
      TABLES
        t_outtab                = at_outtab.

  ENDMETHOD.                    "show_results

  METHOD prepare_fieldcat.

    DATA:
      ls_fieldcat TYPE slis_fieldcat_alv.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'OBJECT'.
    ls_fieldcat-ref_tabname = 'TADIR'.
    APPEND ls_fieldcat TO ct_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'OBJ_NAME'.
    ls_fieldcat-ref_tabname = 'TADIR'.
    ls_fieldcat-hotspot = 'X'.
    APPEND ls_fieldcat TO ct_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'DEVCLASS'.
    ls_fieldcat-ref_tabname = 'TADIR'.
    APPEND ls_fieldcat TO ct_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'AUTHOR'.
    ls_fieldcat-ref_tabname = 'TADIR'.
    APPEND ls_fieldcat TO ct_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'PARENT_OBJECT'.
    ls_fieldcat-ref_tabname = 'TADIR'.
    ls_fieldcat-ref_fieldname = 'OBJECT'.
    ls_fieldcat-reptext_ddic = 'Par. Type'.
    ls_fieldcat-seltext_s = 'Parent Type'.
    ls_fieldcat-seltext_m = 'Parent Obj. Type'.
    ls_fieldcat-seltext_l = 'Parent Object Type'.
    APPEND ls_fieldcat TO ct_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'PARENT_OBJ_NAME'.
    ls_fieldcat-ref_tabname = 'TADIR'.
    ls_fieldcat-ref_fieldname = 'OBJ_NAME'.
    ls_fieldcat-reptext_ddic = 'Par. Obj. Name'.
    ls_fieldcat-seltext_s = ls_fieldcat-reptext_ddic.
    ls_fieldcat-seltext_m = 'Parent Obj. Name'.
    ls_fieldcat-seltext_l = 'Parent Object Name'.
    APPEND ls_fieldcat TO ct_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'STATUS_ICON'.
    ls_fieldcat-reptext_ddic = 'Obj. Status'.
    ls_fieldcat-seltext_s = 'Obj. Status'.
    ls_fieldcat-seltext_m = 'Object Status'.
    ls_fieldcat-seltext_l = 'Object Status'.
    ls_fieldcat-icon = 'X'.
    APPEND ls_fieldcat TO ct_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'STATUS_TXT'.
    ls_fieldcat-seltext_s = 'Status'.
    ls_fieldcat-seltext_m = ls_fieldcat-seltext_s.
    ls_fieldcat-seltext_l = ls_fieldcat-seltext_s.
    ls_fieldcat-hotspot = 'X'.
    APPEND ls_fieldcat TO ct_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'LOG_LINK'.
    ls_fieldcat-seltext_s = 'Log'.
    ls_fieldcat-seltext_m = ls_fieldcat-seltext_s.
    ls_fieldcat-seltext_l = ls_fieldcat-seltext_s.
    ls_fieldcat-hotspot = 'X'.
    APPEND ls_fieldcat TO ct_fieldcat.

  ENDMETHOD.                    "prepare_fieldcat

ENDCLASS.                    "gcl_main IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS gcl_code_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_code_checker IMPLEMENTATION.

  METHOD set_ignore_comments.
    lv_ignore_comments = iv_value.
  ENDMETHOD.                    "set_ignore_comments

  METHOD set_ignore_indents.
    lv_ignore_indents = iv_value.
  ENDMETHOD.                    "set_ignore_indents

  METHOD set_ignore_empty_lines.
    lv_ignore_empty_lines = iv_value.
  ENDMETHOD.

  METHOD get_ignore_comments.
    ov_value = lv_ignore_comments.
  ENDMETHOD.                    "get_ignore_comments

  METHOD get_ignore_indents.
    ov_value = lv_ignore_indents.
  ENDMETHOD.                    "get_ignore_indents

  METHOD get_ignore_empty_lines.
    ov_value = lv_ignore_empty_lines.
  ENDMETHOD.

  METHOD normalize_code.
    CALL FUNCTION 'SCWB_CREATE_NORMAL_FORM'
      EXPORTING
        iv_case            = 'U'
        iv_condense        = lv_ignore_indents
        iv_ignore_comments = lv_ignore_comments
      CHANGING
        ct_code            = ot_code.

    IF lv_ignore_empty_lines = abap_true.
      remove_empty_lines( CHANGING ot_code = ot_code ).
    ENDIF.

  ENDMETHOD.                    "normalize_code

  METHOD remove_empty_lines.
    DELETE ot_code WHERE line IS INITIAL.
  ENDMETHOD.

  METHOD show_diff.

    DATA:
      lv_system TYPE tmssysnam,
      lv_rfc_dest TYPE rfcdest.

    lv_system = ao_target_sys->get_name( ).
    lv_rfc_dest = ao_target_sys->get_rfc_dest( ).

    SUBMIT rsvrsrs3 AND RETURN
          WITH objname  = as_obj-obj_name
          WITH objnam2  = as_obj-obj_name
          WITH versno1  = '00000'
          WITH versno2  = '00000'
          WITH objtyp1  = as_obj-object
          WITH objtyp2  = as_obj-object
*          WITH infoln1a = ls_infoline1a
*          WITH infoln1b = ls_infoline1b
*          WITH infoln2a = ls_infoline2a
*          WITH infoln2b = ls_infoline2b
          WITH log_dest = lv_rfc_dest
          WITH rem_syst = lv_system.

  ENDMETHOD.                    "show

ENDCLASS.                    "gcl_code_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS gcl_generic_code_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_generic_code_checker IMPLEMENTATION.

  METHOD clone.
    DATA:
      lo_checker TYPE REF TO gcl_generic_code_checker.

    CREATE OBJECT lo_checker.
    lo_checker->ao_target_sys = ao_target_sys.
    lo_checker->lv_ignore_comments = lv_ignore_comments.
    lo_checker->lv_ignore_indents = lv_ignore_indents.
    lo_checker->lv_ignore_empty_lines = lv_ignore_empty_lines.
    oo_checker = lo_checker.
  ENDMETHOD.                    "clone

  METHOD clear_irrelevant_data.
    FIELD-SYMBOLS:
      <ls_object> TYPE any,
      <lt_code> TYPE abaptxt255_tab.

    super->clear_irrelevant_data( CHANGING os_obj = os_obj ).
    ASSIGN COMPONENT os_obj-objtype OF STRUCTURE os_obj TO <ls_object>.
    CHECK sy-subrc = 0.

    ASSIGN COMPONENT 'ABAPTEXT' OF STRUCTURE <ls_object> TO <lt_code>.
    CHECK sy-subrc = 0.

    normalize_code( CHANGING ot_code = <lt_code> ).

  ENDMETHOD.                    "clear_irrelevant_data

  METHOD evaluate_delta.
    DATA:
      lv_code_struct_name TYPE string.

    FIELD-SYMBOLS:
      <ls_object_delta> TYPE any,
      <lt_abap_delta> TYPE vxabapt255_tab.

    ov_status = aco_status_unknown.

    ASSIGN COMPONENT as_delta-objtype OF STRUCTURE as_delta TO <ls_object_delta>.
    CHECK sy-subrc = 0.

    ASSIGN COMPONENT 'ABAPTEXT' OF STRUCTURE <ls_object_delta> TO <lt_abap_delta>.
    CHECK sy-subrc = 0.

    READ TABLE <lt_abap_delta> INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "ABAP code"' ).
    ELSE.
      ov_status = aco_status_equal.
    ENDIF.
  ENDMETHOD.                    "evaluate_delta

ENDCLASS.                    "gcl_generic_code_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS gcl_class_section_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_class_section_checker IMPLEMENTATION.

  METHOD clear_irrelevant_meth_data.
    DATA:
      lt_tech_fields TYPE fieldname_tab.

    SPLIT 'DESCRIPT EDITORDER R3RELEASE' AT space
      INTO TABLE lt_tech_fields.
    clear_technical_fields(
      EXPORTING it_tech_fields = lt_tech_fields
      CHANGING ct_tab = ct_meth ).
  ENDMETHOD.                    "clear_irrelevant_meth_data

  METHOD clear_irrelevant_attr_data.
    DATA:
      lt_tech_fields TYPE fieldname_tab.

    SPLIT 'DESCRIPT EDITORDER R3RELEASE' AT space
      INTO TABLE lt_tech_fields.
    clear_technical_fields(
      EXPORTING it_tech_fields = lt_tech_fields
      CHANGING ct_tab = ct_attr ).
  ENDMETHOD.                    "clear_irrelevant_attr_data

  METHOD clear_irrelevant_param_data.
    DATA:
      lt_tech_fields TYPE fieldname_tab.

    SPLIT 'DESCRIPT EDITORDER' AT space
      INTO TABLE lt_tech_fields.

    clear_technical_fields(
      EXPORTING it_tech_fields = lt_tech_fields
      CHANGING ct_tab = ct_param ).

  ENDMETHOD.                    "clear_irrelevant_param_data

  METHOD clear_irrelevant_type_data.
    DATA:
      lt_tech_fields TYPE fieldname_tab.

    SPLIT 'R3RELEASE' AT space
      INTO TABLE lt_tech_fields.

    clear_technical_fields(
      EXPORTING it_tech_fields = lt_tech_fields
      CHANGING ct_tab = ct_type ).

  ENDMETHOD.

  METHOD clear_irrelevant_excep_data.
    DATA:
      lt_tech_fields TYPE fieldname_tab.

    SPLIT 'DESCRIPT EDITORDER' AT space
      INTO TABLE lt_tech_fields.

    clear_technical_fields(
      EXPORTING it_tech_fields = lt_tech_fields
      CHANGING ct_tab = ct_excep ).

  ENDMETHOD.

  METHOD clear_irrelevant_redef_data.
    DATA:
      lt_tech_fields TYPE fieldname_tab.

    SPLIT 'EXPOSURE' AT space
      INTO TABLE lt_tech_fields.

    clear_technical_fields(
      EXPORTING it_tech_fields = lt_tech_fields
      CHANGING ct_tab = ct_redef ).

  ENDMETHOD.

ENDCLASS.                    "gcl_class_section_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS gcl_cpub_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_cpub_checker IMPLEMENTATION.

  METHOD clone.
    DATA:
      lo_checker TYPE REF TO gcl_cpub_checker.

    CREATE OBJECT lo_checker.
    lo_checker->ao_target_sys = ao_target_sys.
    lo_checker->lv_ignore_comments = lv_ignore_comments.
    lo_checker->lv_ignore_indents = lv_ignore_indents.
    oo_checker = lo_checker.
  ENDMETHOD.                    "clone

  METHOD clear_irrelevant_data.
    super->clear_irrelevant_data( CHANGING os_obj = os_obj ).
    clear_irrelevant_param_data( CHANGING ct_param = os_obj-cpub-param ).
    clear_irrelevant_meth_data( CHANGING ct_meth = os_obj-cpub-meth ).
    clear_irrelevant_attr_data( CHANGING ct_attr = os_obj-cpub-attr ).
    clear_irrelevant_type_data( CHANGING ct_type = os_obj-cpub-type ).
    clear_irrelevant_excep_data( CHANGING ct_excep = os_obj-cpub-excep ).
    clear_irrelevant_redef_data( CHANGING ct_redef = os_obj-cpub-redef ).
    normalize_code( CHANGING ot_code = os_obj-cpub-reps ).
  ENDMETHOD.                    "clear_irrelevant_data

  METHOD evaluate_delta.

    READ TABLE as_delta-cpub-reps INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "ABAP code"' ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-cpub-event INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Events" (EVENT)' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-cpub-event it_rem = as_obj_rem-cpub-event ) ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-cpub-excep INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Exceptions" (EXCEP)' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-cpub-excep it_rem = as_obj_rem-cpub-excep ) ).
      RETURN.
    ENDIF.

*    READ TABLE as_delta-cpub-imprl INDEX 1 TRANSPORTING NO FIELDS.
*    IF sy-subrc = 0.
*      ov_status = aco_status_changed.
*      ao_logger->add_warning( 'Difference in "Implementation-based instance relationship" (IMPRL)' ).
*      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-cpub-imprl it_rem = as_obj_rem-cpub-imprl ) ).
*      RETURN.
*    ENDIF.

    READ TABLE as_delta-cpub-meth INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Methods" (METHOD)' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-cpub-meth it_rem = as_obj_rem-cpub-meth ) ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-cpub-mtrel INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Meta relationship"' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-cpub-mtrel it_rem = as_obj_rem-cpub-mtrel ) ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-cpub-param INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Parameters"' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-cpub-param it_rem = as_obj_rem-cpub-param ) ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-cpub-redef INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Redefined methods"' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-cpub-redef it_rem = as_obj_rem-cpub-redef ) ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-cpub-type INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Types"' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-cpub-type it_rem = as_obj_rem-cpub-type ) ).
      RETURN.
    ENDIF.

*    READ TABLE as_delta-cpub-typep INDEX 1 TRANSPORTING NO FIELDS.
*    IF sy-subrc = 0.
*      ov_status = aco_status_changed.
*      ao_logger->add_warning( 'Difference in "Type group application" (TYPEP)' ).
*      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-cpub-typep it_rem = as_obj_rem-cpub-typep ) ).
*      RETURN.
*    ENDIF.

    READ TABLE as_delta-cpub-alias INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Aliases" (ALIAS)' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-cpub-alias it_rem = as_obj_rem-cpub-alias ) ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-cpub-attr INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Attributes" (ATTR)' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-cpub-attr it_rem = as_obj_rem-cpub-attr ) ).
    ELSE.
      ov_status = aco_status_equal.
    ENDIF.
  ENDMETHOD.                    "evaluate_delta

ENDCLASS.                    "gcl_cpub_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS gcl_cpro_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_cpro_checker IMPLEMENTATION.

  METHOD clone.
    DATA:
      lo_checker TYPE REF TO gcl_cpro_checker.

    CREATE OBJECT lo_checker.
    lo_checker->ao_target_sys = ao_target_sys.
    lo_checker->lv_ignore_comments = lv_ignore_comments.
    lo_checker->lv_ignore_indents = lv_ignore_indents.
    oo_checker = lo_checker.
  ENDMETHOD.                    "clone

  METHOD clear_irrelevant_data.
    super->clear_irrelevant_data( CHANGING os_obj = os_obj ).
    clear_irrelevant_param_data( CHANGING ct_param = os_obj-cpro-param ).
    clear_irrelevant_meth_data( CHANGING ct_meth = os_obj-cpro-meth ).
    clear_irrelevant_attr_data( CHANGING ct_attr = os_obj-cpro-attr ).
    clear_irrelevant_type_data( CHANGING ct_type = os_obj-cpro-type ).
    clear_irrelevant_excep_data( CHANGING ct_excep = os_obj-cpro-excep ).
    clear_irrelevant_redef_data( CHANGING ct_redef = os_obj-cpro-redef ).
    normalize_code( CHANGING ot_code = os_obj-cpro-reps ).
  ENDMETHOD.                    "clear_irrelevant_data

  METHOD evaluate_delta.

    READ TABLE as_delta-cpro-reps INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "ABAP code"' ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-cpro-typep INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Type group application"' ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-cpro-alias INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Aliases" (ALIAS)' ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-cpro-attr INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Attributes" (ATTR)' ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-cpro-event INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Events" (EVENT)' ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-cpro-excep INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Exceptions" (EXCEP)' ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-cpro-meth INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Methods" (METHOD)' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-cpro-meth it_rem = as_obj_rem-cpro-meth ) ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-cpro-param INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Parameters" (PARAM)' ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-cpro-redef INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Redefined methods" (REDEF)' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-cpro-redef it_rem = as_obj_rem-cpro-redef ) ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-cpro-type INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Types" (TYPE)' ).
    ELSE.
      ov_status = aco_status_equal.
    ENDIF.
  ENDMETHOD.                    "evaluate_delta
ENDCLASS.                    "gcl_cpro_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS gcl_cpri_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_cpri_checker IMPLEMENTATION.

  METHOD clone.
    DATA:
      lo_checker TYPE REF TO gcl_cpri_checker.

    CREATE OBJECT lo_checker.
    lo_checker->ao_target_sys = ao_target_sys.
    lo_checker->lv_ignore_comments = lv_ignore_comments.
    lo_checker->lv_ignore_indents = lv_ignore_indents.
    oo_checker = lo_checker.
  ENDMETHOD.                    "clone

  METHOD clear_irrelevant_data.
    super->clear_irrelevant_data( CHANGING os_obj = os_obj ).
    clear_irrelevant_param_data( CHANGING ct_param = os_obj-cpri-param ).
    clear_irrelevant_meth_data( CHANGING ct_meth = os_obj-cpri-meth ).
    clear_irrelevant_attr_data( CHANGING ct_attr = os_obj-cpri-attr ).
    clear_irrelevant_type_data( CHANGING ct_type = os_obj-cpri-type ).
    clear_irrelevant_excep_data( CHANGING ct_excep = os_obj-cpri-excep ).
    normalize_code( CHANGING ot_code = os_obj-cpri-reps ).
  ENDMETHOD.                    "clear_irrelevant_data

  METHOD evaluate_delta.
    READ TABLE as_delta-cpri-reps INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "ABAP code" (REPS)' ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-cpri-alias INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Aliases" (ALIAS)' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-cpri-alias it_rem = as_obj_rem-cpri-alias ) ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-cpri-attr INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Attributes" (ATTR)' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-cpri-attr it_rem = as_obj_rem-cpri-attr ) ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-cpri-event INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Events" (EVENT)' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-cpri-event it_rem = as_obj_rem-cpri-event ) ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-cpri-excep INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Exceptions" (EXCEP)' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-cpri-excep it_rem = as_obj_rem-cpri-excep ) ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-cpri-meth INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Methods" (METH)' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-cpri-meth it_rem = as_obj_rem-cpri-meth ) ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-cpri-param INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Parameters" (PARAM)' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-cpri-param it_rem = as_obj_rem-cpri-param ) ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-cpri-type INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Types" (TYPE)' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-cpri-type it_rem = as_obj_rem-cpri-type ) ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-cpri-typep INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Type group application" (TYPEP)' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-cpri-typep it_rem = as_obj_rem-cpri-typep ) ).
    ELSE.
      ov_status = aco_status_equal.
    ENDIF.
  ENDMETHOD.                    "evaluate_delta

ENDCLASS.                    "gcl_cpri_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS gcl_clsd_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_clsd_checker IMPLEMENTATION.

  METHOD clone.
    DATA:
      lo_checker TYPE REF TO gcl_clsd_checker.

    CREATE OBJECT lo_checker.
    lo_checker->ao_target_sys = ao_target_sys.
    lo_checker->lv_ignore_comments = lv_ignore_comments.
    lo_checker->lv_ignore_indents = lv_ignore_indents.
    oo_checker = lo_checker.
  ENDMETHOD.                    "clone

  METHOD clear_irrelevant_data.
    super->clear_irrelevant_data( CHANGING os_obj = os_obj ).
    normalize_code( CHANGING ot_code = os_obj-clsd-reps ).
  ENDMETHOD.                    "clear_irrelevant_data

  METHOD evaluate_delta.
    READ TABLE as_delta-clsd-reps INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "ABAP code"' ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-clsd-frnds INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Friend relationship"' ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-clsd-mtrel INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Meta relationship"' ).
    ELSE.
      ov_status = aco_status_equal.
    ENDIF.
  ENDMETHOD.                    "evaluate_delta

ENDCLASS.                    "gcl_clsd_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS gcl_intf_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_intf_checker IMPLEMENTATION.

  METHOD clone.
    DATA:
      lo_checker TYPE REF TO gcl_intf_checker.

    CREATE OBJECT lo_checker.
    lo_checker->ao_target_sys = ao_target_sys.
    lo_checker->lv_ignore_comments = lv_ignore_comments.
    lo_checker->lv_ignore_indents = lv_ignore_indents.
    oo_checker = lo_checker.
  ENDMETHOD.                    "clone

  METHOD clear_irrelevant_data.
    super->clear_irrelevant_data( CHANGING os_obj = os_obj ).
    normalize_code( CHANGING ot_code = os_obj-intf-reps ).
    normalize_code( CHANGING ot_code = os_obj-intf-preps ).
  ENDMETHOD.                    "clear_irrelevant_data

  METHOD evaluate_delta.

    READ TABLE as_delta-intf-reps INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "ABAP code"' ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-intf-preps INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "ABAP code"' ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-intf-alias INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Aliases" (ALIAS)' ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-intf-attr INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Attributes" (ATTR)' ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-intf-compr INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Interface composition"' ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-intf-event INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Events" (EVENT)' ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-intf-excep INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Exceptions" (EXCEP)' ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-intf-intf INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Interface" (INTF)' ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-intf-meth INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Methods" (METHOD)' ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-intf-typep INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Type group application"' ).
    ELSE.
      ov_status = aco_status_equal.
    ENDIF.
  ENDMETHOD.                    "evaluate_delta

ENDCLASS.                    "gcl_intf_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS gcl_func_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_func_checker IMPLEMENTATION.

  METHOD clone.
    DATA:
      lo_checker TYPE REF TO gcl_func_checker.

    CREATE OBJECT lo_checker.
    lo_checker->ao_target_sys = ao_target_sys.
    lo_checker->lv_ignore_comments = lv_ignore_comments.
    lo_checker->lv_ignore_indents = lv_ignore_indents.
    oo_checker = lo_checker.
  ENDMETHOD.                    "clone

  METHOD evaluate_delta.
    ov_status = super->evaluate_delta( ).
    CHECK ov_status = aco_status_equal.

    READ TABLE as_delta-func-enlfd INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Additional Attributes (ENLFD)"' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-func-enlfd it_rem = as_obj_rem-func-enlfd ) ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-func-fupar INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Function Module Interface (FUPAR)"' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-func-fupar it_rem = as_obj_rem-func-fupar ) ).
    ELSE.
      ov_status = aco_status_equal.
    ENDIF.

  ENDMETHOD.                    "evaluate_delta

  METHOD show_diff.

    DATA:
      lv_system TYPE tmssysnam,
      lv_rfc_dest TYPE rfcdest.

    lv_system = ao_target_sys->get_name( ).
    lv_rfc_dest = ao_target_sys->get_rfc_dest( ).

    SUBMIT rsvrsfu3 AND RETURN
          WITH objname  = as_obj-obj_name
          WITH objnam2  = as_obj-obj_name
          WITH versno1  = '00000'
          WITH versno2  = '00000'
          WITH objtyp1  = as_obj-object
          WITH objtyp2  = as_obj-object
*          WITH infoln1a = ls_infoline1a
*          WITH infoln1b = ls_infoline1b
*          WITH infoln2a = ls_infoline2a
*          WITH infoln2b = ls_infoline2b
          WITH log_dest = lv_rfc_dest
          WITH rem_syst = lv_system.

  ENDMETHOD.                    "show

ENDCLASS.                    "gcl_func_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS gcl_tabd_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_tabd_checker IMPLEMENTATION.

  METHOD clone.
    DATA:
      lo_checker TYPE REF TO gcl_tabd_checker.

    CREATE OBJECT lo_checker.
    lo_checker->ao_target_sys = ao_target_sys.
    oo_checker = lo_checker.
  ENDMETHOD.                    "clone

  METHOD clear_irrelevant_data.
    DATA:
      lt_tech_fields TYPE fieldname_tab,
      ls_dd02v TYPE dd02v.

    super->clear_irrelevant_data( CHANGING os_obj = os_obj ).
    SPLIT 'AS4USER AS4DATE AS4TIME TABCLASS ' &
    'SHLPEXI MASTERLANG MAINFLAG WRONGCL EXCLASS'
    AT space INTO TABLE lt_tech_fields.

    READ TABLE os_obj-tabd-dd02v INDEX 1 INTO ls_dd02v.

    IF ls_dd02v-tabclass <> 'POOL' AND
       ls_dd02v-tabclass <> 'CLUSTER'.
      APPEND 'SQLTAB' TO lt_tech_fields.
    ENDIF.
    IF ls_dd02v-tabclass <> 'APPEND'.
      APPEND 'APPTAB' TO lt_tech_fields.
    ENDIF.
    " Only for database tables
    IF ls_dd02v-tabclass <> 'TRANSP' AND
       ls_dd02v-tabclass <> 'POOL' AND
       ls_dd02v-tabclass <> 'CLUSTER'.
      APPEND 'BUFFERED' TO lt_tech_fields.
      APPEND 'MAINFLAGTXT' TO lt_tech_fields.
      APPEND 'CONTFLAG' TO lt_tech_fields.
    ENDIF.

    clear_technical_fields(
      EXPORTING it_tech_fields = lt_tech_fields
      CHANGING ct_tab = os_obj-tabd-dd02v ).

    SPLIT 'TABNAME DDLANGUAGE POSITION MANDATORY ADMINFIELD ' &
          'INTTYPE INTLEN PRECFIELD CONROUT DDTEXT ' &
          'DOMNAME SHLPORIGIN TABLETYPE DEPTH COMPTYPE DEFFDNAME'
             AT ' ' INTO TABLE lt_tech_fields.

    clear_technical_fields(
      EXPORTING it_tech_fields = lt_tech_fields
      CHANGING ct_tab = os_obj-tabd-dd03v ).

  ENDMETHOD.                    "clear_irrelevant_data

  METHOD replace_texts.

    DATA:
      ls_dd02tv TYPE dd02tv,
      ls_dd03tv TYPE dd03tv.

    FIELD-SYMBOLS:
      <dd02v> TYPE dd02v,
      <dd03v> TYPE dd03v.

    LOOP AT os_obj-tabd-dd02v ASSIGNING <dd02v> WHERE ddlanguage <> sy-langu.
      READ TABLE os_obj-tabd-dd02tv INTO ls_dd02tv
        WITH KEY tabname = <dd02v>-tabname
        ddlanguage = sy-langu.
      CHECK sy-subrc = 0.
      MOVE-CORRESPONDING ls_dd02tv TO <dd02v>.
    ENDLOOP.

    LOOP AT os_obj-tabd-dd03v ASSIGNING <dd03v> WHERE ddlanguage <> sy-langu.
      READ TABLE os_obj-tabd-dd03tv INTO ls_dd03tv
        WITH KEY tabname = <dd03v>-tabname
        fieldname = <dd03v>-fieldname
        ddlanguage = sy-langu.
      CHECK sy-subrc = 0.
      MOVE-CORRESPONDING ls_dd03tv TO <dd03v>.
    ENDLOOP.

  ENDMETHOD.                    "replace_texts

  METHOD evaluate_delta.
    "General Attributes
    READ TABLE as_delta-tabd-dd02v INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "General attributes"' ).
      RETURN.
    ENDIF.
    "Fields Attributes
    READ TABLE as_delta-tabd-dd03v INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Fields"' ).
    ELSE.
      ov_status = aco_status_equal.
    ENDIF.

  ENDMETHOD.                    "evaluate_delta

  METHOD show_diff.
    DATA:
      lv_system TYPE tmssysnam,
      lv_rfc_dest TYPE rfcdest.

    lv_system = ao_target_sys->get_name( ).
    lv_rfc_dest = ao_target_sys->get_rfc_dest( ).

    SUBMIT radvvtb2 AND RETURN
          WITH objname  = as_obj-obj_name
          WITH objnam2  = as_obj-obj_name
          WITH versno1  = '00000'
          WITH versno2  = '00000'
          WITH objtyp1  = 'TABL'
          WITH objtyp2  = 'TABL'
          WITH log_dest = lv_rfc_dest
          WITH rem_syst = lv_system.

  ENDMETHOD.                    "show_diff

ENDCLASS.                    "gcl_tabd_checker IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS gcl_rept_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_rept_checker IMPLEMENTATION.

  METHOD clone.
    DATA:
      lo_checker TYPE REF TO gcl_rept_checker.

    CREATE OBJECT lo_checker.
    lo_checker->ao_target_sys = ao_target_sys.
    oo_checker = lo_checker.
  ENDMETHOD.                    "clone

  METHOD clear_irrelevant_data.
    FIELD-SYMBOLS:
      <textpool> TYPE textpoolt.

    super->clear_irrelevant_data( CHANGING os_obj = os_obj ).
    DELETE os_obj-rept-textpool WHERE id = 'R' OR lang <> sy-langu.
    LOOP AT os_obj-rept-textpool ASSIGNING <textpool>.
      CLEAR <textpool>-length.
    ENDLOOP.
  ENDMETHOD.                    "clear_irrelevant_data

  METHOD evaluate_delta.
    READ TABLE as_delta-rept-textpool INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Text pool"' ).
    ELSE.
      ov_status = aco_status_equal.
    ENDIF.
  ENDMETHOD.                    "evaluate_delta

  METHOD show_diff.

    DATA:
      lv_system TYPE tmssysnam,
      lv_rfc_dest TYPE rfcdest.

    lv_system = ao_target_sys->get_name( ).
    lv_rfc_dest = ao_target_sys->get_rfc_dest( ).

    SUBMIT rsvrstco AND RETURN
          WITH objname  = as_obj-obj_name
          WITH objnam2  = as_obj-obj_name
          WITH versno1  = '00000'
          WITH versno2  = '00000'
          WITH objtyp1  = as_obj-object
          WITH objtyp2  = as_obj-object
          WITH log_dest = lv_rfc_dest
          WITH rem_syst = lv_system.

  ENDMETHOD.                    "show_diff

ENDCLASS.                    "gcl_rept_checker  IMPLEMENENTATI
*----------------------------------------------------------------------*
*       CLASS gcl_dynp_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_dynp_checker IMPLEMENTATION.

  METHOD clone.
    DATA:
      lo_checker TYPE REF TO gcl_dynp_checker.

    CREATE OBJECT lo_checker.
    lo_checker->ao_target_sys = ao_target_sys.
    oo_checker = lo_checker.
  ENDMETHOD.                    "clone

  METHOD clear_irrelevant_data.
    FIELD-SYMBOLS:
      <d020s> TYPE d020s,
      <d021s> TYPE d021se.

    super->clear_irrelevant_data( CHANGING os_obj = os_obj ).
    DELETE os_obj-dynp-d020t WHERE lang <> sy-langu.
    DELETE os_obj-dynp-d021t WHERE lang <> sy-langu.
    LOOP AT os_obj-dynp-d020s ASSIGNING <d020s>.
      CLEAR <d020s>-bzmx.
      CLEAR <d020s>-bzbr.
      CLEAR <d020s>-dgen.
      CLEAR <d020s>-tgen.
    ENDLOOP.

    LOOP AT os_obj-dynp-d021s ASSIGNING <d021s>.
      CLEAR <d021s>-auth.
    ENDLOOP.

  ENDMETHOD.                    "clear_irrelevant_data

  METHOD evaluate_delta.

    " General Attributes
    READ TABLE as_delta-dynp-d020s INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "General Attributes" (D020S)' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-dynp-d020s it_rem = as_obj_rem-dynp-d020s ) ).
      RETURN.
    ENDIF.

    " Screen Short Description
    READ TABLE as_delta-dynp-d020t INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Screen Short Description" (DO20T)' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-dynp-d020t it_rem = as_obj_rem-dynp-d020t ) ).
      RETURN.
    ENDIF.

    " Screen fields
    READ TABLE as_delta-dynp-d021s INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Screen fields" (D021S)' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-dynp-d021s it_rem = as_obj_rem-dynp-d021s ) ).
      RETURN.
    ENDIF.

    " Screen Key Word Texts
    READ TABLE as_delta-dynp-d021t INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Screen Key Word Texts" (DO21T)' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-dynp-d021t it_rem = as_obj_rem-dynp-d021t ) ).
      RETURN.
    ENDIF.

    " Screen flow logic
    READ TABLE as_delta-dynp-d022s INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Screen flow logic" (D022S)' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-dynp-d022s it_rem = as_obj_rem-dynp-d022s ) ).
      RETURN.
    ENDIF.

    " Screen Parameters
    READ TABLE as_delta-dynp-d023s INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Screen Parameters" (D023S)' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-dynp-d023s it_rem = as_obj_rem-dynp-d023s ) ).
    ELSE.
      ov_status = aco_status_equal.
    ENDIF.

  ENDMETHOD.                    "evaluate_delta

  METHOD show_diff.
    DATA:
      lv_system TYPE tmssysnam,
      lv_rfc_dest TYPE rfcdest.

    lv_system = ao_target_sys->get_name( ).
    lv_rfc_dest = ao_target_sys->get_rfc_dest( ).

    SUBMIT rsdynl13 AND RETURN
          WITH objname  = as_obj-obj_name
          WITH objnam2  = as_obj-obj_name
          WITH versno1  = '00000'
          WITH versno2  = '00000'
          WITH objtyp1  = 'DYNP'
          WITH objtyp2  = 'DYNP'
          WITH log_dest = lv_rfc_dest
          WITH rem_syst = lv_system.

  ENDMETHOD.                    "show_diff

ENDCLASS.                    "gcl_dynp_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS gcl_cuad_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_cuad_checker IMPLEMENTATION.

  METHOD clone.
    DATA:
      lo_checker TYPE REF TO gcl_cuad_checker.

    CREATE OBJECT lo_checker.
    lo_checker->ao_target_sys = ao_target_sys.
    oo_checker = lo_checker.
  ENDMETHOD.                    "clone

  METHOD clear_irrelevant_data.
    super->clear_irrelevant_data( CHANGING os_obj = os_obj ).
    DELETE os_obj-cuad-vtexts_40 WHERE sprsl <> sy-langu.
  ENDMETHOD.                    "clear_irrelevant_data

  METHOD evaluate_delta.

    " Menu bars
    READ TABLE as_delta-cuad-vact INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Menu bars"' ).
      RETURN.
    ENDIF.

    " Menu bars (4.0 onwards)
    READ TABLE as_delta-cuad-vact_40 INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Menu bars (4.0 onwards)"' ).
      RETURN.
    ENDIF.

    " Internal administration data
    READ TABLE as_delta-cuad-vadm_40 INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Internal administration data"' ).
      ao_logger->add_warning( get_tab_change_info( it_loc = as_obj_loc-cuad-vadm_40 it_rem = as_obj_rem-cuad-vadm_40 ) ).
      RETURN.
    ENDIF.

    " Function Key Settings
    READ TABLE as_delta-cuad-vatt INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Function Key Settings"' ).
      RETURN.
    ENDIF.

    " Fixed Functions on Application Toolbars
    READ TABLE as_delta-cuad-vbiv_40 INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Fixed Functions on Application Toolbars"' ).
      RETURN.
    ENDIF.

    " Pushbuttons
    READ TABLE as_delta-cuad-vbut INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Pushbuttons"' ).
      RETURN.
    ENDIF.

    " Pushbuttons (4.0 onwards)
    READ TABLE as_delta-cuad-vbut_40 INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Pushbuttons (4.0 onwards)"' ).
      RETURN.
    ENDIF.

    " Context menu
    READ TABLE as_delta-cuad-vctx_40 INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Context menu"' ).
      RETURN.
    ENDIF.

    " Technical texts
    READ TABLE as_delta-cuad-vdoc INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Technical texts"' ).
      RETURN.
    ENDIF.

    "  Attributes (4.0 onwards)
    READ TABLE as_delta-cuad-vdoc_40 INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Attributes (4.0 onwards)"' ).
      RETURN.
    ENDIF.

    " Dynamic function texts
    READ TABLE as_delta-cuad-vfdn INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Dynamic function texts"' ).
      RETURN.
    ENDIF.

    " Functions with icons
    READ TABLE as_delta-cuad-vfin INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Functions with icons"' ).
      RETURN.
    ENDIF.

    " Function text
    READ TABLE as_delta-cuad-vfun INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Function text"' ).
      RETURN.
    ENDIF.

    " Function Texts Language-Independent (from 7.0)
    READ TABLE as_delta-cuad-vfun_40 INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Function Texts Language-Independent (from 7.0)"' ).
      RETURN.
    ENDIF.

    " Include menus
    READ TABLE as_delta-cuad-vinc INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Include menus"' ).
      RETURN.
    ENDIF.

    " Administration info
    READ TABLE as_delta-cuad-vlst INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Administration info"' ).
      RETURN.
    ENDIF.

    " Dynamic menu texts
    READ TABLE as_delta-cuad-vmdn INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Dynamic menu texts"' ).
      RETURN.
    ENDIF.

    " Menus
    READ TABLE as_delta-cuad-vmen INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Menus"' ).
      RETURN.
    ENDIF.

    " Menu structure (4.0 onwards)
    READ TABLE as_delta-cuad-vmen_40 INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Menu structure (4.0 onwards)"' ).
      RETURN.
    ENDIF.

    " Menu texts
    READ TABLE as_delta-cuad-vmtx INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Menu texts"' ).
      RETURN.
    ENDIF.

    " Language-Independent Menu Table (7.0 onwards)
    READ TABLE as_delta-cuad-vmtx_40 INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Language-Independent Menu Table (7.0 onwards)"' ).
      RETURN.
    ENDIF.

    " Function key assignment
    READ TABLE as_delta-cuad-vpfk INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Function key assignment"' ).
      RETURN.
    ENDIF.

    " Function key assignments (4.0 onwards)
    READ TABLE as_delta-cuad-vpfk_40 INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Function key assignments (4.0 onwards)"' ).
      RETURN.
    ENDIF.

    " Active functions
    READ TABLE as_delta-cuad-vset INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Active functions"' ).
      RETURN.
    ENDIF.

    " Status functions (4.0 onwards)
    READ TABLE as_delta-cuad-vset_40 INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Status functions (4.0 onwards)"' ).
      RETURN.
    ENDIF.

    " Status list
    READ TABLE as_delta-cuad-vsta INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Status list"' ).
      RETURN.
    ENDIF.

    " Status (4.0 onwards)
    READ TABLE as_delta-cuad-vsta_40 INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Status (4.0 onwards)"' ).
      RETURN.
    ENDIF.

    " Status texts
    READ TABLE as_delta-cuad-vstx INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Status texts"' ).
      RETURN.
    ENDIF.

    " Standard toolbar
    READ TABLE as_delta-cuad-vsym INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Standard toolbar"' ).
      RETURN.
    ENDIF.

    " Texts
    READ TABLE as_delta-cuad-vtexts_40 INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Texts"' ).
      RETURN.
    ENDIF.

    " GUI Title
    READ TABLE as_delta-cuad-vtit INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "GUI Title"' ).
      RETURN.
    ENDIF.

    " Title codes (4.0)
    READ TABLE as_delta-cuad-vtit_40 INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Title codes (4.0)"' ).
    ELSE.
      ov_status = aco_status_equal.
    ENDIF.

  ENDMETHOD.                    "evaluate_delta

  METHOD show_diff.
    DATA:
      lv_system TYPE tmssysnam,
      lv_rfc_dest TYPE rfcdest.

    lv_system = ao_target_sys->get_name( ).
    lv_rfc_dest = ao_target_sys->get_rfc_dest( ).

    SUBMIT sapmseuk AND RETURN
          WITH objname  = as_obj-obj_name
          WITH objnam2  = as_obj-obj_name
          WITH versno1  = '00000'
          WITH versno2  = '00000'
          WITH objtyp1  = as_obj-object
          WITH objtyp2  = as_obj-object
          WITH log_dest = lv_rfc_dest
          WITH rem_syst = lv_system.

  ENDMETHOD.                    "show_diff

ENDCLASS.                    "gcl_cuad_checker IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS gcl_tabt_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_tabt_checker IMPLEMENTATION.

  METHOD clone.
    DATA:
      lo_checker TYPE REF TO gcl_tabt_checker.

    CREATE OBJECT lo_checker.
    lo_checker->ao_target_sys = ao_target_sys.
    oo_checker = lo_checker.
  ENDMETHOD.                    "clone

  METHOD clear_irrelevant_data.
    DATA:
      lt_tech_fields TYPE fieldname_tab.

    super->clear_irrelevant_data( CHANGING os_obj = os_obj ).
    SPLIT 'AS4USER AS4DATE AS4TIME'
          AT space INTO TABLE lt_tech_fields.

    clear_technical_fields(
      EXPORTING it_tech_fields = lt_tech_fields
      CHANGING ct_tab = os_obj-tabt-dd09v ).

  ENDMETHOD.                    "clear_irrelevant_data

  METHOD evaluate_delta.
    READ TABLE as_delta-tabt-dd09v INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "General attributes"' ).
    ELSE.
      ov_status = aco_status_equal.
    ENDIF.
  ENDMETHOD.                    "evaluate_delta

  METHOD show_diff.

    DATA:
      lv_system TYPE tmssysnam,
      lv_rfc_dest TYPE rfcdest.

    lv_system = ao_target_sys->get_name( ).
    lv_rfc_dest = ao_target_sys->get_rfc_dest( ).

    SUBMIT rsvrstt2 AND RETURN
          WITH objname  = as_obj-obj_name
          WITH objnam2  = as_obj-obj_name
          WITH versno1  = '00000'
          WITH versno2  = '00000'
          WITH objtyp1  = 'DTEL'
          WITH objtyp2  = 'DTEL'
          WITH log_dest = lv_rfc_dest
          WITH rem_syst = lv_system.

  ENDMETHOD.                    "show_diff

ENDCLASS.                    "gcl_tabt_checker IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS gcl_dted_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_dted_checker IMPLEMENTATION.

  METHOD clone.
    DATA:
      lo_checker TYPE REF TO gcl_dted_checker.

    CREATE OBJECT lo_checker.
    lo_checker->ao_target_sys = ao_target_sys.
    oo_checker = lo_checker.
  ENDMETHOD.                    "clone

  METHOD clear_irrelevant_data.
    DATA:
      lt_tech_fields TYPE fieldname_tab.

    super->clear_irrelevant_data( CHANGING os_obj = os_obj ).
    SPLIT 'AS4USER AS4DATE AS4TIME ENTITYTAB OUTPUTLEN VALEXI'
          AT space INTO TABLE lt_tech_fields.

    clear_technical_fields(
      EXPORTING it_tech_fields = lt_tech_fields
      CHANGING ct_tab = os_obj-dted-dd04v ).

  ENDMETHOD.                    "clear_irrelevant_data

  METHOD evaluate_delta.
    READ TABLE as_delta-dted-dd04v INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "General attributes"' ).
    ELSE.
      ov_status = aco_status_equal.
    ENDIF.
  ENDMETHOD.                    "evaluate_delta

  METHOD show_diff.
    DATA:
      lv_system TYPE tmssysnam,
      lv_rfc_dest TYPE rfcdest.

    lv_system = ao_target_sys->get_name( ).
    lv_rfc_dest = ao_target_sys->get_rfc_dest( ).

    SUBMIT radvvde2 AND RETURN
          WITH objname  = as_obj-obj_name
          WITH objnam2  = as_obj-obj_name
          WITH versno1  = '00000'
          WITH versno2  = '00000'
          WITH objtyp1  = 'DTEL'
          WITH objtyp2  = 'DTEL'
          WITH log_dest = lv_rfc_dest
          WITH rem_syst = lv_system.

  ENDMETHOD.                    "show_diff

ENDCLASS.                    "gcl_dted_checker IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS gcl_domd_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_domd_checker IMPLEMENTATION.

  METHOD clone.
    DATA:
      lo_checker TYPE REF TO gcl_domd_checker.

    CREATE OBJECT lo_checker.
    lo_checker->ao_target_sys = ao_target_sys.
    oo_checker = lo_checker.
  ENDMETHOD.                    "clone

  METHOD clear_irrelevant_data.
    DATA:
      lt_tech_fields TYPE fieldname_tab.

    super->clear_irrelevant_data( CHANGING os_obj = os_obj ).
    SPLIT 'AS4USER AS4DATE AS4TIME'
          AT space INTO TABLE lt_tech_fields.

    clear_technical_fields(
      EXPORTING it_tech_fields = lt_tech_fields
      CHANGING ct_tab = os_obj-domd-dd01v ).

    clear_technical_fields(
      EXPORTING it_tech_fields = lt_tech_fields
      CHANGING ct_tab = os_obj-domd-dd07v ).

  ENDMETHOD.                    "clear_irrelevant_data

  METHOD evaluate_delta.
    "General Attributes
    READ TABLE as_delta-domd-dd01v INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ao_logger->add_warning( 'Difference in "General attributes"' ).
      ov_status = aco_status_changed.
      RETURN.
    ENDIF.

    "Values
    READ TABLE as_delta-domd-dd07v INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Fixed values and domain texts"' ).
    ELSE.
      ov_status = aco_status_equal.
    ENDIF.
  ENDMETHOD.                    "evaluate_delta

  METHOD show_diff.
    DATA:
      lv_system TYPE tmssysnam,
      lv_rfc_dest TYPE rfcdest.

    lv_system = ao_target_sys->get_name( ).
    lv_rfc_dest = ao_target_sys->get_rfc_dest( ).

    SUBMIT radvvdo2 AND RETURN
          WITH objname  = as_obj-obj_name
          WITH objnam2  = as_obj-obj_name
          WITH versno1  = '00000'
          WITH versno2  = '00000'
          WITH objtyp1  = 'DTEL'
          WITH objtyp2  = 'DTEL'
          WITH log_dest = lv_rfc_dest
          WITH rem_syst = lv_system.

  ENDMETHOD.                    "show_diff

ENDCLASS.                    "gcl_domd_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS gcl_vied_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_vied_checker IMPLEMENTATION.

  METHOD clone.
    DATA:
      lo_checker TYPE REF TO gcl_vied_checker.

    CREATE OBJECT lo_checker.
    lo_checker->ao_target_sys = ao_target_sys.
    oo_checker = lo_checker.
  ENDMETHOD.                    "clone

  METHOD clear_irrelevant_data.
    DATA:
      lt_tech_fields TYPE fieldname_tab.

    super->clear_irrelevant_data( CHANGING os_obj = os_obj ).
    SPLIT 'AS4USER AS4DATE AS4TIME'
        AT space INTO TABLE lt_tech_fields.

    clear_technical_fields(
      EXPORTING it_tech_fields = lt_tech_fields
      CHANGING ct_tab = os_obj-vied-dd25v ).

  ENDMETHOD.                    "clear_irrelevant_data

  METHOD evaluate_delta.
    " General Attributes
    READ TABLE as_delta-vied-dd25v INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "General Attributes"' ).
      RETURN.
    ENDIF.

    " Base Tables
    READ TABLE as_delta-vied-dd26v INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Base Tables"' ).
      RETURN.
    ENDIF.

    " Fields
    READ TABLE as_delta-vied-dd27v INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Fields"' ).
      RETURN.
    ENDIF.

    " Conditions
    READ TABLE as_delta-vied-dd28v INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Selection Conditions"' ).
    ELSE.
      ov_status = aco_status_equal.
    ENDIF.
  ENDMETHOD.                    "evaluate_delta

  METHOD show_diff.
    DATA:
      lv_system TYPE tmssysnam,
      lv_rfc_dest TYPE rfcdest.

    lv_system = ao_target_sys->get_name( ).
    lv_rfc_dest = ao_target_sys->get_rfc_dest( ).

    SUBMIT radvvvi2 AND RETURN
          WITH objname  = as_obj-obj_name
          WITH objnam2  = as_obj-obj_name
          WITH versno1  = '00000'
          WITH versno2  = '00000'
          WITH objtyp1  = 'TTYP'
          WITH objtyp2  = 'TTYP'
          WITH log_dest = lv_rfc_dest
          WITH rem_syst = lv_system.
  ENDMETHOD.                    "show_diff

ENDCLASS.                    "gcl_vied_checker IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS gcl_ttyd_checker IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_ttyd_checker IMPLEMENTATION.

  METHOD clone.
    DATA:
      lo_checker TYPE REF TO gcl_ttyd_checker.

    CREATE OBJECT lo_checker.
    lo_checker->ao_target_sys = ao_target_sys.
    oo_checker = lo_checker.
  ENDMETHOD.                    "clone

  METHOD clear_irrelevant_data.
    DATA:
        lt_tech_fields TYPE fieldname_tab.

    super->clear_irrelevant_data( CHANGING os_obj = os_obj ).
    SPLIT 'AS4USER AS4DATE AS4TIME'
          AT space INTO TABLE lt_tech_fields.

    clear_technical_fields(
      EXPORTING it_tech_fields = lt_tech_fields
      CHANGING ct_tab = os_obj-ttyd-dd40v ).

    clear_technical_fields(
      EXPORTING it_tech_fields = lt_tech_fields
      CHANGING ct_tab = os_obj-ttyd-dd42v ).

  ENDMETHOD.                    "clear_irrelevant_data

  METHOD evaluate_delta.
    READ TABLE as_delta-ttyd-dd40v INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Interface structure for table types"' ).
      RETURN.
    ENDIF.

    READ TABLE as_delta-ttyd-dd42v INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ov_status = aco_status_changed.
      ao_logger->add_warning( 'Difference in "Interface structure for table type key fields"' ).
    ELSE.
      ov_status = aco_status_equal.
    ENDIF.
  ENDMETHOD.                    "evaluate_delta

  METHOD show_diff.

    DATA:
      lv_system TYPE tmssysnam,
      lv_rfc_dest TYPE rfcdest.

    lv_system = ao_target_sys->get_name( ).
    lv_rfc_dest = ao_target_sys->get_rfc_dest( ).

    SUBMIT radvvty2 AND RETURN
          WITH objname  = as_obj-obj_name
          WITH objnam2  = as_obj-obj_name
          WITH versno1  = '00000'
          WITH versno2  = '00000'
          WITH objtyp1  = 'TTYP'
          WITH objtyp2  = 'TTYP'
          WITH log_dest = lv_rfc_dest
          WITH rem_syst = lv_system.

  ENDMETHOD.                    "show_diff

ENDCLASS.                    "gcl_ttyd_checker IMPLEMENTATION

CLASS gcl_sfpi_checker IMPLEMENTATION.

  METHOD clone.
    DATA:
      lo_checker TYPE REF TO gcl_sfpi_checker.

    CREATE OBJECT lo_checker.
    lo_checker->ao_target_sys = ao_target_sys.
    oo_checker = lo_checker.
  ENDMETHOD.                    "clone

  METHOD clear_irrelevant_data.
    DATA:
        lt_tech_fields TYPE fieldname_tab.

*    super->clear_irrelevant_data( CHANGING os_obj = os_obj ).
*    SPLIT 'AS4USER AS4DATE AS4TIME'
*          AT space INTO TABLE lt_tech_fields.
*
*    clear_technical_fields(
*      EXPORTING it_tech_fields = lt_tech_fields
*      CHANGING ct_tab = os_obj-ttyd-dd40v ).
*
*    clear_technical_fields(
*      EXPORTING it_tech_fields = lt_tech_fields
*      CHANGING ct_tab = os_obj-ttyd-dd42v ).

  ENDMETHOD.                    "clear_irrelevant_data

  METHOD evaluate_delta.
*    READ TABLE as_delta-ttyd-dd40v INDEX 1 TRANSPORTING NO FIELDS.
*    IF sy-subrc = 0.
*      ov_status = aco_status_changed.
*      ao_logger->add_warning( 'Difference in "Interface structure for table types"' ).
*      RETURN.
*    ENDIF.
*
*    READ TABLE as_delta-ttyd-dd42v INDEX 1 TRANSPORTING NO FIELDS.
*    IF sy-subrc = 0.
*      ov_status = aco_status_changed.
*      ao_logger->add_warning( 'Difference in "Interface structure for table type key fields"' ).
*    ELSE.
*      ov_status = aco_status_equal.
*    ENDIF.
    ov_status = aco_status_equal.
  ENDMETHOD.                    "evaluate_delta

  METHOD show_diff.

    DATA:
      lv_system TYPE tmssysnam,
      lv_rfc_dest TYPE rfcdest.

    lv_system = ao_target_sys->get_name( ).
    lv_rfc_dest = ao_target_sys->get_rfc_dest( ).

    SUBMIT radvvty2 AND RETURN
          WITH objname  = as_obj-obj_name
          WITH objnam2  = as_obj-obj_name
          WITH versno1  = '00000'
          WITH versno2  = '00000'
          WITH objtyp1  = 'TTYP'
          WITH objtyp2  = 'TTYP'
          WITH log_dest = lv_rfc_dest
          WITH rem_syst = lv_system.

  ENDMETHOD.                    "show_diff
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS gcl_checker_manager IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_checker_manager IMPLEMENTATION.

  METHOD register_checker.
    DATA:
      ls_obj_checker TYPE yls_obj_checker.

    ls_obj_checker-object = iv_object.
    ls_obj_checker-prototype = io_prototype.
    INSERT ls_obj_checker INTO TABLE at_obj_checker.

  ENDMETHOD.                    "register_checker

  METHOD get_checker_instance.
    DATA:
      ls_obj_checker TYPE yls_obj_checker.

    CLEAR oo_checker.

    READ TABLE at_obj_checker INTO ls_obj_checker
      WITH TABLE KEY object = iv_object.

    IF sy-subrc <> 0.
      READ TABLE at_obj_checker INTO ls_obj_checker
        WITH TABLE KEY object = 'DFLT'.
    ENDIF.

    CHECK sy-subrc = 0.
    oo_checker = ls_obj_checker-prototype->clone( ).

  ENDMETHOD.                    "get_checker

  METHOD get_registered_types.
    DATA:
      ls_type_range TYPE devtyrgln,
      ls_obj_checker TYPE yls_obj_checker.

    CLEAR ot_types.

    ls_type_range-sign = 'I'.
    ls_type_range-option = 'EQ'.

    LOOP AT at_obj_checker INTO ls_obj_checker.
      ls_type_range-low = ls_obj_checker-object.
      APPEND ls_type_range TO ot_types.
    ENDLOOP.

  ENDMETHOD.                    "get_registered_types

ENDCLASS.                    "gcl_checker_manager IMPLEMENTATION
"$. Endregion

"$. Region SELECTION SCREEN

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-bl2.
SELECT-OPTIONS:
  so_ddic FOR sci_dynp-o_ddic MEMORY ID ddic, "ddic type
  so_ddty FOR sci_dynp-o_ddty MEMORY ID ddty. "type group
SELECTION-SCREEN END  OF  BLOCK bl2.

SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-bl3.
SELECT-OPTIONS:
  so_prog FOR sci_dynp-o_repo MEMORY ID prog, "complex program (with includes)
  so_fugr FOR sci_dynp-o_fugr MEMORY ID fugr, "function group
  so_pckg FOR sci_dynp-o_tadir_p MEMORY ID pckg, "package
  so_clas FOR sci_dynp-o_clas MEMORY ID clas. "class
SELECTION-SCREEN END  OF  BLOCK bl3.

SELECTION-SCREEN BEGIN OF BLOCK bl8 WITH FRAME TITLE text-bl8.
SELECT-OPTIONS:
  so_sfpf FOR sfpverscform-name,
  so_sfpi FOR sfpverscform-interface.
SELECTION-SCREEN END  OF  BLOCK bl8.


SELECTION-SCREEN BEGIN OF BLOCK bl4 WITH FRAME TITLE text-bl4.
SELECT-OPTIONS:
  so_tran FOR sci_dynp-o_order MEMORY ID tran.
SELECTION-SCREEN END  OF  BLOCK bl4.

SELECTION-SCREEN BEGIN OF BLOCK bl5 WITH FRAME TITLE text-bl5.
SELECT-OPTIONS:
  so_fpckg FOR sci_dynp-o_tadir_p MEMORY ID fpckg, "filter for package
  so_fresp FOR sci_dynp-o_tadir_r MEMORY ID fresp. "filter for person resonsible
SELECTION-SCREEN END  OF  BLOCK bl5.

SELECTION-SCREEN BEGIN OF BLOCK bl6 WITH FRAME TITLE text-bl6.

PARAMETERS:
  p_syst TYPE verssysnam OBLIGATORY MEMORY ID syst.

SELECTION-SCREEN END OF BLOCK bl6.

SELECTION-SCREEN BEGIN OF BLOCK bl7 WITH FRAME TITLE text-bl7.

PARAMETERS:
  p_diffs TYPE boolean AS CHECKBOX DEFAULT 'X',
  p_noinde TYPE vrsdelta1 DEFAULT 'X',
  p_nocmnt TYPE vrsdelta2 DEFAULT 'X',
  p_noempt TYPE vrsdelta2 DEFAULT 'X',
  p_unsupp TYPE boolean AS CHECKBOX.

SELECTION-SCREEN END  OF  BLOCK bl7.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_syst.
  PERFORM get_system.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_tran-low.
  PERFORM get_tr_request.

  "$. Endregion

  "$. Region FORMS
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UCOMM      text
*      -->SELFIELD   text
*----------------------------------------------------------------------*
FORM user_command USING iv_ucomm TYPE sy-ucomm is_selfield TYPE slis_selfield.
  CASE iv_ucomm.
    WHEN '&IC1'.
      IF is_selfield-fieldname = 'STATUS_TXT'.
        gcl_main=>show_diff( is_selfield-tabindex ).
      ELSEIF is_selfield-fieldname = 'LOG_LINK'.
        gcl_main=>show_log( is_selfield-tabindex ).
      ELSE.
        gcl_main=>show_object( is_selfield-tabindex ).
      ENDIF.
  ENDCASE.
ENDFORM.                    "USER_COMMAND

FORM get_system.

  DATA:
    lv_system  TYPE tmscsys-sysnam .

  CALL FUNCTION 'TMS_UI_F4_SYSTEMS'
    CHANGING
      cv_system = lv_system.

  p_syst = lv_system.
ENDFORM.

FORM get_tr_request.
  DATA:
    lv_from_date TYPE d,
    lv_trkorr    TYPE e070-trkorr.

  lv_from_date = sy-datum - 90.
  CALL FUNCTION 'TR_F4_REQUESTS'
    EXPORTING
      iv_trfunctions      = 'K'
      iv_trstatus         = 'RDL'
      iv_from_date        = lv_from_date
    IMPORTING
      ev_selected_request = lv_trkorr.

  IF sy-subrc = 0.
    so_tran-low = lv_trkorr.
  ENDIF.

ENDFORM.
"$. Endregion

START-OF-SELECTION.

  "Dictionary
  gcl_main=>ar_ddics = so_ddic[].
  gcl_main=>ar_typps = so_ddty[].
  "Complex
  gcl_main=>ar_progs = so_prog[].
  gcl_main=>ar_fugrs = so_fugr[].
  gcl_main=>ar_pckg = so_pckg[].
  gcl_main=>ar_clas = so_clas[].
  "Transports
  gcl_main=>ar_tran = so_tran[].
  "Filters
  gcl_main=>ar_fpckg = so_fpckg[].
  gcl_main=>ar_fresp = so_fresp[].
  "Free Objects
  gcl_main=>add_free_obj( iv_type = 'SFPI' ir_range = so_sfpi[] ).
  gcl_main=>add_free_obj( iv_type = 'SFPF' ir_range = so_sfpf[] ).
  "Target system
  gcl_main=>av_syst = p_syst.
  "Options
  gcl_main=>av_only_diffs = p_diffs.
  gcl_main=>av_ignore_indents = p_noinde.
  gcl_main=>av_ignore_comments = p_nocmnt.
  gcl_main=>av_ignore_empty_lines = p_noempt.
  gcl_main=>av_show_unsupported = p_unsupp.

  gcl_main=>start( ).

END-OF-SELECTION.
