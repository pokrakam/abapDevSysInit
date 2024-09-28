REPORT zdevsysinit.

*-------------------------------------------------------------------------------*
* MIT License
*
* Copyright (c) 2024 Mike Pokraka
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
*-------------------------------------------------------------------------------*

PARAMETERS p_usrpfl TYPE abap_bool AS CHECKBOX DEFAULT abap_false.

SELECTION-SCREEN BEGIN OF BLOCK rfc WITH FRAME.
  PARAMETERS p_rfcdst TYPE abap_bool AS CHECKBOX DEFAULT abap_false.
  PARAMETERS p_ghuser TYPE rfcalias.
  PARAMETERS p_token TYPE rfcexec_ext.
SELECTION-SCREEN END OF BLOCK rfc.

PARAMETERS p_certi TYPE abap_bool AS CHECKBOX DEFAULT abap_false.
PARAMETERS p_agstd TYPE abap_bool AS CHECKBOX DEFAULT abap_true.
PARAMETERS p_agxit TYPE abap_bool AS CHECKBOX DEFAULT abap_true.
PARAMETERS p_repos TYPE abap_bool AS CHECKBOX DEFAULT abap_false.
*parameters p_repcnf type abap_bool AS CHECKBOX DEFAULT abap_false. "Todo: Overwrite notification text


*--------------------------------------------------------------------*
CLASS lcx_error DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
"! Log and progress bar
CLASS output DEFINITION CREATE PUBLIC.
*--------------------------------------------------------------------*

  PUBLIC SECTION.

    CLASS-METHODS get_instance RETURNING VALUE(result) TYPE REF TO output.

    "! Logs output message and shows it on progress bar
    METHODS write IMPORTING data TYPE any.
    "! Displays all messages
    METHODS display.
    "! Appends text to last message
    METHODS add_to_last IMPORTING text TYPE clike.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA out TYPE REF TO output.
    TYPES: BEGIN OF t_out,
             date TYPE sydatum,
             time TYPE sy-uzeit,
             text TYPE string,
           END OF t_out.
    CLASS-DATA outtab TYPE STANDARD TABLE OF t_out WITH EMPTY KEY.

ENDCLASS.


*--------------------------------------------------------------------*
CLASS output IMPLEMENTATION.
*--------------------------------------------------------------------*

  METHOD write.
    CONSTANTS: type_chars TYPE string VALUE 'CNg',
               type_table TYPE string VALUE 'h' ##NO_TEXT.

    FIELD-SYMBOLS <line> TYPE any.

    cl_progress_indicator=>progress_indicate( i_text               = data
                                              i_output_immediately = abap_true ).

    DESCRIBE FIELD data TYPE DATA(type).

    IF type_chars CS type.
      APPEND VALUE #(
          date = sy-datum
          time = sy-uzeit
          text = data )
        TO outtab.

    ELSEIF type = type_table.
      LOOP AT data ASSIGNING <line>.
        write( <line> ).
      ENDLOOP.

    ELSE.
      DATA(json) = /ui2/cl_json=>serialize(
        data             = data
        pretty_name      = /ui2/cl_json=>pretty_mode-low_case
        compress         = abap_false
        assoc_arrays     = abap_true
        assoc_arrays_opt = abap_true
        format_output    = abap_true
        hex_as_base64    = abap_false ).
      APPEND VALUE #(
          date = sy-datum
          time = sy-uzeit
          text = json )
        TO outtab.

    ENDIF.

  ENDMETHOD.

  METHOD display.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(alv)
          CHANGING
            t_table      = outtab ).
      CATCH cx_salv_msg.
    ENDTRY.

    alv->get_columns( )->set_optimize( abap_true ).
    alv->display( ).

  ENDMETHOD.


  METHOD get_instance.
    IF out IS INITIAL.
      out = NEW output( ).
    ENDIF.
    result = out.
  ENDMETHOD.


  METHOD add_to_last.

    DATA(last_row) = lines( outtab ).

    IF last_row > 0.
      DATA(row) = REF #( outtab[ last_row ] ).
      row->text = |{ row->text } { text }|.
    ENDIF.

    cl_progress_indicator=>progress_indicate( i_text               = row->text
                                              i_output_immediately = abap_true ).
  ENDMETHOD.

ENDCLASS.


*--------------------------------------------------------------------*
CLASS rfc_destination DEFINITION CREATE PUBLIC.
*--------------------------------------------------------------------*

  PUBLIC SECTION.

    CLASS-METHODS class_constructor.

    CLASS-METHODS exists IMPORTING name          TYPE rfcdest
                         RETURNING VALUE(result) TYPE abap_bool.

    METHODS execute IMPORTING name  TYPE string
                              user  TYPE rfcalias
                              token TYPE rfcdisplay-rfcexec
                              reset TYPE abap_bool DEFAULT abap_false.
  PROTECTED SECTION.
    METHODS create_rfc_destination IMPORTING user  TYPE rfcalias
                                             token TYPE rfcdisplay-rfcexec.
    METHODS delete_rfc_destination.
    METHODS check_existence IMPORTING name          TYPE rfcdest
                            RETURNING VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    DATA dest_name TYPE rfcdest VALUE 'GITHUB' ##NO_TEXT.
    CLASS-DATA out TYPE REF TO output.

ENDCLASS.


*--------------------------------------------------------------------*
CLASS rfc_destination IMPLEMENTATION.
*--------------------------------------------------------------------*

  METHOD class_constructor.
    out = output=>get_instance( ).
  ENDMETHOD.


  METHOD execute.

    dest_name = name.

    IF check_existence( dest_name ).
      delete_rfc_destination( ).
      out->write( |RFC destination { dest_name } deleted| ).
    ENDIF.

    out->write( |Creating RFC destination { dest_name }...| ).
    create_rfc_destination( user  = user
                            token = token ).
    out->add_to_last( | done.| ).

  ENDMETHOD.


  METHOD create_rfc_destination.

    CALL FUNCTION 'RFC_MODIFY_HTTP_DEST_TO_EXT'
      EXPORTING
        destination                = dest_name
        action                     = 'I'
        authority_check            = abap_true
        servicenr                  = '443'
        server                     = 'github.com'
        user                       = user
        password                   = token
        sslapplic                  = 'ANONYM'
        logon_method               = 'B'
        ssl                        = abap_true
      EXCEPTIONS
        authority_not_available    = 1
        destination_already_exist  = 2
        destination_not_exist      = 3
        destination_enqueue_reject = 4
        information_failure        = 5
        trfc_entry_invalid         = 6
        internal_failure           = 7
        snc_information_failure    = 8
        snc_internal_failure       = 9
        destination_is_locked      = 10
        invalid_parameter          = 11
        OTHERS                     = 12.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD exists.
    result = NEW rfc_destination( )->check_existence( name ).
  ENDMETHOD.


  METHOD delete_rfc_destination.

    CALL FUNCTION 'RFC_MODIFY_HTTP_DEST_TO_EXT'
      EXPORTING
        destination                = dest_name
        action                     = 'D'
        authority_check            = abap_true
      EXCEPTIONS
        authority_not_available    = 1
        destination_already_exist  = 2
        destination_not_exist      = 3
        destination_enqueue_reject = 4
        information_failure        = 5
        trfc_entry_invalid         = 6
        internal_failure           = 7
        snc_information_failure    = 8
        snc_internal_failure       = 9
        destination_is_locked      = 10
        invalid_parameter          = 11
        OTHERS                     = 12.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD check_existence.
    result = NEW cl_dest_factory( )->exists( name ).
  ENDMETHOD.

ENDCLASS.


*--------------------------------------------------------------------*
CLASS repo_import DEFINITION CREATE PUBLIC.
*--------------------------------------------------------------------*

  PUBLIC SECTION.

    METHODS execute IMPORTING name    TYPE string
                              package TYPE devclass
                              url     TYPE string
                    RAISING
                              lcx_error.
    METHODS constructor RAISING lcx_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES t_value TYPE c LENGTH 12.

    TYPES: BEGIN OF t_local_settings,
             display_name                 TYPE string,
             ignore_subpackages           TYPE abap_bool,
             write_protected              TYPE abap_bool,
             only_local_objects           TYPE abap_bool,
             code_inspector_check_variant TYPE sci_chkv,
             block_commit                 TYPE abap_bool,
             main_language_only           TYPE abap_bool,
             labels                       TYPE string,
             transport_request            TYPE trkorr,
             customizing_request          TYPE trkorr,
             flow                         TYPE abap_bool,
             exclude_remote_paths         TYPE string_table,
           END OF t_local_settings.

    TYPES t_languages TYPE STANDARD TABLE OF laiso WITH DEFAULT KEY.

    TYPES: BEGIN OF t_requirement,
             component   TYPE tdevc-dlvunit,
             min_release TYPE saprelease,
             min_patch   TYPE sappatchlv,
           END OF t_requirement.
    TYPES t_requirements TYPE STANDARD TABLE OF t_requirement WITH DEFAULT KEY.

    TYPES: BEGIN OF t_dot_abapgit,
             name                  TYPE string,
             master_language       TYPE spras,
             i18n_languages        TYPE t_languages,
             use_lxe               TYPE abap_bool,
             starting_folder       TYPE string,
             folder_logic          TYPE string,
             ignore                TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
             requirements          TYPE t_requirements,
             version_constant      TYPE string,
             abap_language_version TYPE string,
             original_system       TYPE tadir-srcsystem,
           END OF t_dot_abapgit.

    TYPES t_sha1    TYPE c LENGTH 40.

    TYPES: BEGIN OF t_repo_xml,
             url             TYPE string,
             branch_name     TYPE string,
             selected_commit TYPE t_sha1,
             package         TYPE devclass,
             created_by      TYPE syuname,
             created_at      TYPE timestampl,
             deserialized_by TYPE syuname,
             deserialized_at TYPE timestampl,
             offline         TYPE abap_bool,
             switched_origin TYPE string,
             dot_abapgit     TYPE t_dot_abapgit,
             head_branch     TYPE string,   " HEAD symref of the repo, master branch
             local_settings  TYPE t_local_settings,
           END OF t_repo_xml.

    TYPES: BEGIN OF t_repo,
             key TYPE t_value.
             INCLUDE TYPE t_repo_xml.
    TYPES: END OF t_repo.

    TYPES t_repos TYPE STANDARD TABLE OF t_repo WITH DEFAULT KEY.

    DATA out TYPE REF TO output.

    METHODS get_repos RETURNING VALUE(result) TYPE t_repos
                      RAISING   lcx_error.

ENDCLASS.


*--------------------------------------------------------------------*
CLASS repo_import IMPLEMENTATION.
*--------------------------------------------------------------------*

  METHOD constructor.
    out = output=>get_instance( ).
  ENDMETHOD.


  METHOD execute.

    TYPES: BEGIN OF t_log_out,
             type      TYPE symsgty,
             text      TYPE string,
             obj_type  TYPE trobjtype,
             obj_name  TYPE sobj_name,
             exception TYPE REF TO cx_root,
           END OF t_log_out.
    TYPES tty_log_out TYPE STANDARD TABLE OF t_log_out
                WITH EMPTY KEY.
    DATA messages TYPE tty_log_out.

    DATA repo_srv TYPE REF TO object. "zif_abapgit_repo_srv
    DATA log TYPE REF TO object. "zif_abapgit_log
    DATA background TYPE REF TO object. "zif_abapgit_background
    DATA error TYPE REF TO cx_static_check.

    DATA(repos) = get_repos( ).

    LOOP AT repos INTO DATA(repo_data) WHERE package = package.
      IF repo_data-local_settings-display_name <> name.
        out->write( |Package { package } already used in repo { repo_data-local_settings-display_name }| ).
        RETURN.
      ENDIF.
    ENDLOOP.
    DATA(exists) = xsdbool( sy-subrc = 0 ).

    out->write( |Starting pull for { name }| ).


    CALL METHOD ('\PROGRAM=ZABAPGIT_STANDALONE\CLASS=ZCL_ABAPGIT_REPO_SRV')=>get_instance RECEIVING ri_srv = repo_srv.

    "We've got to confuse the compiler to make sure it doens't notice a different technical type

    DATA repo TYPE REF TO object. "zcl_abapgit_repo_online
    DATA dr_repo TYPE REF TO data.
    CREATE DATA dr_repo TYPE REF TO ('\PROGRAM=ZABAPGIT_STANDALONE\INTERFACE=ZIF_ABAPGIT_REPO').
    dr_repo->* ?= repo.
    ASSIGN dr_repo->* TO FIELD-SYMBOL(<if_repo>).

    IF exists = abap_true.

      TRY.

          CALL METHOD repo_srv->(`ZIF_ABAPGIT_REPO_SRV~GET`)
            EXPORTING
              iv_key  = repo_data-key
            RECEIVING
              ri_repo = <if_repo>.

        CATCH cx_static_check INTO error.  "zcx_abapgit_exception
          out->write( |Open repo { name } failed: { error->get_text( ) }| ).
          RAISE EXCEPTION NEW lcx_error( ).
      ENDTRY.

    ELSE.

      TRY.

          "Needed in order to cast return parameter to correct type
          CREATE OBJECT repo TYPE ('\PROGRAM=ZABAPGIT_STANDALONE\CLASS=ZCL_ABAPGIT_REPO_ONLINE')
            EXPORTING is_data = VALUE t_repo( key = 'DUMMY' ).

          CALL METHOD repo_srv->(`ZIF_ABAPGIT_REPO_SRV~NEW_ONLINE`)
            EXPORTING
              iv_url          = url
              iv_package      = package
              iv_display_name = name
            RECEIVING
              ri_repo         = <if_repo>.


          out->write( |Repo { name } created| ).
        CATCH cx_static_check INTO error.
          out->write( |Create repo { name } failed: { error->get_text( ) }| ).
          RAISE EXCEPTION NEW lcx_error( ).
      ENDTRY.

    ENDIF.

    CREATE OBJECT log TYPE (`\PROGRAM=ZABAPGIT_STANDALONE\CLASS=ZCL_ABAPGIT_LOG`).

    CREATE OBJECT background TYPE (`\PROGRAM=ZABAPGIT_STANDALONE\CLASS=ZCL_ABAPGIT_BACKGROUND_PULL`).

    CREATE DATA dr_repo TYPE REF TO ('\PROGRAM=ZABAPGIT_STANDALONE\CLASS=ZCL_ABAPGIT_REPO_ONLINE').
    dr_repo->* ?= <if_repo>.
    ASSIGN dr_repo->* TO FIELD-SYMBOL(<cl_repo>).

    DATA dr_log TYPE REF TO data.
    CREATE DATA dr_log TYPE REF TO ('\PROGRAM=ZABAPGIT_STANDALONE\CLASS=ZCL_ABAPGIT_LOG').
    dr_log->* ?= log.
    ASSIGN dr_log->* TO FIELD-SYMBOL(<log>).

    TRY.
        CALL METHOD background->(`ZIF_ABAPGIT_BACKGROUND~RUN`)
          EXPORTING
            io_repo = <cl_repo>
            ii_log  = <log>.
      CATCH cx_static_check INTO error.
        out->write( |Import repo { name } failed: { error->get_text( ) }| ).
        RAISE EXCEPTION NEW lcx_error( ).
    ENDTRY.

    IF 1 = 0. "Todo: Too much output, maybe add a debug mode
      CALL METHOD log->(`ZIF_ABAPGIT_LOG~GET_MESSAGES`) RECEIVING rt_msg = messages.
      LOOP AT messages INTO DATA(message).
        IF message-text NS 'no import required'.
          out->write( message-text ).
        ENDIF.
      ENDLOOP.
    ENDIF.

    out->write( |Pull { name } completed.| ).

  ENDMETHOD.


  METHOD get_repos.

    DATA repo TYPE REF TO object.

    TRY.
        CALL METHOD ('\PROGRAM=ZABAPGIT_STANDALONE\CLASS=ZCL_ABAPGIT_PERSIST_FACTORY')=>get_repo RECEIVING ri_repo = repo.
        CALL METHOD repo->('ZIF_ABAPGIT_PERSIST_REPO~LIST') RECEIVING rt_repos = result.

      CATCH cx_sy_dyn_call_illegal_class.
        out->write( `Cannot pull repos, requires abapGit developer version` ).
        RAISE EXCEPTION NEW lcx_error( ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.


CLASS ltc_repo DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      invalid_repo_fails FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_repo IMPLEMENTATION.

  METHOD invalid_repo_fails.
    DATA(cut) = NEW repo_import( ).
    TRY.
        cut->execute(
          name    = 'Test'
          package = '123'
          url     = 'http //'
        ).
        cl_abap_unit_assert=>fail( 'No exception raised' ).
      CATCH lcx_error.
        "As expected
    ENDTRY.

  ENDMETHOD.

ENDCLASS.


*--------------------------------------------------------------------*
CLASS abapgit_standalone DEFINITION CREATE PUBLIC.
*--------------------------------------------------------------------*

  PUBLIC SECTION.
    METHODS execute RAISING lcx_error.

  PROTECTED SECTION.

*    TYPES: BEGIN OF t_source_line,
*             line TYPE abaptxt255,
*           END OF t_source_line.

    TYPES t_source_lines TYPE STANDARD TABLE OF abaptxt255 WITH EMPTY KEY.

    DATA url TYPE string VALUE `https://raw.githubusercontent.com/abapGit/build/main/zabapgit_standalone.prog.abap`.
    DATA out TYPE REF TO output.

    DATA: source       TYPE string,
          source_lines TYPE t_source_lines.

    METHODS get_source RETURNING VALUE(result) TYPE string
                       RAISING   lcx_error.
    METHODS insert_report IMPORTING program_name TYPE sobj_name
                                    program_type TYPE subc DEFAULT '1'
                          RAISING   lcx_error.
    METHODS update_report IMPORTING program TYPE sobj_name
                          RAISING   lcx_error.

    METHODS validate_and_split_source IMPORTING source        TYPE string
                                      RETURNING VALUE(result) TYPE t_source_lines
                                      RAISING   lcx_error.
    METHODS exists IMPORTING obj_name      TYPE sobj_name
                   RETURNING VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    METHODS fail_on_nonzero_subrc IMPORTING message TYPE string
                                  RAISING   lcx_error.
ENDCLASS.


*--------------------------------------------------------------------*
CLASS abapgit_standalone IMPLEMENTATION.
*--------------------------------------------------------------------*

  METHOD execute.
    CONSTANTS program TYPE sobj_name VALUE `ZABAPGIT_STANDALONE` ##NO_TEXT.

    out = output=>get_instance( ).

    out->write( `Fetching ZABAPGIT_STANDALONE source` ).
    source = get_source( ).
    source_lines = validate_and_split_source( source ).

    IF exists( program ).
      out->write( `Updating ZABAPGIT_STANDALONE...` ).
      update_report( program ).
    ELSE.
      out->write( `Creating ZABAPGIT_STANDALONE...` ).
      insert_report( program ).
    ENDIF.

    out->add_to_last( ' done.' ).

  ENDMETHOD.


  METHOD get_source.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = url
      IMPORTING
        client             = DATA(client)    " HTTP Client Abstraction
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).
    fail_on_nonzero_subrc( `Could not create HTTP client` ).

    client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).

    fail_on_nonzero_subrc( |HTTP Send failure| ).

    client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).
    "No subrc check as we're checking status

    client->response->get_status(
      IMPORTING
        code   = DATA(code)
        reason = DATA(reason) ).
    IF code <> 200.
      out->write( |HTTP request failure { code }: { reason }| ).
      RAISE EXCEPTION NEW lcx_error( ).
    ENDIF.

    result = client->response->get_cdata( ).

  ENDMETHOD.


  METHOD fail_on_nonzero_subrc.
    IF sy-subrc <> 0.
      out->write( message ).
      RAISE EXCEPTION NEW lcx_error( ).
    ENDIF.
  ENDMETHOD.


  METHOD insert_report.

    CALL FUNCTION 'RPY_PROGRAM_INSERT'
      EXPORTING
        development_class = '$TMP'
        program_name      = program_name
        program_type      = program_type
        title_string      = 'abapGit Standalone'
        suppress_dialog   = abap_true
      TABLES
        source_extended   = source_lines
      EXCEPTIONS
        already_exists    = 1
        cancelled         = 2
        name_not_allowed  = 3
        permission_error  = 4
        OTHERS            = 5.

    fail_on_nonzero_subrc( |Failed to create ZABAPGIT_STANDALONE, subrc { sy-subrc }| ).

  ENDMETHOD.


  METHOD update_report.

    CALL FUNCTION 'RPY_PROGRAM_UPDATE'
      EXPORTING
        program_name     = program
      TABLES
        source_extended  = source_lines
      EXCEPTIONS
        cancelled        = 1
        permission_error = 2
        not_found        = 3
        OTHERS           = 4.

    fail_on_nonzero_subrc( |Failed to update ZABAPGIT_STANDALONE, subrc { sy-subrc }| ).

  ENDMETHOD.


  METHOD validate_and_split_source.

    IF substring( val = source
                  len = 26 ) <> `REPORT zabapgit_standalone`.
      out->write( `Did not retrieve expected abapGit standalone source` ).
      RAISE EXCEPTION NEW lcx_error( ).
    ENDIF.

    SPLIT source AT cl_abap_char_utilities=>newline INTO TABLE result.

  ENDMETHOD.


  METHOD exists.

    SELECT SINGLE @abap_true FROM reposrc
      WHERE progname = @obj_name
      INTO @result.

  ENDMETHOD.

ENDCLASS.


*--------------------------------------------------------------------*
CLASS abapgit_exit DEFINITION INHERITING FROM abapgit_standalone CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS execute REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS get_source_lines
      RETURNING
        VALUE(result) TYPE t_source_lines.

ENDCLASS.

CLASS abapgit_exit IMPLEMENTATION.


  METHOD execute.

    CONSTANTS program_name TYPE sobj_name VALUE 'ZABAPGIT_USER_EXIT'.

    out = output=>get_instance( ).

    source_lines = get_source_lines( ).

    IF exists( program_name ).
      out->write( `Updating abapGit Standalone Exit...` ).
      update_report( program_name ).
    ELSE.
      out->write( `Creating abapGit Standalone Exit...` ).
      insert_report( program_name = program_name
                     program_type = 'I' ).
    ENDIF.
    out->add_to_last( ' done.' ).

  ENDMETHOD.


  METHOD get_source_lines.

    result = VALUE #(
        ( |CLASS zcl_abapgit_user_exit DEFINITION| )
        ( |  FINAL| )
        ( |  CREATE PUBLIC.| )
        ( || )
        ( |  PUBLIC SECTION.| )
        ( |    INTERFACES zif_abapgit_exit.| )
        ( |  PROTECTED SECTION.| )
        ( |  PRIVATE SECTION.| )
        ( |ENDCLASS.| )
        ( || )
        ( || )
        ( |CLASS zcl_abapgit_user_exit IMPLEMENTATION.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~create_http_client.| )
        ( || )
        ( |    DATA(lv_host) = zcl_abapgit_url=>host( iv_url ).| )
        ( || )
        ( |    DATA(lv_destination) = COND rfcdest( WHEN lv_host CS 'gitlab' THEN 'GITLAB'| )
        ( |                                         WHEN lv_host CS 'github' THEN 'GITHUB' ).| )
        ( || )
        ( |    IF lv_destination IS INITIAL.| )
        ( |      RETURN.| )
        ( |    ENDIF.| )
        ( || )
        ( |    cl_http_client=>create_by_destination(| )
        ( |      EXPORTING| )
        ( |        destination              = lv_destination| )
        ( |      IMPORTING| )
        ( |        client                   = ri_client| )
        ( |      EXCEPTIONS| )
        ( |        argument_not_found       = 1| )
        ( |        destination_not_found    = 2| )
        ( |        destination_no_authority = 3| )
        ( |        plugin_not_active        = 4| )
        ( |        internal_error           = 5| )
        ( |        OTHERS                   = 6 ).| )
        ( || )
        ( |    IF sy-subrc <> 0.| )
        ( |      zcx_abapgit_exception=>raise_t100(  ).| )
        ( |    ENDIF.| )
        ( || )
        ( |  ENDMETHOD.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~adjust_display_commit_url.| )
        ( |  ENDMETHOD.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~adjust_display_filename.| )
        ( |  ENDMETHOD.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~allow_sap_objects.| )
        ( |  ENDMETHOD.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~change_local_host.| )
        ( |  ENDMETHOD.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~change_max_parallel_processes.| )
        ( |  ENDMETHOD.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~change_proxy_authentication.| )
        ( |  ENDMETHOD.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~change_proxy_port.| )
        ( |  ENDMETHOD.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~change_proxy_url.| )
        ( |  ENDMETHOD.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~change_rfc_server_group.| )
        ( |  ENDMETHOD.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~change_supported_data_objects.| )
        ( |  ENDMETHOD.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~change_supported_object_types.| )
        ( |  ENDMETHOD.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~change_tadir.| )
        ( |  ENDMETHOD.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~custom_serialize_abap_clif.| )
        ( |  ENDMETHOD.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~deserialize_postprocess.| )
        ( |  ENDMETHOD.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~determine_transport_request.| )
        ( |  ENDMETHOD.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~enhance_repo_toolbar.| )
        ( |  ENDMETHOD.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~get_ci_tests.| )
        ( |  ENDMETHOD.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~get_ssl_id.| )
        ( |  ENDMETHOD.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~http_client.| )
        ( |  ENDMETHOD.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~on_event.| )
        ( |  ENDMETHOD.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~pre_calculate_repo_status.| )
        ( |  ENDMETHOD.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~serialize_postprocess.| )
        ( |  ENDMETHOD.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~validate_before_push.| )
        ( |  ENDMETHOD.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~wall_message_list.| )
        ( |  ENDMETHOD.| )
        ( || )
        ( |  METHOD zif_abapgit_exit~wall_message_repo.| )
        ( |  ENDMETHOD.| )
        ( || )
        ( |ENDCLASS.| )
    ).
  ENDMETHOD.

ENDCLASS.


*--------------------------------------------------------------------*
"!
"! SSL Certificate classes re-used from
"! https://github.com/sandraros/zcerti which re-used
"! https://github.com/Marc-Bernard-Tools/ABAP-Strust
"!
"! MIT License
"!
"! Copyright (c) 2024 sandraros
"!
"! Permission is hereby granted, free of charge, to any person obtaining a copy
"! of this software and associated documentation files (the "Software"), to deal
"! in the Software without restriction, including without limitation the rights
"! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
"! copies of the Software, and to permit persons to whom the Software is
"! furnished to do so, subject to the following conditions:
"!
"! The above copyright notice and this permission notice shall be included in all
"! copies or substantial portions of the Software.
"!
"! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
"! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
"! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
"! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
"! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
"! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
"! SOFTWARE.
CLASS zcx_certi_strust DEFINITION
  INHERITING FROM cx_static_check
  CREATE PUBLIC.

************************************************************************
* Trust Management Error
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg.
    INTERFACES if_t100_message.

    CLASS-DATA null TYPE string.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !msgv1    TYPE symsgv OPTIONAL
        !msgv2    TYPE symsgv OPTIONAL
        !msgv3    TYPE symsgv OPTIONAL
        !msgv4    TYPE symsgv OPTIONAL.

    "! Raise exception with text
    "! @parameter iv_text | Text
    "! @parameter ix_previous | Previous exception
    "! @raising zcx_certi_strust | Exception
    CLASS-METHODS raise
      IMPORTING
        !iv_text     TYPE clike
        !ix_previous TYPE REF TO cx_root OPTIONAL
      RAISING
        zcx_certi_strust.

    "! Raise exception with T100 message
    "! <p>
    "! Will default to sy-msg* variables. These need to be set right before calling this method.
    "! </p>
    "! @parameter iv_msgid | Message ID
    "! @parameter iv_msgno | Message number
    "! @parameter iv_msgv1 | Message variable 1
    "! @parameter iv_msgv2 | Message variable 2
    "! @parameter iv_msgv3 | Message variable 3
    "! @parameter iv_msgv4 | Message variable 4
    "! @parameter ix_previous | Previous exception
    "! @raising zcx_certi_strust | Exception
    CLASS-METHODS raise_t100
      IMPORTING
        VALUE(iv_msgid) TYPE symsgid DEFAULT sy-msgid
        VALUE(iv_msgno) TYPE symsgno DEFAULT sy-msgno
        VALUE(iv_msgv1) TYPE symsgv DEFAULT sy-msgv1
        VALUE(iv_msgv2) TYPE symsgv DEFAULT sy-msgv2
        VALUE(iv_msgv3) TYPE symsgv DEFAULT sy-msgv3
        VALUE(iv_msgv4) TYPE symsgv DEFAULT sy-msgv4
        !ix_previous    TYPE REF TO cx_root OPTIONAL
      RAISING
        zcx_certi_strust.

    "! Raise with text from previous exception
    "! @parameter ix_previous | Previous exception
    "! @raising zcx_certi_strust | Exception
    CLASS-METHODS raise_with_text
      IMPORTING
        !ix_previous TYPE REF TO cx_root
      RAISING
        zcx_certi_strust.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_generic_error_msg TYPE string VALUE `An error occured`.

    CLASS-METHODS split_text_to_symsg
      IMPORTING
        !iv_text      TYPE string
      RETURNING
        VALUE(rs_msg) TYPE symsg.

ENDCLASS.


CLASS zcx_certi_strust IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( previous = previous ).

    if_t100_dyn_msg~msgv1 = msgv1.
    if_t100_dyn_msg~msgv2 = msgv2.
    if_t100_dyn_msg~msgv3 = msgv3.
    if_t100_dyn_msg~msgv4 = msgv4.

    CLEAR me->textid.

    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

  ENDMETHOD.


  METHOD raise.

    DATA:
      lv_text TYPE string,
      ls_msg  TYPE symsg.

    IF iv_text IS INITIAL.
      lv_text = c_generic_error_msg.
    ELSE.
      lv_text = iv_text.
    ENDIF.

    ls_msg = split_text_to_symsg( lv_text ).

    " Set syst variables using generic error message
    MESSAGE e001(00) WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4 INTO null.

    raise_t100( ix_previous = ix_previous ).

  ENDMETHOD.


  METHOD raise_t100.

    DATA ls_t100_key TYPE scx_t100key.

    IF iv_msgid IS NOT INITIAL.
      ls_t100_key-msgid = iv_msgid.
      ls_t100_key-msgno = iv_msgno.
      ls_t100_key-attr1 = 'IF_T100_DYN_MSG~MSGV1'.
      ls_t100_key-attr2 = 'IF_T100_DYN_MSG~MSGV2'.
      ls_t100_key-attr3 = 'IF_T100_DYN_MSG~MSGV3'.
      ls_t100_key-attr4 = 'IF_T100_DYN_MSG~MSGV4'.
    ENDIF.

    RAISE EXCEPTION TYPE zcx_certi_strust
      EXPORTING
        textid   = ls_t100_key
        msgv1    = iv_msgv1
        msgv2    = iv_msgv2
        msgv3    = iv_msgv3
        msgv4    = iv_msgv4
        previous = ix_previous.

  ENDMETHOD.


  METHOD raise_with_text.

    raise(
      iv_text     = ix_previous->get_text( )
      ix_previous = ix_previous ).

  ENDMETHOD.


  METHOD split_text_to_symsg.

    CONSTANTS:
      lc_length_of_msgv           TYPE i VALUE 50,
      lc_offset_of_last_character TYPE i VALUE 49.

    DATA:
      lv_text    TYPE c LENGTH 200,
      lv_rest    TYPE c LENGTH 200,
      lv_msg_var TYPE c LENGTH lc_length_of_msgv,
      lv_index   TYPE sy-index.

    lv_text = iv_text.

    DO 4 TIMES.

      lv_index = sy-index.

      CALL FUNCTION 'TEXT_SPLIT'
        EXPORTING
          length = lc_length_of_msgv
          text   = lv_text
        IMPORTING
          line   = lv_msg_var
          rest   = lv_rest.

      IF lv_msg_var+lc_offset_of_last_character(1) = space OR lv_text+lc_length_of_msgv(1) = space.
        " keep the space at the beginning of the rest
        " because otherwise it's lost
        lv_rest = | { lv_rest }|.
      ENDIF.

      lv_text = lv_rest.

      CASE lv_index.
        WHEN 1.
          rs_msg-msgv1 = lv_msg_var.
        WHEN 2.
          rs_msg-msgv2 = lv_msg_var.
        WHEN 3.
          rs_msg-msgv3 = lv_msg_var.
        WHEN 4.
          rs_msg-msgv4 = lv_msg_var.
      ENDCASE.

    ENDDO.

  ENDMETHOD.
ENDCLASS.


CLASS zcx_certi DEFINITION
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcx_certi IMPLEMENTATION.
ENDCLASS.


*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

"! Constants taken from the standard include ICMDEF
INTERFACE lif_icmdef.

  CONSTANTS dp_plugin_op_chng_param TYPE i VALUE 8.

*---------------------------------------------------------------------
* Rueckgabewerte
* Sind mit icxx.h abzustimmen
* ...
*---------------------------------------------------------------------
  CONSTANTS icmeok       LIKE sy-subrc VALUE 0.
  CONSTANTS icmenotavail LIKE sy-subrc VALUE -6.

ENDINTERFACE.

"! Constants taken from the standard include TSKHINCL
INTERFACE lif_tskhincl.
***INCLUDE TSKHINCL.

*-------------------------------------------------------------------*/
* Constants for calls of the taskhandler-C-functions
*-------------------------------------------------------------------*/


*-------------------------------------------------------------------*/
* reference fields
*-------------------------------------------------------------------*/
  DATA th_opcode(1) TYPE x.
*--------------------------------------------------------------------*/
* Constants for calling function ThSysInfo
*--------------------------------------------------------------------*/
  ##NEEDED
  CONSTANTS opcode_icm LIKE th_opcode VALUE 35.
ENDINTERFACE.


CLASS zcl_certi_icm_trace DEFINITION
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    TYPES tt_icm_lines TYPE STANDARD TABLE OF icm_lines WITH DEFAULT KEY.
    TYPES tv_certificate_line  TYPE c LENGTH 64.
    TYPES tt_certificate_line TYPE STANDARD TABLE OF tv_certificate_line WITH EMPTY KEY.
    TYPES:
      BEGIN OF ts_certificate,
        lines TYPE tt_certificate_line,
      END OF ts_certificate.
    TYPES tt_certificate TYPE STANDARD TABLE OF ts_certificate WITH DEFAULT KEY.
    TYPES:
      BEGIN OF ts_parsed_trace,
        certificates TYPE tt_certificate,
      END OF ts_parsed_trace.

    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO zcl_certi_icm_trace.

    METHODS delete_trace.

    METHODS get_parsed_trace
      RETURNING
        VALUE(result) TYPE ts_parsed_trace.

    METHODS get_raw_trace
      RETURNING
        VALUE(result) TYPE tt_icm_lines.

    METHODS get_trace_level
      RETURNING
        VALUE(result) TYPE icm_info-trace_lvl.

    METHODS set_trace_level
      IMPORTING
        value TYPE icm_info-trace_lvl DEFAULT 0.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES tt_icm_sinfo2 TYPE STANDARD TABLE OF icm_sinfo2 WITH DEFAULT KEY.

ENDCLASS.



CLASS zcl_certi_icm_trace IMPLEMENTATION.


  METHOD create.
    result = NEW #( ).
  ENDMETHOD.


  METHOD delete_trace.
    CALL FUNCTION 'ICM_RESET_TRACE'
      EXCEPTIONS
        icm_op_failed      = 1
        icm_not_authorized = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      " TODO
    ENDIF.
  ENDMETHOD.


  METHOD get_parsed_trace.
    TYPES:
      BEGIN OF ts_trace_line,
        thread TYPE string,
        text   TYPE string,
      END OF ts_trace_line.
    TYPES tt_trace_line TYPE STANDARD TABLE OF ts_trace_line WITH EMPTY KEY.

    DATA(raw_trace) = get_raw_trace( ).

    FIND ALL OCCURRENCES OF REGEX `^\[Thr ([^\]]+)\] +([^ ].*)$`
        IN TABLE raw_trace
        RESULTS DATA(matches).

    DATA(trace_lines) = VALUE tt_trace_line( ).
    LOOP AT matches REFERENCE INTO DATA(match).
      DATA(raw_trace_line) = REF #( raw_trace[ match->line ] ).
      INSERT VALUE #( thread = substring( val = raw_trace_line->lines
                                          off = match->submatches[ 1 ]-offset
                                          len = match->submatches[ 1 ]-length )
                      text   = substring( val = raw_trace_line->lines
                                          off = match->submatches[ 2 ]-offset ) )
            INTO TABLE trace_lines.
    ENDLOOP.

    TYPES:
      BEGIN OF ts_certificate_in_trace,
        begin_line TYPE sytabix,
        end_line   TYPE sytabix,
      END OF ts_certificate_in_trace.
    TYPES tt_certificate_in_trace TYPE STANDARD TABLE OF ts_certificate_in_trace WITH EMPTY KEY.

    DATA(certificates_in_trace) = VALUE tt_certificate_in_trace( ).
    LOOP AT trace_lines REFERENCE INTO DATA(trace_line)
        WHERE text = `-----BEGIN CERTIFICATE-----`
           OR text = `-----END CERTIFICATE-----`.
      CASE substring( val = trace_line->text
                      off = 5
                      len = 3 ).
        WHEN 'BEG'.
          DATA(certificate_in_trace) = VALUE ts_certificate_in_trace( begin_line = sy-tabix ).
        WHEN 'END'.
          certificate_in_trace-end_line = sy-tabix.
          INSERT certificate_in_trace INTO TABLE certificates_in_trace.
      ENDCASE.
    ENDLOOP.

    LOOP AT certificates_in_trace ASSIGNING FIELD-SYMBOL(<certificate_in_trace>).
      INSERT VALUE #( lines = REDUCE #( INIT t = VALUE tt_certificate_line( )
                                        FOR <trace_line> IN trace_lines
                                            FROM <certificate_in_trace>-begin_line
                                            TO   <certificate_in_trace>-end_line
                                        NEXT t = VALUE #( BASE t
                                                          ( CONV #( <trace_line>-text ) ) ) ) )
            INTO TABLE result-certificates.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_raw_trace.
    " Credit: subroutine ICMAN_SHOW_TRACE in RSMONICM.
    DATA p_filename      TYPE icm_lines-lines VALUE 'dev_icm'.
    DATA p_first_n_lines TYPE i               VALUE 0.
    DATA p_last_n_lines  TYPE i               VALUE 0.
    CALL FUNCTION 'ICM_READ_TRC_FILE2'
      EXPORTING
        icm_filename      = p_filename
        icm_last_n_lines  = p_last_n_lines
        icm_first_n_lines = p_first_n_lines
      TABLES
        lines             = result
      EXCEPTIONS
        icm_open_failed   = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      " TODO
    ENDIF.
  ENDMETHOD.


  METHOD get_trace_level.
    DATA(icm_info_data) = VALUE icm_info( ).
    DATA(dummy_icm_servlist2) = VALUE tt_icm_sinfo2( ).
    CALL FUNCTION 'ICM_GET_INFO2'
      IMPORTING
        info_data   = icm_info_data
      TABLES
        servlist    = dummy_icm_servlist2
*       THRLIST     = ICM_THRLIST
*       SERVLIST3   = ICM_SERVLIST
      EXCEPTIONS
        icm_error   = 1
        icm_timeout = 2
        OTHERS      = 6.
    IF sy-subrc <> 0.
      " TODO
    ENDIF.
    result = icm_info_data-trace_lvl.
  ENDMETHOD.


  METHOD set_trace_level.
    " Credit: subroutine ICMAN_SET_PARAM in RSMONICM.
    DATA p_name  TYPE pfeparname VALUE 'rdisp/TRACE'.
    DATA rc TYPE sysubrc.

    DATA(p_value) = CONV pfepvalue( value ).
    CALL 'ThSysInfo'                                      "#EC CI_CCALL
         ID 'OPCODE'    FIELD lif_tskhincl=>opcode_icm
         ID 'ICMOPCODE' FIELD lif_icmdef=>dp_plugin_op_chng_param
         ID 'PNAME'     FIELD p_name
         ID 'PVALUE'    FIELD p_value.
    IF sy-subrc <> 0.
      " TODO replace with exception
      CASE sy-subrc.
*      WHEN lif_icmdef=>icmeok.
*        MESSAGE s005(icm).
        WHEN lif_icmdef=>icmenotavail.
          MESSAGE i032(icm).
        WHEN OTHERS.
          MESSAGE e006(icm) WITH rc.
      ENDCASE.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS zcl_certi_strust DEFINITION
  FINAL
  CREATE PUBLIC .

************************************************************************
* Trust Management
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

    TYPES:
      ty_line        TYPE c LENGTH 80,
      ty_certificate TYPE STANDARD TABLE OF ty_line WITH DEFAULT KEY,
      BEGIN OF ty_certattr,
        subject     TYPE string,
        issuer      TYPE string,
        serialno    TYPE string,
        validfrom   TYPE string,
        validto     TYPE string,
        datefrom    TYPE d,
        dateto      TYPE d,
        certificate TYPE xstring,
      END OF ty_certattr,
      ty_certattr_tt TYPE STANDARD TABLE OF ty_certattr WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING
        !iv_context TYPE psecontext
        !iv_applic  TYPE ssfappl
      RAISING
        zcx_certi_strust.

    METHODS load
      IMPORTING
        !iv_create TYPE abap_bool DEFAULT abap_false
        !iv_id     TYPE ssfid OPTIONAL
        !iv_org    TYPE string OPTIONAL
      RAISING
        zcx_certi_strust.

    METHODS add
      IMPORTING
        !it_certificate TYPE ty_certificate
      RAISING
        zcx_certi_strust.

    METHODS get_own_certificate
      RETURNING
        VALUE(rs_result) TYPE ty_certattr
      RAISING
        zcx_certi_strust.

    METHODS get_certificate_list
      RETURNING
        VALUE(rt_result) TYPE ty_certattr_tt
      RAISING
        zcx_certi_strust.

    METHODS remove
      IMPORTING
        VALUE(iv_subject) TYPE string
      RAISING
        zcx_certi_strust.

    METHODS update
      RETURNING
        VALUE(rt_result) TYPE ty_certattr_tt
      RAISING
        zcx_certi_strust.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      mv_context   TYPE psecontext,
      mv_applic    TYPE ssfappl,
      mv_psename   TYPE ssfpsename,
      mv_psetext   TYPE strustappltxt ##NEEDED,
      mv_distrib   TYPE ssfflag,
      mv_tempfile  TYPE localfile,
      mv_id        TYPE ssfid,
      mv_profile   TYPE ssfpab,
      mv_profilepw TYPE ssfpabpw,
      mv_cert_own  TYPE xstring,
      mt_cert_new  TYPE ty_certattr_tt,
      ms_cert_old  TYPE ty_certattr,
      mt_cert_old  TYPE ty_certattr_tt,
      mv_save      TYPE abap_bool.

    METHODS _create
      IMPORTING
        !iv_id  TYPE ssfid OPTIONAL
        !iv_org TYPE string OPTIONAL
      RAISING
        zcx_certi_strust.

    METHODS _lock
      RAISING
        zcx_certi_strust.

    METHODS _unlock
      RAISING
        zcx_certi_strust.

    METHODS _save
      RAISING
        zcx_certi_strust.

ENDCLASS.



CLASS zcl_certi_strust IMPLEMENTATION.


  METHOD add.

    DATA:
      lv_certb64  TYPE string,
      lo_certobj  TYPE REF TO cl_abap_x509_certificate,
      ls_cert_new TYPE ty_certattr.

    FIELD-SYMBOLS:
      <lv_data> TYPE any.

    CONCATENATE LINES OF it_certificate INTO lv_certb64.

    " Remove Header and Footer
    TRY.
        FIND REGEX '-{5}.{0,}BEGIN.{0,}-{5}(.*)-{5}.{0,}END.{0,}-{5}' IN lv_certb64 SUBMATCHES lv_certb64.
        IF sy-subrc = 0.
          ASSIGN lv_certb64 TO <lv_data>.
          ASSERT sy-subrc = 0.
        ELSE.
          zcx_certi_strust=>raise( 'Inconsistent certificate format'(010) ).
        ENDIF.
      CATCH cx_sy_regex_too_complex.
        " e.g. multiple PEM frames in file
        zcx_certi_strust=>raise( 'Inconsistent certificate format'(010) ).
    ENDTRY.

    TRY.
        CREATE OBJECT lo_certobj
          EXPORTING
            if_certificate = <lv_data>.

        ls_cert_new-certificate = lo_certobj->get_certificate( ).

        CALL FUNCTION 'SSFC_PARSE_CERTIFICATE'
          EXPORTING
            certificate         = ls_cert_new-certificate
          IMPORTING
            subject             = ls_cert_new-subject
            issuer              = ls_cert_new-issuer
            serialno            = ls_cert_new-serialno
            validfrom           = ls_cert_new-validfrom
            validto             = ls_cert_new-validto
          EXCEPTIONS
            ssf_krn_error       = 1
            ssf_krn_nomemory    = 2
            ssf_krn_nossflib    = 3
            ssf_krn_invalid_par = 4
            OTHERS              = 5.
        IF sy-subrc <> 0.
          _unlock( ).
          zcx_certi_strust=>raise_t100( ).
        ENDIF.

        ls_cert_new-datefrom = ls_cert_new-validfrom(8).
        ls_cert_new-dateto   = ls_cert_new-validto(8).
        APPEND ls_cert_new TO mt_cert_new.

      CATCH cx_abap_x509_certificate.
        _unlock( ).
        zcx_certi_strust=>raise_t100( ).
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.

    mv_context = iv_context.
    mv_applic  = iv_applic.

    CALL FUNCTION 'SSFPSE_FILENAME'
      EXPORTING
        context       = mv_context
        applic        = mv_applic
      IMPORTING
        psename       = mv_psename
        psetext       = mv_psetext
        distrib       = mv_distrib
      EXCEPTIONS
        pse_not_found = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      zcx_certi_strust=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_certificate_list.

    DATA:
      lt_certlist TYPE ssfbintab,
      ls_cert_old TYPE ty_certattr.

    FIELD-SYMBOLS <lv_certlist> LIKE LINE OF lt_certlist.

    CALL FUNCTION 'SSFC_GET_CERTIFICATELIST'
      EXPORTING
        profile               = mv_profile
        profilepw             = mv_profilepw
      IMPORTING
        certificatelist       = lt_certlist
      EXCEPTIONS
        ssf_krn_error         = 1
        ssf_krn_nomemory      = 2
        ssf_krn_nossflib      = 3
        ssf_krn_invalid_par   = 4
        ssf_krn_nocertificate = 5
        OTHERS                = 6.
    IF sy-subrc <> 0.
      _unlock( ).
      zcx_certi_strust=>raise_t100( ).
    ENDIF.

    LOOP AT lt_certlist ASSIGNING <lv_certlist>.

      CLEAR ls_cert_old.

      CALL FUNCTION 'SSFC_PARSE_CERTIFICATE'
        EXPORTING
          certificate         = <lv_certlist>
        IMPORTING
          subject             = ls_cert_old-subject
          issuer              = ls_cert_old-issuer
          serialno            = ls_cert_old-serialno
          validfrom           = ls_cert_old-validfrom
          validto             = ls_cert_old-validto
        EXCEPTIONS
          ssf_krn_error       = 1
          ssf_krn_nomemory    = 2
          ssf_krn_nossflib    = 3
          ssf_krn_invalid_par = 4
          OTHERS              = 5.
      IF sy-subrc <> 0.
        _unlock( ).
        zcx_certi_strust=>raise_t100( ).
      ENDIF.

      ls_cert_old-datefrom = ls_cert_old-validfrom(8).
      ls_cert_old-dateto   = ls_cert_old-validto(8).
      APPEND ls_cert_old TO mt_cert_old.

    ENDLOOP.

    rt_result = mt_cert_old.

  ENDMETHOD.


  METHOD get_own_certificate.

    mv_profile = mv_tempfile.

    CALL FUNCTION 'SSFC_GET_OWNCERTIFICATE'
      EXPORTING
        profile               = mv_profile
        profilepw             = mv_profilepw
      IMPORTING
        certificate           = mv_cert_own
      EXCEPTIONS
        ssf_krn_error         = 1
        ssf_krn_nomemory      = 2
        ssf_krn_nossflib      = 3
        ssf_krn_invalid_par   = 4
        ssf_krn_nocertificate = 5
        OTHERS                = 6.
    IF sy-subrc <> 0.
      _unlock( ).
      zcx_certi_strust=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'SSFC_PARSE_CERTIFICATE'
      EXPORTING
        certificate         = mv_cert_own
      IMPORTING
        subject             = ms_cert_old-subject
        issuer              = ms_cert_old-issuer
        serialno            = ms_cert_old-serialno
        validfrom           = ms_cert_old-validfrom
        validto             = ms_cert_old-validto
      EXCEPTIONS
        ssf_krn_error       = 1
        ssf_krn_nomemory    = 2
        ssf_krn_nossflib    = 3
        ssf_krn_invalid_par = 4
        OTHERS              = 5.
    IF sy-subrc <> 0.
      _unlock( ).
      zcx_certi_strust=>raise_t100( ).
    ENDIF.

    ms_cert_old-datefrom = ms_cert_old-validfrom(8).
    ms_cert_old-dateto   = ms_cert_old-validto(8).

    rs_result = ms_cert_old.

  ENDMETHOD.


  METHOD load.

    CLEAR mv_save.

    _lock( ).

    CALL FUNCTION 'SSFPSE_LOAD'
      EXPORTING
        psename           = mv_psename
      IMPORTING
        id                = mv_id
        fname             = mv_tempfile
      EXCEPTIONS
        authority_missing = 1
        database_failed   = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      IF iv_create = abap_true.
        _create(
          iv_id  = iv_id
          iv_org = iv_org ).
      ELSE.
        zcx_certi_strust=>raise_t100( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD remove.

    FIELD-SYMBOLS:
      <ls_cert_old> LIKE LINE OF mt_cert_old.

    " Remove certificate
    LOOP AT mt_cert_old ASSIGNING <ls_cert_old> WHERE subject = iv_subject.

      CALL FUNCTION 'SSFC_REMOVECERTIFICATE'
        EXPORTING
          profile               = mv_profile
          profilepw             = mv_profilepw
          subject               = <ls_cert_old>-subject
          issuer                = <ls_cert_old>-issuer
          serialno              = <ls_cert_old>-serialno
        EXCEPTIONS
          ssf_krn_error         = 1
          ssf_krn_nomemory      = 2
          ssf_krn_nossflib      = 3
          ssf_krn_invalid_par   = 4
          ssf_krn_nocertificate = 5
          OTHERS                = 6.
      IF sy-subrc <> 0.
        _unlock( ).
        zcx_certi_strust=>raise_t100( ).
      ENDIF.

      mv_save = abap_true.
    ENDLOOP.

    _save( ).

    _unlock( ).

  ENDMETHOD.


  METHOD update.

    FIELD-SYMBOLS:
      <ls_cert_old> LIKE LINE OF mt_cert_old,
      <ls_cert_new> LIKE LINE OF mt_cert_new.

    " Remove expired certificates
    LOOP AT mt_cert_old ASSIGNING <ls_cert_old>.

      LOOP AT mt_cert_new ASSIGNING <ls_cert_new> WHERE subject = <ls_cert_old>-subject.

        IF <ls_cert_new>-dateto > <ls_cert_old>-dateto.
          " Certificate is newer, so remove the old certificate
          CALL FUNCTION 'SSFC_REMOVECERTIFICATE'
            EXPORTING
              profile               = mv_profile
              profilepw             = mv_profilepw
              subject               = <ls_cert_old>-subject
              issuer                = <ls_cert_old>-issuer
              serialno              = <ls_cert_old>-serialno
            EXCEPTIONS
              ssf_krn_error         = 1
              ssf_krn_nomemory      = 2
              ssf_krn_nossflib      = 3
              ssf_krn_invalid_par   = 4
              ssf_krn_nocertificate = 5
              OTHERS                = 6.
          IF sy-subrc <> 0.
            _unlock( ).
            zcx_certi_strust=>raise_t100( ).
          ENDIF.

          mv_save = abap_true.
        ELSE.
          " Certificate already exists, no update necessary
          DELETE mt_cert_new.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

    " Add new certificates to PSE
    LOOP AT mt_cert_new ASSIGNING <ls_cert_new>.

      CALL FUNCTION 'SSFC_PUT_CERTIFICATE'
        EXPORTING
          profile             = mv_profile
          profilepw           = mv_profilepw
          certificate         = <ls_cert_new>-certificate
        EXCEPTIONS
          ssf_krn_error       = 1
          ssf_krn_nomemory    = 2
          ssf_krn_nossflib    = 3
          ssf_krn_invalid_par = 4
          ssf_krn_certexists  = 5
          OTHERS              = 6.
      IF sy-subrc <> 0.
        _unlock( ).
        zcx_certi_strust=>raise_t100( ).
      ENDIF.

      mv_save = abap_true.
    ENDLOOP.

    _save( ).

    _unlock( ).

    rt_result = mt_cert_new.

  ENDMETHOD.


  METHOD _create.

    DATA:
      lv_license_num TYPE c LENGTH 10,
      lv_id          TYPE ssfid,
      lv_subject     TYPE certsubjct,
      lv_psepath     TYPE trfile.

*   Create new PSE (using RSA-SHA256 2048 which is the default in STRUST in recent releases)
    IF iv_id IS INITIAL.
      CASE mv_applic.
        WHEN 'DFAULT'.
          lv_id = `CN=%SID SSL client SSL Client (Standard), ` &&
                  `OU=I%LIC, OU=SAP Web AS, O=SAP Trust Community, C=DE` ##NO_TEXT.
        WHEN 'ANONYM'.
          lv_id = 'CN=anonymous' ##NO_TEXT.
      ENDCASE.
    ELSE.
      lv_id = iv_id.
    ENDIF.

    CALL FUNCTION 'SLIC_GET_LICENCE_NUMBER'
      IMPORTING
        license_number = lv_license_num.

    REPLACE '%SID' WITH sy-sysid INTO lv_id.
    REPLACE '%LIC' WITH lv_license_num INTO lv_id.
    REPLACE '%ORG' WITH iv_org INTO lv_id.
    CONDENSE lv_id.

    lv_subject = lv_id.

    CALL FUNCTION 'SSFPSE_CREATE'
      EXPORTING
        dn                = lv_subject
        alg               = 'R'
        keylen            = 2048
      IMPORTING
        psepath           = lv_psepath
      EXCEPTIONS
        ssf_unknown_error = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      zcx_certi_strust=>raise_t100( ).
    ENDIF.

    mv_tempfile = lv_psepath.

    _save( ).

  ENDMETHOD.


  METHOD _lock.

    CALL FUNCTION 'SSFPSE_ENQUEUE'
      EXPORTING
        psename         = mv_psename
      EXCEPTIONS
        database_failed = 1
        foreign_lock    = 2
        internal_error  = 3
        OTHERS          = 4.
    IF sy-subrc <> 0.
      zcx_certi_strust=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD _save.

    DATA lv_credname TYPE icm_credname.

    CHECK mv_save = abap_true.

    " Store PSE
    CALL FUNCTION 'SSFPSE_STORE'
      EXPORTING
        fname             = mv_tempfile
        psepin            = mv_profilepw
        psename           = mv_psename
        id                = mv_id
        b_newdn           = abap_false
        b_distribute      = mv_distrib
      EXCEPTIONS
        file_load_failed  = 1
        storing_failed    = 2
        authority_missing = 3
        OTHERS            = 4.
    IF sy-subrc <> 0.
      _unlock( ).
      zcx_certi_strust=>raise_t100( ).
    ENDIF.

    lv_credname = mv_psename.

    CALL FUNCTION 'ICM_SSL_PSE_CHANGED'
      EXPORTING
        global              = 1
        cred_name           = lv_credname
      EXCEPTIONS
        icm_op_failed       = 1
        icm_get_serv_failed = 2
        icm_auth_failed     = 3
        OTHERS              = 4.
    IF sy-subrc = 0.
      MESSAGE s086(trust).
    ELSE.
      MESSAGE s085(trust).
    ENDIF.

  ENDMETHOD.



  METHOD _unlock.

    " Drop temporary file
    TRY.
        DELETE DATASET mv_tempfile.
      CATCH cx_sy_file_open.
        zcx_certi_strust=>raise( 'Error deleting file'(020) && | { mv_tempfile }| ).
      CATCH cx_sy_file_authority.
        zcx_certi_strust=>raise( 'Not authorized to delete file'(030) && | { mv_tempfile }| ).
    ENDTRY.

    " Unlock PSE
    CALL FUNCTION 'SSFPSE_DEQUEUE'
      EXPORTING
        psename         = mv_psename
      EXCEPTIONS
        database_failed = 1
        foreign_lock    = 2
        internal_error  = 3
        OTHERS          = 4.
    IF sy-subrc <> 0.
      zcx_certi_strust=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.

**********************************************************************
* End of include from https://github.com/sandraros/zcerti
**********************************************************************


*--------------------------------------------------------------------*
CLASS sslcert DEFINITION.
*--------------------------------------------------------------------*

  PUBLIC SECTION.
    METHODS constructor.
    METHODS execute IMPORTING host TYPE string.
    METHODS exists  IMPORTING host          TYPE string
                    RETURNING VALUE(result) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA strust TYPE REF TO zcl_certi_strust.
    DATA out TYPE REF TO output.

ENDCLASS.


*--------------------------------------------------------------------*
CLASS sslcert IMPLEMENTATION.
*--------------------------------------------------------------------*

  METHOD execute.

    out = output=>get_instance( ).

    IF exists( host ).
      out->write( |Certificate for { host } already exists, not imported| ).
      RETURN.
    ENDIF.

    out->write( |Importing certificate for { host }...| ).

    DATA(icm_trace_api) = zcl_certi_icm_trace=>create( ).

    " Get current ICM trace level for later restore
    DATA(original_trace_level) = icm_trace_api->get_trace_level( ).

    " Set the ICM trace level to 3 to obtain the contents of certificates
    icm_trace_api->set_trace_level( '3' ).

    " Clear the trace
    icm_trace_api->delete_trace( ).

    "===================================
    " HTTPS GET
    "===================================
    cl_http_client=>create_by_url( EXPORTING  url    = `https://` && host
                                   IMPORTING  client = DATA(http_client)
                                   EXCEPTIONS OTHERS = 1 ).

    DATA(request) = http_client->request.
    request->set_method( 'GET' ).
    request->set_version( if_http_request=>co_protocol_version_1_1 ). " HTTP 1.0 or 1.1

    http_client->send( EXCEPTIONS OTHERS = 1 ).
    http_client->receive( EXCEPTIONS OTHERS = 1 ).
    http_client->response->get_status( IMPORTING code   = DATA(return_code)
                                                 reason = DATA(reason) ).
    IF return_code <> 200 AND return_code <> 500.
      out->write( |Could not reach host { host }, returned { return_code }: { reason }| ).
      RETURN.
    ENDIF.

    http_client->response->get_cdata( ).
    http_client->close( EXCEPTIONS OTHERS = 1 ).

    " Get and parse the ICM trace
    DATA(parsed_icm_trace) = icm_trace_api->get_parsed_trace( ).

    " Restore original ICM trace level
    icm_trace_api->set_trace_level( original_trace_level ).

    TRY.
        LOOP AT parsed_icm_trace-certificates REFERENCE INTO DATA(certificate).
          strust->add( CONV #( certificate->lines ) ).
        ENDLOOP.
        strust->update( ).
        COMMIT WORK.

      CATCH zcx_certi_strust INTO DATA(strust_error).
        MESSAGE strust_error TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
    out->add_to_last( `done.` ).

  ENDMETHOD.


  METHOD exists.

    TRY.

        " initialize MV_PROFILE from MV_TEMPFILE
        strust->get_own_certificate( ).
        " get list only after MV_PROFILE is initialized
        DATA(certificates) = strust->get_certificate_list( ).

        LOOP AT certificates TRANSPORTING NO FIELDS WHERE subject CS host.
        ENDLOOP.
        IF sy-subrc = 0.
          result = abap_true.
        ENDIF.

      CATCH zcx_certi_strust INTO DATA(error).
        out->write( |SSL existence check failed: { error->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.

    TRY.
        out = NEW output( ).

        strust = NEW zcl_certi_strust( iv_context = 'SSLC'
                                       iv_applic  = 'ANONYM' ).
        strust->load( ).

      CATCH zcx_certi_strust INTO DATA(error).
        out->write( |zcl_certi_strust error: { error->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.


*--------------------------------------------------------------------*
CLASS user_profile DEFINITION CREATE PUBLIC.
*--------------------------------------------------------------------*

  PUBLIC SECTION.

    CONSTANTS: BEGIN OF date_format,
                 dmy_dot   TYPE xudatfm VALUE '1',
                 mdy_slash TYPE xudatfm VALUE '2',
                 mdy_dash  TYPE xudatfm VALUE '3',
                 ymd_dot   TYPE xudatfm VALUE '4',
                 ymd_slash TYPE xudatfm VALUE '5',
                 ymd_dash  TYPE xudatfm VALUE '6',
               END OF date_format.

    CONSTANTS: BEGIN OF decimal_format,
                 comma             TYPE xudcpfm VALUE space,  "1.000.000,00
                 comma_with_spaces TYPE xudcpfm VALUE 'Y',    "1 000 000,00
                 point             TYPE xudcpfm VALUE 'X',    "1,000,000.00
               END OF decimal_format.

    TYPES: BEGIN OF profile_type,
             username          TYPE xubname,
             firstname         TYPE ad_namefir,
             lastname          TYPE ad_namelas,
             email             TYPE ad_smtpadr,
             date_format       TYPE xudatfm,
             decimal_format    TYPE xudcpfm,
             hide_menu_picture TYPE abap_bool, "Show picture on main menu
             show_menu_tcodes  TYPE abap_bool, "Show transaction codes on main menu
           END OF profile_type.

    METHODS execute IMPORTING profile TYPE profile_type.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA out TYPE REF TO output.

    METHODS update_user IMPORTING profile TYPE profile_type
                        RAISING   lcx_error.

    METHODS update_menu IMPORTING profile TYPE profile_type
                        RAISING   lcx_error.

ENDCLASS.


*--------------------------------------------------------------------*
CLASS user_profile IMPLEMENTATION.
*--------------------------------------------------------------------*

  METHOD execute.

    out = NEW output( ).

    out->write( 'Updating user profile...' ).

    TRY.
        update_user( profile ).
      CATCH lcx_error.
        out->write( `Error updating profile` ).
        out->write( 'Updating user settings...' ).
    ENDTRY.

    TRY.
        update_menu( profile ).
      CATCH lcx_error.
        out->write( `Error updating settings` ).
        RETURN.
    ENDTRY.

    out->add_to_last( 'done.' ).

  ENDMETHOD.


  METHOD update_user.

    DATA address TYPE bapiaddr3.
    DATA addressx TYPE bapiaddr3x.
    DATA return TYPE bapirettab.
    DATA defaults TYPE bapidefaul.
    DATA defaultsx TYPE bapidefax.

    defaults-datfm  = profile-date_format.
    defaultsx-datfm = profile-date_format.

    defaults-dcpfm  = profile-decimal_format.
    defaultsx-dcpfm = profile-date_format.

    address-firstname  = profile-firstname.
    address-lastname   = profile-lastname.
    addressx-firstname = abap_true.
    addressx-lastname  = abap_true.
    address-e_mail = profile-email.
    addressx-e_mail = 'X'.

    DATA(username) = COND #(
      WHEN profile-username IS INITIAL
      THEN sy-uname
      ELSE profile-username ).

    CALL FUNCTION 'BAPI_USER_CHANGE'
      EXPORTING
        username  = username
        defaults  = defaults
        defaultsx = defaultsx
        address   = address
        addressx  = addressx
      TABLES
        return    = return.

    LOOP AT return INTO DATA(msg) WHERE type CA 'AEX'.
      out->write( msg-message ).
      RAISE EXCEPTION NEW lcx_error( ).
    ENDLOOP.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

  ENDMETHOD.


  METHOD update_menu.

    DATA: flag1 TYPE c,
          flag2 TYPE c,
          flag3 TYPE c,
          flag4 TYPE c,
          flag5 TYPE c,
          flag6 TYPE c,
          flag7 TYPE c,
          flag8 TYPE c,
          flag9 TYPE c.

    CALL FUNCTION 'PRGN_GET_BROWSER_OPTIONS_USER'
      EXPORTING
        uname                = sy-uname
      IMPORTING
        flag1                = flag1
        flag2                = flag2
        flag3                = flag3
        flag4                = flag4
        flag5                = flag5
        flag6                = flag6
        flag7                = flag7
        flag8                = flag8
        flag9                = flag9
      EXCEPTIONS
        no_options_available = 1
        OTHERS               = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW lcx_error( ).
    ENDIF.

    flag3 = profile-hide_menu_picture.
    flag4 = profile-show_menu_tcodes.

    CALL FUNCTION 'PRGN_SET_BROWSER_OPTIONS_USER'
      EXPORTING
        uname = sy-uname
        flag1 = flag1
        flag2 = flag2
        flag3 = flag3
        flag4 = flag4
        flag5 = flag5
        flag6 = flag6
        flag7 = flag7
        flag8 = flag8
        flag9 = flag9.

  ENDMETHOD.

ENDCLASS.


*--------------------------------------------------------------------*
CLASS main DEFINITION CREATE PUBLIC.
*--------------------------------------------------------------------*

  PUBLIC SECTION.
    METHODS run.
    METHODS constructor.
    METHODS get_github_user RETURNING VALUE(result) TYPE rfcalias.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF repo_type,
             name    TYPE string,
             package TYPE devclass,
             url     TYPE string,
           END OF repo_type.

    DATA repos TYPE STANDARD TABLE OF repo_type WITH EMPTY KEY.
    DATA github_user  TYPE rfcalias.
    DATA github_token TYPE rfcexec_ext.
    DATA sslhosts TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    DATA profile TYPE user_profile=>profile_type.
    DATA out TYPE REF TO output.

    METHODS import_repos.

ENDCLASS.


*--------------------------------------------------------------------*
CLASS main IMPLEMENTATION.
*--------------------------------------------------------------------*

  METHOD constructor.

    out = output=>get_instance( ).

    "Customize setup here, or use include zdevsysinit_params
    "
    "Create include outside package (e.g. $tmp) if it should not go to GitHub
    "Remember to back it up locally!

    "To customize here, please see example values at:
    " https://github.com/pokrakam/abapDevSysInit/blob/main/zdevsysinit_params.sample.abap

    "Please note order of priority:
    " 1. Anything supplied in the selection screen (highest priority)
    " 2. Values specified in ZDEVSYSINIT_PARAMS
    " 3. Config values here (lowest prio)

*--- Begin customization section / default values / lowest prio

    sslhosts = VALUE #(
        ( `github.com` )
        ( `github.io` ) ).

*--- End customization section

    "Next prio
    INCLUDE zdevsysinit_params IF FOUND.

    "Parameters override everything
    IF p_ghuser IS NOT INITIAL.
      github_user = p_ghuser.
    ENDIF.

    IF p_token IS NOT INITIAL.
      github_token = p_token.
    ENDIF.

  ENDMETHOD.


  METHOD run.

    out->write( `Starting system configuration` ).

    IF p_usrpfl = abap_true.
      NEW user_profile( )->execute( profile ).
    ENDIF.

    IF p_rfcdst = abap_true.

      NEW rfc_destination( )->execute(
        name  = 'GITHUB'
        user  = github_user
        token = github_token ).

    ENDIF.

    IF p_certi = abap_true.

      DATA(cert) = NEW sslcert( ).
      LOOP AT sslhosts INTO DATA(host).
        cert->execute( host ).
      ENDLOOP.

    ENDIF.

    IF p_agxit = abap_true.

      TRY.
          NEW abapgit_exit( )->execute( ).
        CATCH lcx_error.
          out->write( `Could not write ZABAPGIT_USER_EXIT` ).
      ENDTRY.

    ENDIF.

    IF p_agstd = abap_true.

      TRY.
          NEW abapgit_standalone( )->execute( ).
        CATCH lcx_error.
          out->write( `Could not create ZABAPGIT_STANDALONE` ).
      ENDTRY.

    ENDIF.

    IF p_repos = abap_true.
      out->write( `Importing repos` ).
      import_repos( ).
    ENDIF.

    out->write( `Finished.` ).
    out->display( ).

  ENDMETHOD.


  METHOD import_repos.

    TRY.
        DATA(repo_importer) = NEW repo_import( ).
      CATCH lcx_error INTO DATA(error).
        out->write( |Could not get list of repos: { error->get_text( ) }| ).
        RETURN.
    ENDTRY.

    LOOP AT repos REFERENCE INTO DATA(repo).

      TRY.
          repo_importer->execute(
            name    = repo->name
            package = repo->package
            url     = repo->url ).
        CATCH lcx_error.
          out->write( |Could not pull repo { repo->name }| ).
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_github_user.
    result = github_user.
  ENDMETHOD.

ENDCLASS.


*--------------------------------------------------------------------*
CLASS initialization DEFINITION CREATE PUBLIC.
*--------------------------------------------------------------------*

  PUBLIC SECTION.
    METHODS propose_parameters.
    METHODS check_and_update_texts.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS insert_selection_texts.
    METHODS is_default_user_name
      RETURNING
        VALUE(result) TYPE abap_bool.

ENDCLASS.


*--------------------------------------------------------------------*
CLASS initialization IMPLEMENTATION.
*--------------------------------------------------------------------*

  METHOD propose_parameters.

    p_usrpfl = is_default_user_name( ).

    IF NOT rfc_destination=>exists( 'GITHUB' ).
      p_rfcdst = abap_true.
      p_ghuser = NEW main( )->get_github_user( ).
    ENDIF.

    IF NOT NEW sslcert( )->exists( `github.com` ).
      p_certi = abap_true.
    ENDIF.

    SELECT SINGLE @abap_true
           FROM reposrc
           WHERE progname = 'ZABAPGIT_STANDALONE'
           INTO @DATA(exists).

    p_agstd = xsdbool( exists = abap_false ).

    CLEAR exists.
    SELECT SINGLE @abap_true
           FROM reposrc
           WHERE progname = 'ZABAPGIT_USER_EXIT'
           INTO @exists.

    p_agxit = xsdbool( exists = abap_false ).

  ENDMETHOD.


  METHOD check_and_update_texts.

    DATA texts TYPE STANDARD TABLE OF textpool WITH EMPTY KEY.

    READ TEXTPOOL sy-repid INTO texts.

    IF NOT line_exists( texts[ id = 'S' ] ).
      insert_selection_texts( ).
      MESSAGE 'Texts updated, please restart program.' TYPE 'W' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD insert_selection_texts.

    DATA texts TYPE STANDARD TABLE OF textpool WITH EMPTY KEY.

    texts = VALUE #(
      ( id = 'R' entry = 'Setup Dev system' length = '16' )
      ( id = 'S' key = 'P_USRPFL'  entry = '        Update User Profile'            length = '27' )
      ( id = 'S' key = 'P_AGSTD'   entry = '        Get abapGit Standalone'         length = '30' )
      ( id = 'S' key = 'P_AGXIT'   entry = '        Create abapGit Standalone Exit' length = '38' )
      ( id = 'S' key = 'P_CERTI'   entry = '        Install SSL Certificates'       length = '32' )
      ( id = 'S' key = 'P_GHUSER'  entry = '        GitHub User'                    length = '19' )
      ( id = 'S' key = 'P_REPOS'   entry = '        Pull Repos (Overwrite local!)'  length = '29' )
      ( id = 'S' key = 'P_RFCDST'  entry = '        Set up GitHub RFC Destination'  length = '37' )
      ( id = 'S' key = 'P_TOKEN'   entry = '        GitHub Token'                   length = '21' ) ).

    INSERT TEXTPOOL sy-repid FROM texts.

  ENDMETHOD.


  METHOD is_default_user_name.

    DATA: address TYPE bapiaddr3,
          return  TYPE STANDARD TABLE OF bapiret2 WITH EMPTY KEY.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = sy-uname
      IMPORTING
        address  = address
      TABLES
        return   = return.

    result = xsdbool( address-fullname = 'John Doe' ).

  ENDMETHOD.

ENDCLASS.


*--------------------------------------------------------------------*
INITIALIZATION.
*--------------------------------------------------------------------*

  DATA(init) = NEW initialization( ).
  init->check_and_update_texts( ).
  init->propose_parameters( ).

*--------------------------------------------------------------------*
START-OF-SELECTION.
*--------------------------------------------------------------------*

  NEW main( )->run( ).


**********************************************************************
* Tests
**********************************************************************


*--------------------------------------------------------------------*
CLASS ltc_ag_standalone DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM abapgit_standalone.
*--------------------------------------------------------------------*

  PROTECTED SECTION.
    METHODS get_source REDEFINITION.
    METHODS insert_report REDEFINITION.

  PRIVATE SECTION.
    DATA report_lines TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    METHODS setup.

    METHODS response_content_ok FOR TESTING RAISING cx_static_check.
    METHODS report_inserted FOR TESTING RAISING cx_static_check.
    METHODS response_received FOR TESTING RAISING cx_static_check.

ENDCLASS.


*--------------------------------------------------------------------*
CLASS ltc_ag_standalone IMPLEMENTATION.
*--------------------------------------------------------------------*

  METHOD setup.
    out = output=>get_instance( ).
    url = `https://raw.githubusercontent.com/pokrakam/abapDevSysInit/main/LICENSE`.
  ENDMETHOD.


  METHOD response_received.
    cl_abap_unit_assert=>assert_not_initial( get_source( ) ).
  ENDMETHOD.


  METHOD response_content_ok.
    source = get_source( ).
    DATA(header) = substring( val = source
                              len = 11 ).
    cl_abap_unit_assert=>assert_equals( act = header
                                        exp = `MIT License` ).
  ENDMETHOD.


  METHOD report_inserted.

    DATA(local_source) = |REPORT zabapgit_standalone.\n{ get_source( ) }|.

    source_lines = validate_and_split_source( local_source ).

    insert_report( 'FOO' ).

    cl_abap_unit_assert=>assert_not_initial( report_lines ).
    cl_abap_unit_assert=>assert_table_contains( line  = `SOFTWARE.`
                                                table = report_lines ).

  ENDMETHOD.


  METHOD insert_report.
    report_lines = source_lines.
  ENDMETHOD.


  METHOD get_source.
    "Bit hacky, but let's only go out to GitHub once per test run
    IF source IS INITIAL.
      source = super->get_source( ).
    ENDIF.
    result = source.
  ENDMETHOD.

ENDCLASS.


*--------------------------------------------------------------------*
CLASS ltc_rfcdest DEFINITION FINAL FOR TESTING
  INHERITING FROM rfc_destination
  DURATION SHORT
  RISK LEVEL HARMLESS.
*--------------------------------------------------------------------*

  PROTECTED SECTION.
    METHODS create_rfc_destination REDEFINITION.
    METHODS delete_rfc_destination REDEFINITION.
    METHODS check_existence REDEFINITION.

  PRIVATE SECTION.
    DATA: created   TYPE abap_bool,
          deleted   TYPE abap_bool,
          td_exists TYPE abap_bool.

    METHODS create_if_not_exists FOR TESTING RAISING cx_static_check.
    METHODS no_create_if_exists FOR TESTING RAISING cx_static_check.
    METHODS recreate_if_reset FOR TESTING RAISING cx_static_check.

ENDCLASS.


*--------------------------------------------------------------------*
CLASS ltc_rfcdest IMPLEMENTATION.
*--------------------------------------------------------------------*


  METHOD create_if_not_exists.

    td_exists = abap_false.

    execute(
      name  = ''
      user  = ''
      token = '' ).

    cl_abap_unit_assert=>assert_true( created ).

  ENDMETHOD.


  METHOD no_create_if_exists.
    td_exists = abap_true.
    execute(
      name  = ''
      user  = ''
      token = '' ).
    cl_abap_unit_assert=>assert_false( created ).
  ENDMETHOD.


  METHOD recreate_if_reset.

    td_exists = abap_true.

    execute(
      name  = ''
      user  = ''
      token = ''
      reset = abap_true ).

    cl_abap_unit_assert=>assert_true( deleted ).
    cl_abap_unit_assert=>assert_true( created ).

  ENDMETHOD.


  METHOD create_rfc_destination.
    created = abap_true.
  ENDMETHOD.


  METHOD delete_rfc_destination.
    deleted = abap_true.
  ENDMETHOD.


  METHOD check_existence.
    result = td_exists.
  ENDMETHOD.


ENDCLASS.
