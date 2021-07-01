*
* This is version 0.5.1 for 7.4 upwards
*
*The MIT License (MIT)
*
*Copyright (c) 2021 Matthew Billingham
*
*Permission is hereby granted, free of charge, to any person obtaining a copy
*of this software and associated documentation files (the "Software"), to deal
*in the Software without restriction, including without limitation the rights
*to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*copies of the Software, and to permit persons to whom the Software is
*furnished to do so, subject to the following conditions:
*
*The above copyright notice and this permission notice shall be included in all
*copies or substantial portions of the Software.
*
*Adapted from zi_abap_to_codecharta (c) 2021 Benjamin Wiesheit
*Dependent on https://github.com/BenjaminWeisheit/ABAP-2-CODE-CHARTA/tree/main/src
*
*THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*SOFTWARE.

REPORT zi_file_to_codecharta.

CLASS lcl_file_handler DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS check_input_file
      IMPORTING
        i_file TYPE localfile.
    METHODS check_output_file_directory
      IMPORTING
        i_file TYPE localfile.
    METHODS read_input_file
      IMPORTING
        i_file          TYPE localfile
      RETURNING
        VALUE(r_result) TYPE string_table.
    METHODS constructor
      IMPORTING
        i_on_appserver TYPE abap_bool.
    CLASS-METHODS f4_help_for_appserver_file
      RETURNING
        VALUE(r_result) TYPE dxfields-longpath.
    CLASS-METHODS f4_help_for_pres_file
      RETURNING
        VALUE(r_result) TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA on_app_server TYPE abap_bool.
ENDCLASS.

CLASS lcl_file_handler IMPLEMENTATION.
  METHOD check_input_file.
    IF on_app_server EQ abap_true.
      DATA error_message TYPE string.
      OPEN DATASET i_file FOR INPUT IN TEXT MODE ENCODING DEFAULT MESSAGE error_message.
      IF sy-subrc IS NOT INITIAL.
        MESSAGE error_message TYPE 'E'.
      ELSE.
        CLOSE DATASET i_file.
      ENDIF.
    ELSE.
      IF NOT cl_gui_frontend_services=>file_exist( CONV #( i_file ) ).
        MESSAGE 'Input file does not exist on the front end' TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD read_input_file.
    IF on_app_server EQ abap_true.
      OPEN DATASET i_file FOR INPUT IN TEXT MODE ENCODING DEFAULT.
      DO.
        DATA(record) = CONV string( '' ).
        READ DATASET i_file INTO record.
        IF sy-subrc IS NOT INITIAL.
          EXIT.
        ENDIF.
        INSERT record INTO TABLE r_result.
      ENDDO.
    ELSE.
      cl_gui_frontend_services=>gui_upload(
        EXPORTING
          filename                = CONV #( i_file )
        CHANGING
          data_tab                =                  r_result
        EXCEPTIONS
          file_open_error         = 1                " File does not exist and cannot be opened
          file_read_error         = 2                " Error when reading file
          no_batch                = 3                " Cannot execute front-end function in background
          gui_refuse_filetransfer = 4                " Incorrect front end or error on front end
          invalid_type            = 5                " Incorrect parameter FILETYPE
          no_authority            = 6                " No upload authorization
          unknown_error           = 7                " Unknown error
          bad_data_format         = 8                " Cannot Interpret Data in File
          header_not_allowed      = 9                " Invalid header
          separator_not_allowed   = 10               " Invalid separator
          header_too_long         = 11               " Header information currently restricted to 1023 bytes
          unknown_dp_error        = 12               " Error when calling data provider
          access_denied           = 13               " Access to File Denied
          dp_out_of_memory        = 14               " Not enough memory in data provider
          disk_full               = 15               " Storage medium is full.
          dp_timeout              = 16               " Data provider timeout
          not_supported_by_gui    = 17               " GUI does not support this
          error_no_gui            = 18               " GUI not available
          OTHERS                  = 19       ).
      IF sy-subrc <> 0.
        MESSAGE ID 'E' TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD check_output_file_directory.
    DATA: directory TYPE localfile,
          filename  TYPE localfile.
    CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
      EXPORTING
        full_name     = i_file
      IMPORTING
        stripped_name = filename
        file_path     = directory
      EXCEPTIONS
        x_error       = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    IF filename IS INITIAL.
      MESSAGE 'No file name specified in output file'(003) TYPE 'E'.
    ENDIF.
    CALL FUNCTION 'PFL_CHECK_DIRECTORY'
      EXPORTING
        directory_long              = directory    " Name of directory
      EXCEPTIONS
        pfl_dir_not_exist           = 1
        pfl_permission_denied       = 2
        pfl_cant_build_dataset_name = 3
        pfl_file_not_exist          = 4
        pfl_authorization_missing   = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          MESSAGE 'Output directory does not exist on the appserver'(001) TYPE 'E'.
        WHEN 2 OR 5.
          MESSAGE 'No permission to the output directory on the appserver'(002) TYPE 'E'.
        WHEN OTHERS.
          MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDCASE.
    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    on_app_server = i_on_appserver.
  ENDMETHOD.

  METHOD f4_help_for_pres_file.
    DATA file_tab TYPE filetable.
    DATA rc TYPE i ##NEEDED.
    cl_gui_frontend_services=>file_open_dialog(   CHANGING file_table = file_tab
                                                           rc         = rc
                                                EXCEPTIONS file_open_dialog_failed = 0
                                                           cntl_error              = 0
                                                           error_no_gui            = 0
                                                           not_supported_by_gui    = 0
                                                           OTHERS                  = 0 ).
    READ TABLE file_tab INTO r_result INDEX 1.
  ENDMETHOD.

  METHOD f4_help_for_appserver_file.
    CALL FUNCTION 'F4_DXFILENAME_TOPRECURSION'
      EXPORTING
        i_location_flag = 'A'              " Flag: Application or presentation server
        i_path          = ''
        filemask        = '*.*'
        fileoperation   = 'R'
      IMPORTING
        o_path          = r_result
      EXCEPTIONS
        rfc_error       = 0
        error_with_gui  = 0
        OTHERS          = 0.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_code_metrics_handler DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS store_code_metrics_from_file
      IMPORTING
        i_file_handler TYPE REF TO lcl_file_handler
        i_file         TYPE localfile.
    METHODS constructor.
    METHODS submit_analyser_in_background
      IMPORTING
        i_aggregate_by_package TYPE abap_bool
        i_with_dependencies    TYPE abap_bool
        i_with_cycles          TYPE abap_bool
        i_file                 TYPE localfile.
  PRIVATE SECTION.
    CONSTANTS c_codecharta_job_name TYPE btcjob VALUE 'CODECHARTA' ##NO_TEXT.
    DATA number_of_input_fields TYPE i.
    METHODS convert_to_code_metric
      IMPORTING
        i_input_record  TYPE string
        i_tabix         TYPE sytabix
      RETURNING
        VALUE(r_result) TYPE zsi_a2cc_code_metrics.
    METHODS store_metrics_on_db
      IMPORTING
        i_metrics TYPE ztti_a2cc_code_metrics.
ENDCLASS.

CLASS lcl_code_metrics_handler IMPLEMENTATION.
  METHOD store_code_metrics_from_file.
    DATA(input_file) = i_file_handler->read_input_file( i_file ).
    DATA metrics TYPE ztti_a2cc_code_metrics.
    LOOP AT input_file ASSIGNING FIELD-SYMBOL(<input_record>).
      INSERT convert_to_code_metric( i_input_record = <input_record> i_tabix = sy-tabix ) INTO TABLE metrics.
    ENDLOOP.
    store_metrics_on_db( metrics ).
  ENDMETHOD.

  METHOD convert_to_code_metric.
    SPLIT i_input_record AT cl_abap_char_utilities=>horizontal_tab INTO TABLE DATA(fields).
    IF lines( fields ) NE number_of_input_fields.
      DATA(message) = CONV string( 'Record &1 of input file contains &2 fields. &3 fields were expected'(006) ).
      REPLACE ALL OCCURRENCES OF '&1' IN message WITH CONV string( i_tabix ).
      REPLACE ALL OCCURRENCES OF '&2' IN message WITH CONV string( lines( fields ) ).
      REPLACE ALL OCCURRENCES OF '&3' IN message WITH CONV string( number_of_input_fields ).
      MESSAGE message TYPE 'E'.
    ENDIF.
    r_result = VALUE #( package = fields[ 1 ]
                                  category = fields[ 2 ]
                                  modu_unit_1 = fields[ 3 ]
                                  sub_type = fields[ 4 ]
                                  modu_unit_2 = fields[ 5 ]
                                  author = fields[ 6 ]
                                  changer = fields[ 7 ]
                                  sobj_name = fields[ 8 ]
                                  loc = fields[ 9 ]
                                  nos = fields[ 10 ]
                                  noc = fields[ 11 ]
                                  nop = fields[ 12 ]
                                  mcc_com = fields[ 13 ]
                                  vrsd_chan = fields[ 14 ]
                                  dd_total = fields[ 15  ]
                                  stmts_c = fields[ 16 ]
                                  stmts_p = fields[ 17 ]
                                  stmts_i = fields[ 18 ]
                                  stmts_e = fields[ 19 ]
                                  stmts_ei = fields[ 20 ]
                                  stmts_d = fields[ 21 ]
                                  stmts_w = fields[ 22 ]
                                  stmts_l = fields[ 23 ]
                                  stmts_ca = fields[ 24  ]
                                  stmts_wh = fields[ 25 ]
                                  stmts_sl = fields[ 26 ]
                                  stmts_ins = fields[ 27  ]
                                  stmts_upd = fields[ 28 ]
                                  stmts_del = fields[ 29 ]
                                  stmts_mod = fields[ 30 ]
                                  diff_vers = fields[ 31 ]
                                  diff_mod_l = fields[ 32 ]
                                  diff_new_l = fields[ 33 ]
                                  diff_sap = fields[ 34 ]
                                  mod_db = fields[ 35 ]
                                  mod_call = fields[ 36 ]
                                  mod_bra = fields[ 37 ]
                                  number_of_methods = fields[ 38 ]
                                  average_nos_per_method = fields[ 39 ]
                                  decission_depth_complexity = fields[ 40 ]
                                  db_access_statements = fields[ 41 ]
                                  cyclomatic_complexity = fields[ 42 ]
                                  cyclomatic_complexity_avg = fields[ 43 ]
                                  component_to_be_changed = fields[ 44 ] ).
  ENDMETHOD.

  METHOD constructor.
    DATA struct_descr TYPE REF TO cl_abap_structdescr.
    struct_descr ?= cl_abap_typedescr=>describe_by_name( 'ZSI_A2CC_CODE_METRICS' ).
    number_of_input_fields = lines( struct_descr->get_components( ) ).
  ENDMETHOD.

  METHOD submit_analyser_in_background.
    DATA job_number TYPE btcjobcnt.
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = c_codecharta_job_name
      IMPORTING
        jobcount         = job_number
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      MESSAGE 'Job Aborted after JOB_OPEN Function Module.' TYPE 'E'.
    ENDIF.
    SUBMIT zdb_to_codecharta
        VIA JOB c_codecharta_job_name
         NUMBER  job_number
            WITH p_file = i_file
            WITH p_aggpak = i_aggregate_by_package
            WITH p_withcy = i_with_cycles
            WITH p_withdp = i_with_dependencies
          AND RETURN.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE 'Error submitting ZDB_TO_CODECHARTA' TYPE 'E'.
    ENDIF.
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = job_number
        jobname              = c_codecharta_job_name
        strtimmed            = abap_true
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        OTHERS               = 8.
    IF sy-subrc <> 0.
      MESSAGE 'Job Aborted after JOB_CLOSE Function Module.' TYPE 'E'.
    ENDIF.
  ENDMETHOD.


  METHOD store_metrics_on_db.
    CALL FUNCTION 'ENQUEUE_EZCODE_METRICS'
      EXCEPTIONS
        foreign_lock   = 1                " Object already locked
        system_failure = 2                " Internal error from enqueue server
        OTHERS         = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    DATA metrics TYPE STANDARD TABLE OF zcode_metrics.
    LOOP AT i_metrics ASSIGNING FIELD-SYMBOL(<metric>).
      INSERT INITIAL LINE INTO TABLE metrics ASSIGNING FIELD-SYMBOL(<db_metric>).
      <db_metric> = CORRESPONDING zcode_metrics( <metric> MAPPING pack = package ).
      <db_metric>-line_count = sy-tabix.
    ENDLOOP.
    DELETE FROM zcode_metrics WHERE pack NE space.
    INSERT zcode_metrics FROM TABLE metrics.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE 'Error on insert' TYPE 'E'.
      ROLLBACK WORK.
    ENDIF.
    CALL FUNCTION 'DEQUEUE_EZCODE_METRICS'.
  ENDMETHOD.

ENDCLASS.

PARAMETERS b_pres RADIOBUTTON GROUP src DEFAULT 'X' USER-COMMAND rb.
PARAMETERS b_apps RADIOBUTTON GROUP src.
PARAMETERS ifile TYPE localfile.
PARAMETERS file TYPE localfile.

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-bld. " Aggregation Level
PARAMETERS agg_modl TYPE abap_bool RADIOBUTTON GROUP agg DEFAULT 'X'.
PARAMETERS agg_devc TYPE abap_bool RADIOBUTTON GROUP agg.
SELECTION-SCREEN END OF BLOCK a.

SELECTION-SCREEN BEGIN OF BLOCK d WITH FRAME TITLE TEXT-bla. " Dependencies
PARAMETERS dpndncy TYPE abap_bool RADIOBUTTON GROUP dpn DEFAULT 'X'.
PARAMETERS wo_dpnd TYPE abap_bool RADIOBUTTON GROUP dpn.
PARAMETERS cycls TYPE abap_bool RADIOBUTTON GROUP dpn.
SELECTION-SCREEN END OF BLOCK d.

DATA file_handler TYPE REF TO lcl_file_handler.

AT SELECTION-SCREEN.
  file_handler = NEW #( b_apps ).
  file_handler->check_input_file( i_file = ifile ).
  file_handler->check_output_file_directory( file ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR ifile.
  IF b_pres EQ abap_true.
    ifile = lcl_file_handler=>f4_help_for_pres_file( ).
  ELSE.
    ifile = lcl_file_handler=>f4_help_for_appserver_file( ).
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR file.
  file = lcl_file_handler=>f4_help_for_appserver_file( ).

START-OF-SELECTION.
  DATA(metrics_handler) = NEW lcl_code_metrics_handler( ).
  metrics_handler->store_code_metrics_from_file( i_file = ifile i_file_handler = file_handler ).
  metrics_handler->submit_analyser_in_background( i_aggregate_by_package = agg_devc
                                                  i_with_dependencies    = dpndncy
                                                  i_with_cycles          = cycls
                                                  i_file                 = file ).
  MESSAGE 'Job submitted' TYPE 'S'.
