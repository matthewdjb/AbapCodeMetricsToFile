*
* This is version 0.5.1 for 7.31 upwards
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
*THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*SOFTWARE.
REPORT zi_codemetrics_to_file.

*----------------------------------------------------------------------*
*       CLASS lcl_code_metrics DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_code_metrics DEFINITION.
  PUBLIC SECTION.
    METHODS run
      IMPORTING
        selection_variant TYPE variant
      RETURNING
        VALUE(result)     TYPE ztti_a2cc_code_metrics
      RAISING
        cx_salv_bs_sc_runtime_info.
    METHODS convert_to_string
      IMPORTING
        i_code_metrics  TYPE ztti_a2cc_code_metrics
      RETURNING
        VALUE(r_result) TYPE string.
    METHODS check_variant
      IMPORTING
        i_variant TYPE variant.
  PRIVATE SECTION.
    METHODS set_alv_runtime_info.
    METHODS submit_code_metrics_report
      IMPORTING
        selection_variant TYPE variant.
    METHODS get_alv_list_from_report
      RETURNING
        VALUE(result) TYPE ztti_a2cc_code_metrics
      RAISING
        cx_salv_bs_sc_runtime_info.
    METHODS convert_code_metric_to_string
      IMPORTING
        i_code_metric   TYPE zsi_a2cc_code_metrics
      RETURNING
        VALUE(r_result) TYPE string.

ENDCLASS.                    "lcl_code_metrics DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_code_metrics IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_code_metrics IMPLEMENTATION.
  METHOD run.
    set_alv_runtime_info( ).
    submit_code_metrics_report( selection_variant ).
    result = get_alv_list_from_report( ).
  ENDMETHOD.                    "run

  METHOD get_alv_list_from_report.
    FIELD-SYMBOLS <alv_list>   TYPE STANDARD TABLE.
    DATA alv_list              TYPE REF TO data.
    cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = alv_list ).
    ASSIGN alv_list->* TO <alv_list>.
    FIELD-SYMBOLS <alv_line> TYPE any.
    LOOP AT <alv_list> ASSIGNING <alv_line>.
      DATA result_line LIKE LINE OF result.
      MOVE-CORRESPONDING <alv_line> TO result_line.
      INSERT result_line INTO TABLE result.
    ENDLOOP.
  ENDMETHOD.                    "get_alv_list_from_report

  METHOD submit_code_metrics_report.
    SUBMIT /sdf/cd_custom_code_metric
    USING SELECTION-SET selection_variant
    EXPORTING LIST TO MEMORY
    AND RETURN.
  ENDMETHOD.                    "submit_code_metrics_report

  METHOD set_alv_runtime_info.
    cl_salv_bs_runtime_info=>set(
      EXPORTING display  = abap_false
                metadata = abap_false
                data     = abap_true ).
  ENDMETHOD.                    "set_alv_runtime_info

  METHOD convert_to_string.
    DATA code_metric TYPE zsi_a2cc_code_metrics.
    LOOP AT i_code_metrics INTO code_metric.
      DATA tab_delimited_record TYPE string.
      tab_delimited_record = convert_code_metric_to_string( code_metric ).
      AT FIRST.
        r_result = tab_delimited_record.
        CONTINUE.
      ENDAT.
      r_result = r_result && cl_abap_char_utilities=>cr_lf && tab_delimited_record.
    ENDLOOP.
  ENDMETHOD.                    "convert_to_string


  METHOD convert_code_metric_to_string.
    r_result = |{ i_code_metric-package }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-category }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-modu_unit_1 }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-sub_type }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-modu_unit_2 }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-author }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-changer }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-sobj_name }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-loc }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-nos }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-noc }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-nop }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-mcc_com }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-vrsd_chan }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-dd_total }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-stmts_c }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-stmts_p }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-stmts_i }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-stmts_e }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-stmts_ei }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-stmts_d }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-stmts_w }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-stmts_l }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-stmts_ca }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-stmts_wh }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-stmts_sl }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-stmts_ins }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-stmts_upd }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-stmts_del }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-stmts_mod }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-diff_vers }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-diff_mod_l }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-diff_new_l }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-diff_sap }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-mod_db }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-mod_call }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-mod_bra }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-number_of_methods }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-average_nos_per_method }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-decission_depth_complexity }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-db_access_statements }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-cyclomatic_complexity }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-cyclomatic_complexity_avg }{ cl_abap_char_utilities=>horizontal_tab }| &&
               |{ i_code_metric-component_to_be_changed }{ cl_abap_char_utilities=>horizontal_tab }|.
  ENDMETHOD.                    "convert_code_metric_to_string

  METHOD check_variant.
    DATA exists TYPE variant ##needed.
    SELECT SINGLE variant INTO exists FROM varid
        WHERE report EQ '/SDF/CD_CUSTOM_CODE_METRIC'
          AND variant EQ i_variant.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE 'Variant not defined for Code Metrics in tx /SDF/CD_CCA'(001) TYPE 'E'.
    ENDIF.
  ENDMETHOD.                    "check_variant
ENDCLASS.                    "lcl_code_metrics IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_file_output DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_file_output DEFINITION.
  PUBLIC SECTION.
    METHODS write_file
      IMPORTING
        VALUE(file_name) TYPE localfile
        content          TYPE string.
    METHODS check_file_directory
      IMPORTING
        i_file TYPE localfile.
  PRIVATE SECTION.
    METHODS convert_string_to_xstring
      IMPORTING
        string        TYPE string
      RETURNING
        VALUE(result) TYPE xstring.
    METHODS write_xstring_to_file
      IMPORTING
        file_name TYPE localfile
        xstring   TYPE xstring.
    METHODS adjust_filename
      IMPORTING
        file_name     TYPE localfile
      RETURNING
        VALUE(result) TYPE localfile.
ENDCLASS.                    "lcl_file_output DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_file_output IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_file_output IMPLEMENTATION.
  METHOD write_file.
    write_xstring_to_file(
        file_name = adjust_filename( file_name )
        xstring   = convert_string_to_xstring( content ) ).
  ENDMETHOD.                    "write_file

  METHOD write_xstring_to_file.
    OPEN DATASET file_name FOR OUTPUT IN BINARY MODE.
    IF sy-subrc NE 0.
      WRITE : / 'Error Opening the Server Filepath :', file_name.
    ELSE.
      TRANSFER xstring TO file_name.
      CLOSE DATASET file_name.
      WRITE : / 'File Downloaded to the Server at :', file_name.
    ENDIF.
  ENDMETHOD.                    "write_xstring_to_file

  METHOD convert_string_to_xstring.
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = string
      IMPORTING
        buffer = result.
  ENDMETHOD.                    "convert_string_to_xstring

  METHOD adjust_filename.
    result = file_name.
    REPLACE ALL OCCURRENCES OF '<DATE>' IN result WITH sy-datum.
  ENDMETHOD.                    "adjust_filename

  METHOD check_file_directory.
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
      MESSAGE 'No file name specified'(001) TYPE 'E'.
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
          MESSAGE 'Directory does not exist on the appserver'(002) TYPE 'E'.
        WHEN 2 OR 5.
          MESSAGE 'No permission to the directory on the appserver'(003) TYPE 'E'.
        WHEN OTHERS.
          MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDCASE.
    ENDIF.
  ENDMETHOD.                    "check_file_directory
ENDCLASS.                    "lcl_file_output IMPLEMENTATION

PARAMETERS varnt TYPE variant.
PARAMETERS file TYPE localfile.

INITIALIZATION.
  DATA file_handler TYPE REF TO lcl_file_output.
  CREATE OBJECT file_handler.
  DATA code_metrics_handler TYPE REF TO lcl_code_metrics.
  CREATE OBJECT code_metrics_handler.

AT SELECTION-SCREEN.
  file_handler->check_file_directory( file ).
  code_metrics_handler->check_variant( varnt ).

START-OF-SELECTION.
  DATA code_metrics_output TYPE string.
  code_metrics_output = code_metrics_handler->convert_to_string( code_metrics_handler->run( varnt ) ).
  file_handler->write_file( file_name = file
                            content   = code_metrics_output ).
