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
REPORT zdb_to_codecharta.

CLASS lcl_file_handler DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS write_file
      IMPORTING
        file_name TYPE localfile
        json      TYPE string.
  PRIVATE SECTION.
    METHODS write_xstring_to_file
      IMPORTING
        file_name TYPE localfile
        xstring   TYPE xstring.
    METHODS adjust_filename
      IMPORTING
        file_name     TYPE localfile
      RETURNING
        VALUE(result) TYPE localfile.
    METHODS convert_string_to_xstring
      IMPORTING
        string        TYPE string
      RETURNING
        VALUE(result) TYPE xstring.
ENDCLASS.

CLASS lcl_file_handler IMPLEMENTATION.

  METHOD write_file.
    write_xstring_to_file(
        file_name = adjust_filename( file_name )
        xstring   = convert_string_to_xstring( json ) ).
  ENDMETHOD.

  METHOD write_xstring_to_file.
    OPEN DATASET file_name FOR OUTPUT IN BINARY MODE.
    IF sy-subrc NE 0.
      WRITE : / 'Error Opening the Server Filepath :'(004), file_name.
    ELSE.
      TRANSFER xstring TO file_name.
      CLOSE DATASET file_name.
      WRITE : / 'File Downloaded to the Server at :'(005), file_name.
    ENDIF.
  ENDMETHOD.

  METHOD adjust_filename.
    result = file_name.
    REPLACE ALL OCCURRENCES OF '<DATE>' IN result WITH sy-datum.
  ENDMETHOD.

  METHOD convert_string_to_xstring.
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = string
      IMPORTING
        buffer = result.
  ENDMETHOD.
ENDCLASS.

PARAMETERS: p_aggpak TYPE abap_bool,
            p_withdp TYPE abap_bool,
            p_withcy TYPE abap_bool,
            p_file   TYPE localfile.

START-OF-SELECTION.
  SELECT * FROM zcode_metrics INTO TABLE @DATA(code_metrics).
  DATA(abap2codecharta) = zcl_i_a2cc_factory=>create( COND #( WHEN p_aggpak = abap_true
                                                            THEN zcl_i_a2cc_factory=>aggregation_levels-package
                                                            ELSE zcl_i_a2cc_factory=>aggregation_levels-class ) ).
  DATA(json) = abap2codecharta->to_json( metrics               = CORRESPONDING #( code_metrics MAPPING package = pack )
                                         analyze_dependecies   = p_withdp
                                         analyze_direct_cycles = p_withcy ).
  NEW lcl_file_handler( )->write_file( file_name = p_file
                                       json      = json ).
