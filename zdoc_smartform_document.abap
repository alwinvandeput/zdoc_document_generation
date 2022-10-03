CLASS zdoc_smartform_document DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zdoc_abstract_document
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS create_document
      IMPORTING form_name        TYPE tdsfname
                content_data_obj TYPE REF TO data
      RETURNING VALUE(document)  TYPE REF TO zdoc_smartform_document .

    METHODS display REDEFINITION.

    METHODS print REDEFINITION.

    METHODS get_pdf_xstring REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA:
      form_name        TYPE tdsfname,
      content_data_obj TYPE REF TO data.

    METHODS _generate_otf
      IMPORTING control_parameters TYPE ssfctrlop OPTIONAL
                output_options     TYPE ssfcompop OPTIONAL
      RETURNING VALUE(otf_tab)     TYPE tt_itcoo
      RAISING   zcx_return3.

    METHODS _get_function_module_name
      RETURNING VALUE(function_module_name) TYPE rs38l_fnam
      RAISING   zcx_return3.

    METHODS _get_pdf_output_options
      RETURNING VALUE(output_options) TYPE ssfcompop
      RAISING   zcx_return3.

    METHODS _convert_otf_tab_to_xstring
      IMPORTING otf_tab            TYPE tt_itcoo
      RETURNING VALUE(pdf_xstring) TYPE xstring
      RAISING   zcx_return3.

    METHODS _get_parameter_tab
      RETURNING VALUE(parameter_tab) TYPE abap_func_parmbind_tab
      RAISING   zcx_return3.

    METHODS _get_exception_tab
      RETURNING VALUE(parameter_tab) TYPE abap_func_excpbind_tab.

ENDCLASS.



CLASS ZDOC_SMARTFORM_DOCUMENT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZDOC_SMARTFORM_DOCUMENT=>CREATE_DOCUMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] FORM_NAME                      TYPE        TDSFNAME
* | [--->] CONTENT_DATA_OBJ               TYPE REF TO DATA
* | [<-()] DOCUMENT                       TYPE REF TO ZDOC_SMARTFORM_DOCUMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_document.

    document = NEW zdoc_smartform_document( ).

    document->form_name = form_name.
    document->content_data_obj = content_data_obj.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZDOC_SMARTFORM_DOCUMENT->DISPLAY
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_RETURN3
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD display.

    DATA(control_parameters) = VALUE ssfctrlop(
      no_dialog = abap_false ).

    _generate_otf(
      control_parameters = control_parameters ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZDOC_SMARTFORM_DOCUMENT->PRINT
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_RETURN3
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD print.

    DATA(function_module) = _get_function_module_name( ).

    DATA(control_parameters) = VALUE ssfctrlop(
      no_dialog = abap_true ).

    _generate_otf(
      control_parameters = control_parameters ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZDOC_SMARTFORM_DOCUMENT->GET_PDF_XSTRING
* +-------------------------------------------------------------------------------------------------+
* | [<-()] PDF_XSTRING                    TYPE        XSTRING
* | [!CX!] ZCX_RETURN3
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_pdf_xstring.

    DATA(function_module) = _get_function_module_name( ).

    DATA(control_parameters) = VALUE ssfctrlop(
      no_dialog = abap_true
      getotf    = abap_true ).

    DATA(output_options) = _get_pdf_output_options( ).

    DATA(oft_tab) =
      _generate_otf(
        control_parameters = control_parameters ).

    pdf_xstring = _convert_otf_tab_to_xstring( oft_tab ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZDOC_SMARTFORM_DOCUMENT->_GET_PDF_OUTPUT_OPTIONS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] OUTPUT_OPTIONS                 TYPE        SSFCOMPOP
* | [!CX!] ZCX_RETURN3
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _get_pdf_output_options.

    DATA device_type TYPE rspoptype.

    CALL FUNCTION 'SSF_GET_DEVICE_TYPE'
      EXPORTING
        i_language             = sy-langu
      IMPORTING
        e_devtype              = device_type
      EXCEPTIONS
        no_language            = 1
        language_not_installed = 2
        no_devtype_found       = 3
        system_error           = 4
        OTHERS                 = 5.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_return3
        MESSAGE
          ID sy-msgid
          TYPE sy-msgty
          NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

    output_options-tdprinter = device_type.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZDOC_SMARTFORM_DOCUMENT->_GENERATE_OTF
* +-------------------------------------------------------------------------------------------------+
* | [--->] CONTROL_PARAMETERS             TYPE        SSFCTRLOP(optional)
* | [--->] OUTPUT_OPTIONS                 TYPE        SSFCOMPOP(optional)
* | [<-()] OTF_TAB                        TYPE        TT_ITCOO
* | [!CX!] ZCX_RETURN3
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _generate_otf.

    DATA(function_module_name) = _get_function_module_name( ).

    "----------------------------------------------------------
    "Set parameters
    "----------------------------------------------------------
    DATA parameter_tab TYPE abap_func_parmbind_tab.

    parameter_tab =
      VALUE abap_func_parmbind_tab(
        (  name  = 'CONTROL_PARAMETERS'
           kind  = abap_func_exporting
           value = REF #( control_parameters ) )
        (  name  = 'OUTPUT_OPTIONS'
           kind  = abap_func_exporting
           value = REF #( output_options ) )
      ).

    DATA(abap_data_descr) = cl_abap_structdescr=>describe_by_data_ref( me->content_data_obj ).

    DATA(abap_struct_descr) = CAST cl_abap_structdescr( abap_data_descr ).

    DATA(components) = abap_struct_descr->get_components( ).

    LOOP AT components
      ASSIGNING FIELD-SYMBOL(<component>).

      ASSIGN COMPONENT <component>-name
        OF STRUCTURE me->content_data_obj->*
        TO FIELD-SYMBOL(<source_parameter>).

      DATA target_parameter LIKE LINE OF parameter_tab.

      target_parameter =
        VALUE #(
          name   = <component>-name
          kind  = abap_func_exporting
          value  = REF #( <source_parameter> ) ).

      INSERT target_parameter
        INTO TABLE parameter_tab.

    ENDLOOP.

    DATA job_output_info  TYPE ssfcrescl.

    INSERT
      VALUE #(
        name   = 'JOB_OUTPUT_INFO'
        kind  = abap_func_importing
        value  = REF #( job_output_info ) )
      INTO TABLE parameter_tab.

    "----------------------------------------------------------
    "Set exceptions
    "----------------------------------------------------------
    DATA(exception_tab) = _get_exception_tab( ).

    "----------------------------------------------------------
    "Call function
    "----------------------------------------------------------
    CALL FUNCTION function_module_name
      PARAMETER-TABLE parameter_tab
      EXCEPTION-TABLE exception_tab.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_return3
        MESSAGE
          ID sy-msgid
          TYPE sy-msgty
          NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

    otf_tab = job_output_info-otfdata.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZDOC_SMARTFORM_DOCUMENT->_GET_FUNCTION_MODULE_NAME
* +-------------------------------------------------------------------------------------------------+
* | [<-()] FUNCTION_MODULE_NAME           TYPE        RS38L_FNAM
* | [!CX!] ZCX_RETURN3
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _get_function_module_name.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = me->form_name
      IMPORTING
        fm_name            = function_module_name
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_return3
        USING MESSAGE.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZDOC_SMARTFORM_DOCUMENT->_GET_PARAMETER_TAB
* +-------------------------------------------------------------------------------------------------+
* | [<-()] PARAMETER_TAB                  TYPE        ABAP_FUNC_PARMBIND_TAB
* | [!CX!] ZCX_RETURN3
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _get_parameter_tab.




*ptab = VALUE #( ( name  = 'FILENAME'
*                  kind  = abap_func_exporting
*                  value = REF #( filename ) )
*                ( name  = 'FILETYPE'
*                  kind  = abap_func_exporting
*                  value = REF #( filetype ) )
*                ( name  = 'DATA_TAB'
*                  kind  = abap_func_tables
*                  value = REF #( text_tab ) )
*                ( name  = 'FILELENGTH'
*                  kind  = abap_func_importing
*                  value = REF #( fleng ) ) ).

*      IMPORTING
*        job_output_info  = job_output

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZDOC_SMARTFORM_DOCUMENT->_GET_EXCEPTION_TAB
* +-------------------------------------------------------------------------------------------------+
* | [<-()] PARAMETER_TAB                  TYPE        ABAP_FUNC_EXCPBIND_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _get_exception_tab.

    parameter_tab =
      VALUE #(
        ( name = 'FORMATTING_ERROR' value = 1 )
        ( name = 'INTERNAL_ERROR'   value = 2 )
        ( name = 'SEND_ERROR'       value = 3 )
        ( name = 'USER_CANCELED'    value = 4 )
        ( name = 'OTHERS'           value = 5 )
      ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZDOC_SMARTFORM_DOCUMENT->_CONVERT_OTF_TAB_TO_XSTRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] OTF_TAB                        TYPE        TT_ITCOO
* | [<-()] PDF_XSTRING                    TYPE        XSTRING
* | [!CX!] ZCX_RETURN3
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _convert_otf_tab_to_xstring.

    DATA pdf_bin_file_size  TYPE i.
    DATA pdf_lines          TYPE TABLE OF tline.

    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
      IMPORTING
        bin_filesize          = pdf_bin_file_size
        bin_file              = pdf_xstring
      TABLES
        otf                   = otf_tab
        "Lines is a mandatory parameter
        lines                 = pdf_lines
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        err_bad_otf           = 4
        OTHERS                = 5.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_return3
        MESSAGE
          ID sy-msgid
          TYPE sy-msgty
          NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
