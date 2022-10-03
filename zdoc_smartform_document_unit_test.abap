CLASS unit_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  "Generate data with program SAPBC_DATA_GENERATOR

  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_flight_data,
        customer    TYPE  scustom,
        bookings    TYPE  ty_bookings,
        connections TYPE  ty_connections,
      END OF t_flight_data.

    METHODS display_document  FOR TESTING.
    METHODS get_pdf_xstring   FOR TESTING.
    METHODS print             FOR TESTING.

    METHODS _get_data
      RETURNING VALUE(flight_data) TYPE t_flight_data.
ENDCLASS.       "unit_Test


CLASS unit_test IMPLEMENTATION.

  METHOD display_document.

    TRY.

        DATA(flight_data) = me->_get_data( ).

        DATA(smartform_document) =
          zdoc_smartform_document=>create_document(
            form_name        = 'SF_EXAMPLE_01'
            content_data_obj = REF #( flight_data ) ).

        smartform_document->display( ).

      CATCH zcx_return3 INTO DATA(return3_exc).

        cl_abap_unit_assert=>fail(
          msg = return3_exc->get_text( ) ).

    ENDTRY.

  ENDMETHOD.

  METHOD get_pdf_xstring.

    TRY.

        DATA(flight_data) = me->_get_data( ).

        DATA(smartform_document) =
          zdoc_smartform_document=>create_document(
            form_name        = 'SF_EXAMPLE_01'
            content_data_obj = REF #( flight_data ) ).

        BREAK-POINT.
        DATA(pdf_xstring) = smartform_document->get_pdf_xstring( ).

        IF pdf_xstring IS INITIAL.

          cl_abap_unit_assert=>fail(
            msg = 'No PDF xstring generated' ).

        ENDIF.

      CATCH zcx_return3 INTO DATA(return3_exc).

        cl_abap_unit_assert=>fail(
          msg = return3_exc->get_text( ) ).

    ENDTRY.

  ENDMETHOD.

  METHOD print.

    TRY.

        DATA(flight_data) = me->_get_data( ).

        DATA(smartform_document) =
          zdoc_smartform_document=>create_document(
            form_name        = 'SF_EXAMPLE_01'
            content_data_obj = REF #( flight_data ) ).

        smartform_document->print( ).

      CATCH zcx_return3 INTO DATA(return3_exc).

        cl_abap_unit_assert=>fail(
          msg = return3_exc->get_text( ) ).

    ENDTRY.

  ENDMETHOD.

  METHOD _get_data.

    "Generate data with program SAPBC_DATA_GENERATOR

    DATA(customer_id) = CONV scustom-id( '00000001' ).

    SELECT SINGLE *
      FROM scustom
      INTO flight_data-customer
      WHERE id = customer_id.

    CHECK sy-subrc = 0.

    SELECT *
      FROM sbook
      INTO TABLE flight_data-bookings
      WHERE
        customid = customer_id
      ORDER BY PRIMARY KEY.

    IF flight_data-bookings[] IS NOT INITIAL.

      SELECT *
        FROM spfli
        INTO TABLE flight_data-connections
        FOR ALL ENTRIES IN flight_data-bookings
        WHERE
          carrid = flight_data-bookings-carrid AND
          connid = flight_data-bookings-connid
        ORDER BY PRIMARY KEY.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
