CLASS zbp_i_pricat_006 DEFINITION PUBLIC ABSTRACT FINAL FOR BEHAVIOR OF zi_pricat_006.

*   Get Description for Article, Color, Pricat, Series (via Custom Business Object ODATA API)
    CLASS-METHODS get_custom_fields_internal
        IMPORTING VALUE(i_article_code)         TYPE string OPTIONAL
                  VALUE(i_color_code)           TYPE string OPTIONAL
                  VALUE(i_pricat_code)          TYPE string OPTIONAL
                  VALUE(i_series_code)          TYPE string OPTIONAL
        EXPORTING VALUE(o_article_description)  TYPE string
                  VALUE(o_color_description)    TYPE string
                  VALUE(o_pricat_description)   TYPE string
                  VALUE(o_series_description)   TYPE string
                  VALUE(o_article_code)         TYPE string
                  VALUE(o_color_code)           TYPE string
                  VALUE(o_pricat_code)          TYPE string
                  VALUE(o_series_code)          TYPE string.

    TYPES: BEGIN OF descr_table. " for cashing
        TYPES code(4)          TYPE C.
        TYPES description(40)  TYPE C.
    TYPES: END OF descr_table.
    CLASS-DATA it_article   TYPE TABLE OF descr_table. " WITH UNIQUE KEY code.
    CLASS-DATA it_color     TYPE TABLE OF descr_table. " WITH UNIQUE KEY code.
    CLASS-DATA it_pricat    TYPE TABLE OF descr_table. " WITH UNIQUE KEY code.
    CLASS-DATA it_series    TYPE TABLE OF descr_table. " WITH UNIQUE KEY code.

*   Get Description for Article, Color, Pricat, Series (via Custom Business Object ODATA API) - optimized via cash
    CLASS-METHODS get_custom_fields_opt_internal
        IMPORTING VALUE(i_article_code)         TYPE string OPTIONAL
                  VALUE(i_color_code)           TYPE string OPTIONAL
                  VALUE(i_pricat_code)          TYPE string OPTIONAL
                  VALUE(i_series_code)          TYPE string OPTIONAL
        EXPORTING VALUE(o_article_description)  TYPE string
                  VALUE(o_color_description)    TYPE string
                  VALUE(o_pricat_description)   TYPE string
                  VALUE(o_series_description)   TYPE string
                  VALUE(o_article_code)         TYPE string
                  VALUE(o_color_code)           TYPE string
                  VALUE(o_pricat_code)          TYPE string
                  VALUE(o_series_code)          TYPE string.

ENDCLASS. " zbp_i_pricat_006 DEFINITION

CLASS zbp_i_pricat_006 IMPLEMENTATION.

  METHOD get_custom_fields_internal. " Get Description for Article, Color, Pricat, Series (via Custom Business Object ODATA API)

    DATA system_url TYPE string.

    DATA i_username TYPE string VALUE 'INBOUND_USER'.
    DATA i_password TYPE string VALUE 'rtrVDDgelabtTjUiybRX}tVD3JksqqfvPpBdJRaL'.

    DATA token      TYPE string.
    DATA body       TYPE string.
    DATA text       TYPE string.
    DATA s1         TYPE string.
    DATA s2         TYPE string.
    DATA s3         TYPE string.

    TRY.

*  DATA(i_url) = 'https://my404898.s4hana.cloud.sap/sap/opu/odata/sap/YY1_CUSTOMFIELDS_CDS/YY1_CUSTOMFIELDS(guid'91bf6b38-1c0f-1ede-b2ca-79c4ed0310fc')'.
*  DATA(i_url) = 'https://my404898.s4hana.cloud.sap/sap/opu/odata/sap/YY1_CUSTOMFIELDS_CDS/YY1_CUSTOMFIELDS'.

        system_url = cl_abap_context_info=>get_system_url( ).


*       Read list of objects and get UUID of the first one:

        CONCATENATE
                'https://'
                system_url(8) " my404898
                '-api.s4hana.cloud.sap/sap/opu/odata/sap/YY1_CUSTOMFIELDS_CDS/YY1_CUSTOMFIELDS'
            INTO DATA(i_url).

        IF ( system_url(8) = 'my404907' ). " test
            i_username  = 'INBOUND_USER'.
            i_password  = 'rtrVDDgelabtTjUiybRX}tVD3JksqqfvPpBdJRaL'.
        ENDIF.
        IF ( system_url(8) = 'my410080' ). " prod
            i_username  = 'INBOUND_USER'.
            i_password  = 'YKXMYdjNnGgqko&aEueVx5mHTFPRGcDGAVgQgnFh'.
        ENDIF.

        DATA(http_destination) = cl_http_destination_provider=>create_by_url( i_url = i_url ).

        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( http_destination ).

        lo_http_client->get_http_request( )->set_authorization_basic(
            i_username = i_username
            i_password = i_password
        ).

        DATA(lo_http_request) = lo_http_client->get_http_request( ).

        DATA(lo_http_response) = lo_http_client->execute(
            i_method   = if_web_http_client=>get
        ).

        text                          = lo_http_response->get_text( ).
        DATA(status)                  = lo_http_response->get_status( ).
        DATA(response_header_fields)  = lo_http_response->get_header_fields( ).

        REPLACE '<d:SAP_UUID>'    IN text WITH '***SAP_UUID***'.
        REPLACE '</d:SAP_UUID>'   IN text WITH '***SAP_UUID***'.
        SPLIT text AT '***SAP_UUID***' INTO s1 s2 s3.

        DATA(sap_uuid) = s2.

        CONCATENATE
                'https://'
                system_url(8) " my404898
                '-api.s4hana.cloud.sap/sap/opu/odata/sap/YY1_CUSTOMFIELDS_CDS/YY1_CUSTOMFIELDS'
                '(guid''' sap_uuid ''')' " '(guid''91bf6b38-1c0f-1ede-b2ca-79c4ed0310fc'')'
            INTO i_url.

        http_destination = cl_http_destination_provider=>create_by_url( i_url = i_url ).

        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( http_destination ).

        lo_http_client->get_http_request( )->set_authorization_basic(
            i_username = i_username
            i_password = i_password
        ).

        lo_http_request = lo_http_client->get_http_request( ).


*       Get Token:

        lo_http_request->set_header_field(
            i_name  = 'x-csrf-token'
            i_value = 'fetch'
        ).

        lo_http_response = lo_http_client->execute(
            i_method   = if_web_http_client=>get
        ).

        text                   = lo_http_response->get_text( ).
        status                 = lo_http_response->get_status( ).
        response_header_fields = lo_http_response->get_header_fields( ).

        READ TABLE response_header_fields WITH KEY name = 'x-csrf-token' INTO DATA(field).
        IF ( sy-subrc = 0 ).
            token = field-value.
        ENDIF.


*       Update Codes:

        DATA i_fields TYPE if_web_http_request=>name_value_pairs.
        APPEND VALUE #(
            name  = 'x-csrf-token'
            value = token " '5iGZK1qT45Vi4UfHYazbPQ=='
        )
        TO i_fields.
        APPEND VALUE #(
            name  = 'Content-Type'
            value = 'application/json'
        )
        TO i_fields.
*        APPEND VALUE #(
*            name  = 'Content-Length'
*            value = '1000'
*        )
*        TO i_fields.

        lo_http_request->set_header_fields(
          EXPORTING
            i_fields = i_fields
*          RECEIVING
*            r_value  =
        ).

        CONCATENATE
                '{'
                '"ARTICLE_CODE":"' i_article_code '",'
                '"COLOR_CODE":"' i_color_code '",'
                '"PRICAT_CODE":"' i_pricat_code '",'
                '"SERIES_CODE":"' i_series_code '"'
                '}'
            INTO
                body.

        lo_http_request->set_text(
          EXPORTING
            i_text   = body
*            i_offset = 0
*            i_length = -1
*          RECEIVING
*            r_value  =
        ).

        lo_http_response = lo_http_client->execute(
            i_method   = if_web_http_client=>put
        ).

        text                      = lo_http_response->get_text( ).
        status                    = lo_http_response->get_status( ).
        response_header_fields    = lo_http_response->get_header_fields( ).


*       Read Descriptions

        lo_http_response = lo_http_client->execute(
            i_method   = if_web_http_client=>get
        ).

        text                    = lo_http_response->get_text( ).
        status                  = lo_http_response->get_status( ).
        response_header_fields  = lo_http_response->get_header_fields( ).

        REPLACE '<d:ARTICLE_DESCRIPTION>'   IN text WITH '***ARTICLE_DESCRIPTION***'.
        REPLACE '</d:ARTICLE_DESCRIPTION>'  IN text WITH '***ARTICLE_DESCRIPTION***'.
        SPLIT text AT '***ARTICLE_DESCRIPTION***' INTO s1 s2 s3.
        o_article_description = s2.
        o_article_code        = i_article_code.

        REPLACE '<d:COLOR_DESCRIPTION>'     IN text WITH '***COLOR_DESCRIPTION***'.
        REPLACE '</d:COLOR_DESCRIPTION>'    IN text WITH '***COLOR_DESCRIPTION***'.
        SPLIT text AT '***COLOR_DESCRIPTION***' INTO s1 s2 s3.
        o_color_description = s2.
        o_color_code        = i_color_code.

        REPLACE '<d:PRICAT_DESCRIPTION>'     IN text WITH '***PRICAT_DESCRIPTION***'.
        REPLACE '</d:PRICAT_DESCRIPTION>'    IN text WITH '***PRICAT_DESCRIPTION***'.
        SPLIT text AT '***PRICAT_DESCRIPTION***' INTO s1 s2 s3.
        o_pricat_description = s2.
        o_pricat_code        = i_pricat_code.

        REPLACE '<d:SERIES_DESCRIPTION>'     IN text WITH '***SERIES_DESCRIPTION***'.
        REPLACE '</d:SERIES_DESCRIPTION>'    IN text WITH '***SERIES_DESCRIPTION***'.
        SPLIT text AT '***SERIES_DESCRIPTION***' INTO s1 s2 s3.
        o_series_description = s2.
        o_series_code        = i_series_code.

    CATCH cx_web_message_error INTO DATA(lx_web_message_error).
      " Handle Exception
*      RAISE SHORTDUMP lx_web_message_error.

    CATCH cx_abap_context_info_error INTO DATA(lx_abap_context_info_error).
      " Handle Exception
*      RAISE SHORTDUMP lx_abap_context_info_error.

    CATCH /iwbep/cx_cp_remote INTO DATA(lx_remote).
      " Handle remote Exception
*      RAISE SHORTDUMP lx_remote.

    CATCH /iwbep/cx_gateway INTO DATA(lx_gateway).
      " Handle Exception
*      RAISE SHORTDUMP lx_gateway.

    CATCH cx_web_http_client_error INTO DATA(lx_web_http_client_error).
      " Handle Exception
*      RAISE SHORTDUMP lx_web_http_client_error.

    CATCH cx_http_dest_provider_error INTO DATA(lx_http_dest_provider_error).
        "handle exception
*      RAISE SHORTDUMP lx_http_dest_provider_error.

    ENDTRY.

  ENDMETHOD. " get_custom_fields_internal

  METHOD get_custom_fields_opt_internal. " Get Description for Article, Color, Pricat, Series (via Custom Business Object ODATA API) - optimized

*   DATA it_cash_create TYPE TABLE FOR CREATE zi_cash_006. " Cash

    IF ( i_article_code IS NOT INITIAL ).
        READ TABLE it_article INTO DATA(article) WITH KEY code = i_article_code BINARY SEARCH.
*        SELECT SINGLE * FROM zi_cash_006 WHERE ( type = 'A' ) AND ( code = @i_article_code ) INTO @DATA(article).
        IF ( sy-subrc = 0 ).
            o_article_description   = article-description.
            o_article_code          = article-code.
        ENDIF.
    ENDIF.
    IF ( i_color_code IS NOT INITIAL ).
        READ TABLE it_color INTO DATA(color) WITH KEY code = i_color_code BINARY SEARCH.
*        SELECT SINGLE * FROM zi_cash_006 WHERE ( type = 'C' ) AND ( code = @i_color_code ) INTO @DATA(color).
        IF ( sy-subrc = 0 ).
            o_color_description   = color-description.
            o_color_code          = color-code.
        ENDIF.
    ENDIF.
    IF ( i_pricat_code IS NOT INITIAL ).
        READ TABLE it_pricat INTO DATA(pricat) WITH KEY code = i_pricat_code BINARY SEARCH.
*        SELECT SINGLE * FROM zi_cash_006 WHERE ( type = 'P' ) AND ( code = @i_pricat_code ) INTO @DATA(pricat).
        IF ( sy-subrc = 0 ).
            o_pricat_description   = pricat-description.
            o_pricat_code          = pricat-code.
        ENDIF.
    ENDIF.
    IF ( i_series_code IS NOT INITIAL ).
        READ TABLE it_series  INTO DATA(series) WITH KEY code = i_series_code BINARY SEARCH.
*        SELECT SINGLE * FROM zi_cash_006 WHERE ( type = 'S' ) AND ( code = @i_series_code ) INTO @DATA(series).
        IF ( sy-subrc = 0 ).
            o_series_description   = series-description.
            o_series_code          = series-code.
        ENDIF.
    ENDIF.

*   If did not find descriptions in cash
    DATA(article_found) = abap_true.
    IF ( ( i_article_code IS NOT INITIAL ) AND ( o_article_code IS INITIAL ) ).
        article_found = abap_false.
    ENDIF.
    DATA(color_found) = abap_true.
    IF ( ( i_color_code IS NOT INITIAL ) AND ( o_color_code IS INITIAL ) ).
        color_found = abap_false.
    ENDIF.
    DATA(pricat_found) = abap_true.
    IF ( ( i_pricat_code IS NOT INITIAL ) AND ( o_pricat_code IS INITIAL ) ).
        pricat_found = abap_false.
    ENDIF.
    DATA(series_found) = abap_true.
    IF ( ( i_series_code IS NOT INITIAL ) AND ( o_series_code IS INITIAL ) ).
        series_found = abap_false.
    ENDIF.
*   If did not find descriptions in cash - call API
    IF ( ( article_found = abap_false ) OR ( color_found = abap_false ) OR ( pricat_found = abap_false ) OR ( series_found = abap_false ) ).

        zbp_i_pricat_006=>get_custom_fields_internal(
          EXPORTING
             i_article_code        = i_article_code " '123'
             i_color_code          = i_color_code   " '030'
             i_pricat_code         = i_pricat_code  " '21'
             i_series_code         = i_series_code  " '126'
          IMPORTING
             o_article_description = o_article_description
             o_color_description   = o_color_description
             o_pricat_description  = o_pricat_description
             o_series_description  = o_series_description
             o_article_code        = o_article_code
             o_color_code          = o_color_code
             o_pricat_code         = o_pricat_code
             o_series_code         = o_series_code
        ).

    ENDIF.
    IF ( ( article_found = abap_false ) AND ( o_article_code IS NOT INITIAL ) ). " found via API
        APPEND VALUE descr_table( code = o_article_code description = o_article_description ) TO it_article.
        SORT it_article STABLE BY code.
*        it_cash_create = VALUE #(
*            (
*                %cid = 'root'
*                %data = VALUE #(
*                    Type        = 'A'
*                    Code        = o_article_code
*                    Description = o_article_description
*                )
*            )
*        ).
*        MODIFY ENTITIES OF zi_cash_006
*            ENTITY Cash
*            CREATE FIELDS ( Type Code Description )
*            WITH it_cash_create
*            MAPPED DATA(mapped1)
*            FAILED DATA(failed1)
*            REPORTED DATA(reported1).
    ENDIF.
    IF ( ( color_found = abap_false ) AND ( o_color_code IS NOT INITIAL ) ). " found via API
        APPEND VALUE descr_table( code = o_color_code description = o_color_description ) TO it_color.
        SORT it_color STABLE BY code.
*        it_cash_create = VALUE #(
*            (
*                %cid = 'root'
*                %data = VALUE #(
*                    Type        = 'C'
*                    Code        = o_color_code
*                    Description = o_color_description
*                )
*            )
*        ).
*        MODIFY ENTITIES OF zi_cash_006
*            ENTITY Cash
*            CREATE FIELDS ( Type Code Description )
*            WITH it_cash_create
*            MAPPED DATA(mapped2)
*            FAILED DATA(failed2)
*            REPORTED DATA(reported2).
    ENDIF.
    IF ( ( pricat_found = abap_false ) AND ( o_pricat_code IS NOT INITIAL ) ). " found via API
        APPEND VALUE descr_table( code = o_pricat_code description = o_pricat_description ) TO it_pricat.
        SORT it_pricat STABLE BY code.
*        it_cash_create = VALUE #(
*            (
*                %cid = 'root'
*                %data = VALUE #(
*                    Type        = 'P'
*                    Code        = o_pricat_code
*                    Description = o_pricat_description
*                )
*            )
*        ).
*        MODIFY ENTITIES OF zi_cash_006
*            ENTITY Cash
*            CREATE FIELDS ( Type Code Description )
*            WITH it_cash_create
*            MAPPED DATA(mapped3)
*            FAILED DATA(failed3)
*            REPORTED DATA(reported3).
    ENDIF.
    IF ( ( series_found = abap_false ) AND ( o_series_code IS NOT INITIAL ) ). " found via API
        APPEND VALUE descr_table( code = o_series_code description = o_series_description ) TO it_series.
        SORT it_series STABLE BY code.
*        it_cash_create = VALUE #(
*            (
*                %cid = 'root'
*                %data = VALUE #(
*                    Type        = 'S'
*                    Code        = o_series_code
*                    Description = o_series_description
*                )
*            )
*        ).
*        MODIFY ENTITIES OF zi_cash_006
*            ENTITY Cash
*            CREATE FIELDS ( Type Code Description )
*            WITH it_cash_create
*            MAPPED DATA(mapped4)
*            FAILED DATA(failed4)
*            REPORTED DATA(reported4).
    ENDIF.

  ENDMETHOD. " get_custom_fields_opt_internal

ENDCLASS. " zbp_i_pricat_006 IMPLEMENTATION
