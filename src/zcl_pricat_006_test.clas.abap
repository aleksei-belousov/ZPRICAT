CLASS zcl_pricat_006_test DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.

*   read via custom business object
    METHODS read_via_cbo.

*   create via custom business object
    METHODS create_via_cbo
        RETURNING VALUE(o_uuid)         TYPE string.

*   get token via custom business object
    METHODS get_token_via_cbo
        RETURNING VALUE(o_token)        TYPE string.

*   update via custom business object
    METHODS update_via_cbo
        IMPORTING VALUE(i_color)        TYPE string.

*   Get Description for Color Code via custom business object
    METHODS get_description_via_cbo
        IMPORTING VALUE(i_color)        TYPE string
        RETURNING VALUE(o_color_name)   TYPE string.

*   read via service consumption model (commented)
    METHODS read_via_rap.

*   Get Description for Article, Color, Pricat, Series via custom business object
    METHODS get_custom_fields_internal
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

ENDCLASS.



CLASS ZCL_PRICAT_006_TEST IMPLEMENTATION.


  METHOD create_via_cbo. " create object via custom business object
  ENDMETHOD. " create_via_cbo


  METHOD get_custom_fields_internal.

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


  METHOD get_description_via_cbo.

    DATA system_url TYPE string.

    DATA i_username TYPE string VALUE 'INBOUND_USER'.
    DATA i_password TYPE string VALUE 'rtrVDDgelabtTjUiybRX}tVD3JksqqfvPpBdJRaL'.

    DATA text   TYPE string.
    DATA s1     TYPE string.
    DATA s2     TYPE string.
    DATA s3     TYPE string.

    TRY.

*  DATA(i_url) = 'https://my404898.s4hana.cloud.sap/sap/opu/odata/sap/YY1_CUSTOMFIELDS_CDS/YY1_CUSTOMFIELDS(guid'91bf6b38-1c0f-1ede-b2ca-79c4ed0310fc')'.
*  DATA(i_url) = 'https://my404898.s4hana.cloud.sap/sap/opu/odata/sap/YY1_CUSTOMFIELDS_CDS/YY1_CUSTOMFIELDS'.

        system_url = cl_abap_context_info=>get_system_url( ).

*       Read list of objects and get UUID of the first
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

        REPLACE '<d:SAP_UUID>'    IN text WITH '******'.
        REPLACE '</d:SAP_UUID>'   IN text WITH '******'.
        SPLIT text AT '******' INTO s1 s2 s3.

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

*        DATA token TYPE string.
        READ TABLE response_header_fields WITH KEY name = 'x-csrf-token' INTO DATA(field).
        IF ( sy-subrc = 0 ).
            DATA(token) = field-value.
        ENDIF.

*       Update Code:

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

        lo_http_request->set_text(
          EXPORTING
            i_text   = '{"CODE":"' && i_color && '"}' " '004'
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

*       Read Description

        lo_http_response = lo_http_client->execute(
            i_method   = if_web_http_client=>get
        ).

        text                    = lo_http_response->get_text( ).
        status                  = lo_http_response->get_status( ).
        response_header_fields  = lo_http_response->get_header_fields( ).

        REPLACE '<d:DESCRIPTION>'    IN text WITH '******'.
        REPLACE '</d:DESCRIPTION>'   IN text WITH '******'.
        SPLIT text AT '******' INTO s1 s2 s3.

        o_color_name = s2.

    CATCH cx_web_message_error.

    CATCH cx_abap_context_info_error INTO DATA(lx_abap_context_info_error).
      " Handle remote Exception
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

  ENDMETHOD. " get_description_via_cbo


  METHOD get_token_via_cbo. " get token via custom business object
    TRY.

*  DATA(i_url) = 'https://my404898.s4hana.cloud.sap/sap/opu/odata/sap/YY1_CUSTOMFIELDS_CDS/YY1_CUSTOMFIELDS(guid'91bf6b38-1c0f-1ede-b2b3-fbf4a512b0f3')'.
*  DATA(i_url) = 'https://my404898.s4hana.cloud.sap/sap/opu/odata/sap/YY1_CUSTOMFIELDS_CDS/YY1_CUSTOMFIELDS'.

        DATA(system_url) = cl_abap_context_info=>get_system_url( ).
        CONCATENATE
                'https://'
                system_url(8) " my404898
                '-api.s4hana.cloud.sap/sap/opu/odata/sap/YY1_CUSTOMFIELDS_CDS/YY1_CUSTOMFIELDS'
                '(guid''91bf6b38-1c0f-1ede-b2ca-79c4ed0310fc'')'
            INTO DATA(i_url).

        DATA i_username TYPE string VALUE 'INBOUND_USER'.
        DATA i_password TYPE string VALUE 'rtrVDDgelabtTjUiybRX}tVD3JksqqfvPpBdJRaL'.

        IF ( system_url(8) = 'my404907' ). " test
            i_username  = 'INBOUND_USER'.
            i_password  = 'rtrVDDgelabtTjUiybRX}tVD3JksqqfvPpBdJRaL'.
        ENDIF.
        IF ( system_url(8) = 'my410080' ). " prod
            i_username  = 'INBOUND_USER'.
            i_password  = 'YKXMYdjNnGgqko&aEueVx5mHTFPRGcDGAVgQgnFh'.
        ENDIF.

        DATA(http_destination) = cl_http_destination_provider=>create_by_url( i_url ).

        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( http_destination ).

        lo_http_client->get_http_request( )->set_authorization_basic(
            i_username = i_username
            i_password = i_password
        ).

        DATA(lo_http_request) = lo_http_client->get_http_request( ).

        lo_http_request->set_header_field(
            i_name  = 'x-csrf-token'
            i_value = 'fetch'
        ).

        DATA(lo_http_response) = lo_http_client->execute(
            i_method   = if_web_http_client=>get
        ).

        DATA(text)                   = lo_http_response->get_text( ).
        DATA(status)                 = lo_http_response->get_status( ).
        DATA(response_header_fields) = lo_http_response->get_header_fields( ).

        DATA token TYPE string.
        LOOP AT response_header_fields INTO DATA(response_header_field).
            IF ( response_header_field-name = 'x-csrf-token' ).
                token = response_header_field-value.
                EXIT.
            ENDIF.
        ENDLOOP.

        o_token  = token.

    CATCH cx_abap_context_info_error INTO DATA(lx_abap_context_info_error).
      " Handle remote Exception
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

  ENDMETHOD. " get token via custom busness object


  METHOD if_oo_adt_classrun~main.

    SELECT SINGLE * FROM i_product WHERE ( Product = '0000326-046-B-105' ) INTO @DATA(product).

*    DATA(description) = get_description_via_cbo( i_color = '003' ).
    get_custom_fields_internal(
      EXPORTING
         i_article_code        = '326'
         i_color_code          = '004'
         i_pricat_code         = '21'
         i_series_code         = '003'
      IMPORTING
         o_article_description = DATA(article_description)
         o_color_description   = DATA(color_description)
         o_pricat_description  = DATA(pricat_description)
         o_series_description  = DATA(series_description)
         o_article_code        = DATA(article_code)
         o_color_code          = DATA(color_code)
         o_pricat_code         = DATA(pricat_code)
         o_series_code         = DATA(series_code)
    ).

    out->write( '"' && article_description && '"' ).
    out->write( '"' && color_description && '"' ).
    out->write( '"' && pricat_description && '"' ).
    out->write( '"' && series_description && '"' ).

  ENDMETHOD.


  METHOD read_via_cbo. " get token via custom business object

    TRY.

*  DATA(i_url) = 'https://my404898.s4hana.cloud.sap/sap/opu/odata/sap/YY1_CUSTOMFIELDS_CDS/YY1_CUSTOMFIELDS(guid'91bf6b38-1c0f-1ede-b2b3-fbf4a512b0f3')'.
*  DATA(i_url) = 'https://my404898.s4hana.cloud.sap/sap/opu/odata/sap/YY1_CUSTOMFIELDS_CDS/YY1_CUSTOMFIELDS'.
*  DATA(i_url) = 'https://my404930.s4hana.cloud.sap/sap/opu/odata/sap/YY1_CUSTOMFIELDS_CDS/YY1_CUSTOMFIELDS(guid'91bf6b38-1c0f-1ede-b2ca-79c4ed0310fc')'.
*  DATA(i_url) = 'https://my404930.s4hana.cloud.sap/sap/opu/odata/sap/YY1_CUSTOMFIELDS_CDS/YY1_CUSTOMFIELDS'.

        DATA(system_url)    = cl_abap_context_info=>get_system_url( ).
        CONCATENATE
                'https://'
                system_url(8) " my404898
                '-api.s4hana.cloud.sap/sap/opu/odata/sap/YY1_CUSTOMFIELDS_CDS/YY1_CUSTOMFIELDS'
                '(guid''91bf6b38-1c0f-1ede-b2ca-79c4ed0310fc'')'
            INTO DATA(i_url).

        DATA i_username TYPE string VALUE 'INBOUND_USER'.
        DATA i_password TYPE string VALUE 'rtrVDDgelabtTjUiybRX}tVD3JksqqfvPpBdJRaL'.

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

        DATA(text) = lo_http_response->get_text( ).

        DATA(status) = lo_http_response->get_status( ).

        REPLACE '<d:DESCRIPTION>'    IN text WITH '******'.
        REPLACE '</d:DESCRIPTION>'   IN text WITH '******'.
        SPLIT text AT '******' INTO DATA(s1) DATA(s2) DATA(s3).

        DATA(description) = s2.

    CATCH cx_abap_context_info_error INTO DATA(lx_abap_context_info_error).
      " Handle remote Exception
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

  ENDMETHOD. " read_via_cbo


  METHOD read_via_rap.

*    DATA:
*      ls_entity_key    TYPE zzrap_yy1_colorset,
*      ls_business_data TYPE zzrap_yy1_colorset,
*      lo_http_client   TYPE REF TO if_web_http_client,
*      lo_resource      TYPE REF TO /iwbep/if_cp_resource_entity,
*      lo_client_proxy  TYPE REF TO /iwbep/if_cp_client_proxy,
*      lo_request       TYPE REF TO /iwbep/if_cp_request_read,
*      lo_response      TYPE REF TO /iwbep/if_cp_response_read.
*
*    TRY.
*
*     " Create http client
*        DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
*                                                             comm_scenario  = 'YY1_CUSTOMFIELDS'
*                                                             comm_system_id = 'TECH'
*                                                             service_id     = '<Service Id>' ).
**lo_http_client = cl_web_http_client_manager=>create_by_http_destination( lo_destination ).
*        lo_client_proxy = cl_web_odata_client_factory=>create_v2_remote_proxy(
*            EXPORTING
*                iv_service_definition_name = 'ZSC_PRICAT_006'
*                io_http_client             = lo_http_client
*                iv_relative_service_root   = '<service_root>' ).
*
*
*    ASSERT lo_http_client IS BOUND.
*
*" Set entity key
*    ls_entity_key = VALUE #( code  = 'Code' ).
*
*" Navigate to the resource
*    lo_resource = lo_client_proxy->create_resource_for_entity_set( 'YY1_COLORSET' )->navigate_with_key( ls_entity_key ).
*
*" Execute the request and retrieve the business data
*    lo_response = lo_resource->create_request_for_read( )->execute( ).
*    lo_response->get_business_data( IMPORTING es_business_data = ls_business_data ).
*
*    CATCH /iwbep/cx_cp_remote INTO DATA(lx_remote).
*    " Handle remote Exception
*    " It contains details about the problems of your http(s) connection
*
*    CATCH /iwbep/cx_gateway INTO DATA(lx_gateway).
*    " Handle Exception
*
*    CATCH cx_web_http_client_error INTO DATA(lx_web_http_client_error).
*    " Handle Exception
**    RAISE SHORTDUMP lx_web_http_client_error.
*
*      catch cx_http_dest_provider_error.
*        "handle exception
*    ENDTRY.

  ENDMETHOD. " via_rap


  METHOD update_via_cbo. " update via custom business object

    TRY.

*  DATA(i_url) = 'https://my404898.s4hana.cloud.sap/sap/opu/odata/sap/YY1_CUSTOMFIELDS_CDS/YY1_CUSTOMFIELDS(guid'91bf6b38-1c0f-1ede-b2ca-79c4ed0310fc')'.
*  DATA(i_url) = 'https://my404898.s4hana.cloud.sap/sap/opu/odata/sap/YY1_CUSTOMFIELDS_CDS/YY1_CUSTOMFIELDS'.

        DATA(system_url) = cl_abap_context_info=>get_system_url( ).
        CONCATENATE
                'https://'
                system_url(8) " my404898
                '-api.s4hana.cloud.sap/sap/opu/odata/sap/YY1_CUSTOMFIELDS_CDS/YY1_CUSTOMFIELDS'
                '(guid''91bf6b38-1c0f-1ede-b2ca-79c4ed0310fc'')'
            INTO DATA(i_url).

        DATA i_username TYPE string VALUE 'INBOUND_USER'.
        DATA i_password TYPE string VALUE 'rtrVDDgelabtTjUiybRX}tVD3JksqqfvPpBdJRaL'.

        IF ( system_url(8) = 'my404907' ). " test
            i_username  = 'INBOUND_USER'.
            i_password  = 'rtrVDDgelabtTjUiybRX}tVD3JksqqfvPpBdJRaL'.
        ENDIF.
        IF ( system_url(8) = 'my410080' ). " prod
            i_username  = 'INBOUND_USER'.
            i_password  = 'YKXMYdjNnGgqko&aEueVx5mHTFPRGcDGAVgQgnFh'.
        ENDIF.

        DATA(http_destination) = cl_http_destination_provider=>create_by_url( i_url ).

        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( http_destination ).

        lo_http_client->get_http_request( )->set_authorization_basic(
            i_username = i_username
            i_password = i_password
        ).

        DATA(lo_http_request) = lo_http_client->get_http_request( ).


*       Get Token:

        lo_http_request->set_header_field(
            i_name  = 'x-csrf-token'
            i_value = 'fetch'
        ).

        DATA(lo_http_response) = lo_http_client->execute(
            i_method   = if_web_http_client=>get
        ).

        DATA(text)                   = lo_http_response->get_text( ).
        DATA(status)                 = lo_http_response->get_status( ).
        DATA(response_header_fields) = lo_http_response->get_header_fields( ).

        READ TABLE response_header_fields WITH KEY name = 'x-csrf-token' INTO DATA(field).
        IF ( sy-subrc = 0 ).
            DATA(token) = field-value.
        ENDIF.

*       Update Code:

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

        lo_http_request->set_header_fields(
          EXPORTING
            i_fields = i_fields
*          RECEIVING
*            r_value  =
        ).

        lo_http_request->set_text(
            i_text   = '{"CODE":"' && i_color && '"}' " '004'
        ).

        lo_http_response = lo_http_client->execute(
            i_method   = if_web_http_client=>put
        ).

        text                      = lo_http_response->get_text( ).
        status                    = lo_http_response->get_status( ).
        response_header_fields    = lo_http_response->get_header_fields( ).

    CATCH cx_web_message_error.

    CATCH cx_abap_context_info_error INTO DATA(lx_abap_context_info_error).
      " Handle remote Exception
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

  ENDMETHOD. " update_via_cbo
ENDCLASS.
